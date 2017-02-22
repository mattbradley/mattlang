module Mattlang
  class Scope
    class Error < CompilerError
      def self.title; 'Scope Error' end

      attr_accessor :ast

      def initialize(message, ast = nil)
        @ast = ast
        super(message)
      end
    end

    class NoMatchingFunction < Error; end

    attr_reader :enclosing_scope, :modules, :infix_operators, :functions, :binding

    def initialize(enclosing_scope = nil)
      @enclosing_scope = enclosing_scope
      @infix_operators = {}
      @functions = Hash.new { |h, k| h[k] = [] }
      @binding = {}
      @type_params = []
      @modules = {}
    end

    def find_runtime_function(name, arg_types)
      fns = @functions[[name, arg_types.size]] || raise(Error.new("Unknown function '#{name}' with #{arg_types.empty? ? 'no args' : 'arg types (' + arg_types.join(', ') + ')'}"))
      arg_types.each { |t| raise Error.new("Union type '#{t}' argument for function '#{name}' cannot be used for dispatch at runtime") if t.is_a?(Types::Union) }

      runtime_fn, type_bindings = find_functions(name, arg_types).first
      if runtime_fn
        [runtime_fn, type_bindings]
      else
        if @enclosing_scope
          @enclosing_scope.find_runtime_function(name, arg_types)
        else
          raise Error.new("No runtime function clause matches '#{name}' with #{arg_types.empty? ? 'no args' : 'arg types (' + arg_types.join(', ') + ')'}")
        end
      end
    end

    def find_functions(name, types, all_matches: false)
      fns = @functions.key?([name, types.size]) && @functions[[name, types.size]] || []

      compatible_fns = fns.map do |fn|
        type_bindings = fn.generic? ? fn.type_params.map { |t| [t, nil] }.to_h : nil
        candidate_lambda_types = []

        is_match = fn.arg_types.zip(types).all? do |fn_type, type|
          if type.is_a?(Hash)
            if fn_type.is_a?(Types::Lambda) && fn_type.args.size == type[:arg_count]
              candidate_lambda_types << fn_type
              next true
            else
              next false
            end
          else
            candidate_lambda_types << nil
          end

          if fn.generic?
            local_type_bindings = fn.type_params.map { |t| [t, nil] }.to_h

            if fn_type.subtype?(type, local_type_bindings)
              local_type_bindings.all? do |type_param, bound_type|
                if bound_type.nil?
                  true
                elsif type_bindings[type_param].nil?
                  type_bindings[type_param] = bound_type
                  true
                else
                  if type_bindings[type_param].subtype?(bound_type, nil, true)
                    true
                  elsif bound_type.subtype?(type_bindings[type_param], nil, true)
                    type_bindings[type_param] = bound_type
                    true
                  else
                    false
                  end
                end
              end
            else
              false
            end
          else
            type.is_a?(Types::Simple) && type.parameter_type? || fn_type.subtype?(type)
          end
        end

        if is_match
          types.zip(candidate_lambda_types).each do |type, candidate_type|
            type[:candidate_types] << candidate_type.replace_type_bindings(type_bindings.select { |k, v| !v.nil? }) if type.is_a?(Hash)
          end

          if all_matches
            [fn, type_bindings]
          else
            return [[fn, type_bindings]]
          end
        end
      end.compact
    end

    # TODO: I think there is a bug here with resolving a generic function in the enclosing scope.
    # It might not bind the type parameters to the correct union type.
    #
    # For instance, this fails if `foo` is in the enclosing scope
    #
    # fn foo<T>(a: T, b: T) -> List<T>; [a, b] end
    #
    # x = if true; 5 else 5.0 end # Float | Int
    # y = if false; 6 else 6.0 end # Float | Int
    #
    # foo(x, y)
    # # Compilation error. Should this return List<Float | Int>? Or should it fail
    # since `x` could be Int and `y` could be Float, which would break the `T` constraint.
    # I'm leaning towards it returning List<Float | Int>, meaning `T` is bound to Float | Int.
    # I think that would be fine semantically, since `foo` can only send `a` and `b` to other
    # generic functions (and so on, recursively). And, since these generic functions can have
    # their type parameters bound to any type, it shouldn't matter what type is sent to them.
    # (This conclusion should hold until generic type contraints are implemented.)
    def resolve_function(name, arg_types, exclude_lambdas: false, infer_untyped_lambdas: false)
      if !exclude_lambdas &&
        (binding = resolve_binding(name)) &&
        binding.is_a?(Types::Lambda) &&
        binding.args.size == arg_types.size &&
        arg_types.zip(binding.args).all? { |arg_type, lambda_arg| lambda_arg.subtype?(arg_type, nil, true) }
          return binding.return_type
      end

      first, *rest = arg_types.map { |arg_type| deconstruct_type(arg_type) }
      return_types = (first.nil? ? [[]] : first.product(*rest)).map do |types|
        found_fns = find_functions(name, types, all_matches: arg_types.any? { |t| t.is_a?(Hash) || t.parameter_type? })

        if found_fns.any?
          found_fns.map do |compatible_fn, type_bindings|
            if type_bindings && !infer_untyped_lambdas
              compatible_fn.return_type.replace_type_bindings(type_bindings)
            else
              compatible_fn.return_type
            end
          end
        else
          if @enclosing_scope
            @enclosing_scope.resolve_function(name, types)
          else
            types_message =
              if types.empty?
                'no args'
              else
                'arg types (' + types.map do |type|
                  if type.is_a?(Hash)
                    if type[:arg_count] == 1
                      '? -> ?'
                    else
                      "(#{Array.new(type[:arg_count], '?').join(', ')}) -> ?"
                    end
                  else
                    type
                  end
                end.join(', ') + ')'
              end
            raise Error.new("No function clause matches '#{name}' with #{types_message}")
          end
        end
      end.flatten

      Types.combine(return_types)
    end

    def function_exists?(name, arg_count)
      @functions.key?([name, types.size]) || @enclosing_scope && @enclosing_scope.function_exists?(name, arg_count)
    end

    def define_infix_operator(operator, associativity, precedence)
      raise Error.new("The infix operator '#{operatorerator}' has already been declared at this scope") if @infix_operators.key?(operator)
      @infix_operators[operator] = [associativity, precedence]
    end

    def define_function(name, args, return_type, body, type_params: nil)
      @functions[[name, args.size]] << Function.new(name, args, return_type, body, type_params: type_params)
    end

    def define_type(type_param)
      @type_params << type_param
    end

    def define_module(name)
      @modules[name] ||= Scope.new(self)
    end

    def define(name, type)
      @binding[name] = Variable.new(name, type)
    end

    def resolve(name)
      if (variable_type = resolve_binding(name))
        variable_type
      else
        begin
          resolve_function(name, [])
        rescue StandardError
          raise Error.new("Undefined function or local variable '#{name}'")
        end
      end
    end

    def resolve_binding(name)
      if @binding.key?(name)
        @binding[name].type
      elsif @enclosing_scope
        @enclosing_scope.resolve_binding(name)
      else
        nil
      end
    end

    def resolve_module(name)
      if (mod = @modules[name])
        mod
      elsif @enclosing_scope
        @enclosing_scope.resolve_module(name)
      else
        raise Error.new("Undefined module '#{name}'")
      end
    end

    def resolve_infix_operator(name)
      if (op = @infix_operators[name])
        op
      elsif @enclosing_scope
        @enclosing_scope.resolve_infix_operator(name)
      else
        raise Error.new("Undefined infix operator '#{name}'")
      end
    end

    def type_exists?(type_atom)
      if @type_params.include?(type_atom)
        true
      elsif @enclosing_scope
        @enclosing_scope.type_exists?(type_atom)
      else
        false
      end
    end

    def bound_types
      (@enclosing_scope&.bound_types || {}).merge(@type_params.map { |t| [t, Types::Simple.new(t, parameter_type: true)] }.to_h)
    end

    private

    # Deconstructs union types into an array of simple or simple generic types
    def deconstruct_type(type)
      case type
      when Types::Union
        type.types.map { |t| deconstruct_type(t) }.flatten
      else
        [type]
      end
    end
  end
end
