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

    attr_reader :enclosing_scope, :modules, :infix_operators, :functions, :binding, :type_params
    attr_accessor :native_types

    def initialize(enclosing_scope = nil, module_name: nil)
      @enclosing_scope = enclosing_scope
      @infix_operators = {}
      @functions = Hash.new { |h, k| h[k] = [] }
      @binding = {}
      @native_types = {}
      @type_params = []
      @typealiases = {}
      @modules = {}

      @module_name = module_name
    end

    def fetch_module(name)
      @modules[name] ||= Scope.new(self, module_name: name)
    end

    def find_runtime_function(name, arg_types)
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
            if fn_type.anything?
              candidate_lambda_types << Types::Lambda.new([Types.nothing] * type[:arg_count], Types.anything)
              next true
            elsif fn_type.is_a?(Types::Lambda) && fn_type.args.size == type[:arg_count]
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
            fn_type.subtype?(type)
          end
        end

        if is_match
          types.zip(candidate_lambda_types).each do |type, candidate_type|
            if type.is_a?(Hash)
              if type_bindings
                type[:candidate_types] << candidate_type.replace_type_bindings(type_bindings.select { |k, v| !v.nil? })
              else
                type[:candidate_types] << candidate_type
              end
            end
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
    def resolve_function(name, arg_types, exclude_lambdas: false, infer_untyped_lambdas: false, force_scope: false)
      if !exclude_lambdas &&
        (binding = resolve_binding(name)) &&
        binding.is_a?(Types::Lambda) &&
        binding.args.size == arg_types.size &&
        arg_types.zip(binding.args).all? { |arg_type, lambda_arg| lambda_arg.subtype?(arg_type, nil, true) }
          return binding.return_type
      end

      first, *rest = arg_types.map { |arg_type| deconstruct_type(arg_type) }
      return_types = (first.nil? ? [[]] : first.product(*rest)).map do |types|
        found_fns = find_functions(name, types, all_matches: arg_types.any? { |t| t.is_a?(Hash) })

        if found_fns.any?
          found_fns.map do |compatible_fn, type_bindings|
            if type_bindings && !infer_untyped_lambdas
              compatible_fn.return_type.replace_type_bindings(type_bindings)
            else
              compatible_fn.return_type
            end
          end
        else
          if @enclosing_scope && !force_scope
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
      raise Error.new("The infix operator '#{operator}' has already been declared at this scope") if @infix_operators.key?(operator)
      @infix_operators[operator] = [associativity, precedence]
    end

    def define_function(name, args, return_type, body, type_params: nil)
      @functions[[name, args.size]] << Function.new(name, args, return_type, body, type_params: type_params)
    end

    def define_type_param(type_param)
      @type_params << type_param
    end

    def define_typealias(name, type, type_params = [])
      raise Error.new("The typealias '#{name}' has already been defined at this scope") if @typealiases.key?(name)

      @typealiases[name] = [type, type_params]
    end

    def define(name, type)
      @binding[name] = type
    end

    def resolve(name, force_scope: false)
      if (variable_type = resolve_binding(name))
        variable_type
      else
        begin
          resolve_function(name, [], force_scope: force_scope)
        rescue Error
          raise Error.new("Undefined function or local variable '#{name}'")
        end
      end
    end

    def resolve_binding(name)
      if @binding.key?(name)
        @binding[name]
      elsif @enclosing_scope
        @enclosing_scope.resolve_binding(name)
      else
        nil
      end
    end

    def resolve_module(name, force_scope: false)
      if (mod = @modules[name])
        mod
      elsif @enclosing_scope && !force_scope
        @enclosing_scope.resolve_module(name)
      else
        raise Error.new("Undefined module '#{name}'")
      end
    end

    def resolve_module_path(path)
      return self if path.nil? || path.empty?
      path = path.dup

      outermost_module_scope = resolve_module(path.shift)
      path.reduce(outermost_module_scope) { |scope, mod| scope.resolve_module(mod, force_scope: true) }
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

    def resolve_type(type, original_scope = self, ignore_module_path: false)
      case type
      when Types::Tuple
        Types::Tuple.new(type.types.map { |t| original_scope.resolve_type(t) })
      when Types::Record
        Types::Record.new(type.types_hash.map { |k, t| [k, original_scope.resolve_type(t)] }.to_h)
      when Types::Lambda
        Types::Lambda.new(type.args.map { |t| original_scope.resolve_type(t) }, original_scope.resolve_type(type.return_type))
      when Types::Union
        Types.combine(type.types.map { |t| original_scope.resolve_type(t) })
      else
        if !ignore_module_path && !type.module_path.empty?
          resolve_module_path(type.module_path).resolve_type(type, original_scope, ignore_module_path: true)
        elsif @type_params.include?(type.type_atom)
          raise Error.new("Type parameter '#{type.type_atom}' is not a generic type") if type.is_a?(Types::Generic)
          Types::Simple.new(type.type_atom, parameter_type: true)
        elsif (typealias = @typealiases[type.type_atom])
          aliased_type, aliased_type_params = typealias

          if type.is_a?(Types::Simple)
            raise Error.new("Generic type '#{type.type_atom}' is missing type parameters") if aliased_type_params.count != 0
            resolve_type(aliased_type)
          else
            raise Error.new("Generic type '#{type.type_atom}' requires #{aliased_type_params.count} type parameter#{'s' if aliased_type_params.count > 1}, but was given #{type.type_parameters.count}") if aliased_type_params.count != type.type_parameters.count
            resolved_type_parameters = type.type_parameters.map { |t| original_scope.resolve_type(t) }
            resolve_type(aliased_type.replace_type_bindings(aliased_type_params.zip(resolved_type_parameters).to_h))
          end
        elsif type.module_path.empty? && (native_type_params_count = @native_types[type.type_atom])
          if type.is_a?(Types::Simple)
            raise Error.new("Generic type '#{type.type_atom}' is missing type parameters") if native_type_params_count != 0
            type
          else
            raise Error.new("Generic type '#{type.type_atom}' requires #{native_type_params_count} type parameter#{'s' if native_type_params_count > 1}, but was given #{type.type_parameters.count}") if native_type_params_count != type.type_parameters.count
            Types::Generic.new(type.type_atom, type.type_parameters.map { |t| original_scope.resolve_type(t) })
          end
        elsif @enclosing_scope
            @enclosing_scope.resolve_type(type, original_scope)
        else
          type_str = type.is_a?(Types::Simple) ? type.to_s : Types::Simple.new(type.type_atom, module_path: type.module_path).to_s
          raise Error.new("Unknown type '#{type_str}'")
        end
      end
    end

    def bound_types
      (@enclosing_scope&.bound_types || {}).merge(@type_params.map { |t| [t, Types::Simple.new(t, parameter_type: true)] }.to_h)
    end

    def module_path
     (@enclosing_scope&.module_path || []) + (@module_name.nil? ? [] : [@module_name])
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
