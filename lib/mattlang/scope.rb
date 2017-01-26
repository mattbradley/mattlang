module Mattlang
  class Scope
    attr_reader :enclosing_scope, :functions, :binding

    def initialize(enclosing_scope = nil)
      @enclosing_scope = enclosing_scope
      @functions = Hash.new { |h, k| h[k] = [] }
      @binding = {}
      @type_params = []
    end

    def find_runtime_function(name, arg_types)
      fns = @functions[[name, arg_types.size]] || raise("Unknown function '#{name}' with #{arg_types.empty? ? 'no args' : 'arg types (' + arg_types.join(', ') + ')'}")
      arg_types.each { |t| raise "Union type '#{t}' argument for function '#{name}' cannot be used for dispatch at runtime" if t.is_a?(Types::Union) }

      runtime_fn = fns.find do |fn|
        fn.arg_types.zip(arg_types).all? { |fn_type, type| fn_type.subtype?(type) }
      end

      runtime_fn || raise("No function clause matches '#{name}' with #{arg_types.empty? ? 'no args' : 'arg types (' + arg_types.join(', ') + ')'}")
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
    def resolve_function(name, arg_types)
      if (binding = resolve_binding(name)) &&
        binding.is_a?(Types::Lambda) &&
        binding.args.size == arg_types.size &&
        arg_types.zip(binding.args).all? { |arg_type, lambda_arg| arg_type.subtype?(lambda_arg) }
          return binding.return_type
      end

      fns = @functions.key?([name, arg_types.size]) && @functions[[name, arg_types.size]] || []

      first, *rest = arg_types.map { |arg_type| deconstruct_type(arg_type) }
      return_types = (first.nil? ? [[]] : first.product(*rest)).map do |types|
        type_bindings = nil

        compatible_fn = fns.find do |fn|
          type_bindings = fn.generic? ? fn.type_params.map { |t| [t, nil] }.to_h : nil

          fn.arg_types.zip(types).all? do |fn_type, type|
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
        end

        if compatible_fn
          if type_bindings
            compatible_fn.return_type.replace_type_bindings(type_bindings)
          else
            compatible_fn.return_type
          end
        else
          if @enclosing_scope
            @enclosing_scope.resolve_function(name, types)
          else
            raise "No function clause matches '#{name}' with #{types.empty? ? 'no args' : 'arg types (' + types.join(', ') + ')'}"
          end
        end
      end

      Types.combine(return_types)
    end

    def define_function(name, args, return_type, body, type_params: nil)
      @functions[[name, args.size]] << Function.new(name, args, return_type, body, type_params: type_params)
    end

    def define(name, type)
      @binding[name] = Variable.new(name, type)
    end

    def define_type(type_param)
      @type_params << type_param
    end

    def resolve(name)
      if (variable_type = resolve_binding(name))
        variable_type
      else
        begin
          resolve_function(name, [])
        rescue StandardError
          raise "Undefined function or local variable '#{name}'"
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

    def type_exists(type_atom)
      if @type_params.include?(type_atom)
        true
      elsif @enclosing_scope
        @enclosing_scope.type_exists(type_atom)
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
