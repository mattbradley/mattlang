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

    def resolve_function(name, arg_types)
      fns = @functions[[name, arg_types.size]]

      first, *rest = arg_types.map { |arg_type| deconstruct_type(arg_type) }
      return_types = first.product(*rest).map do |types|
        type_bindings = nil

        compatible_fn = fns.find do |fn|
          type_bindings = fn.generic? ? fn.type_params.map { |t| [t, nil] }.to_h : nil

          fn.arg_types.zip(types).all? do |fn_type, type|
            r = if fn.generic?
              local_type_bindings = fn.type_params.map { |t| [t, nil] }.to_h

              if fn_type.subtype?(type, local_type_bindings)
                local_type_bindings.all? do |type_param, bound_type|
                  if bound_type.nil?
                    true
                  elsif type_bindings[type_param].nil?
                    type_bindings[type_param] = bound_type
                    true
                  else
                    if type_bindings[type_param].subtype?(bound_type)
                      true
                    elsif bound_type.subtype?(type_bindings[type_param])
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
            #puts "#{r} after #{fn_type}, #{type}: #{type_bindings.inspect}"
            r
          end
        end

        if compatible_fn
          if type_bindings
            #puts "*** Type bindings: #{type_bindings.inspect}"
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
    #   `String` => `[String]`
    #   `Int | Float` => `[Int, Float]`
    #   `List<Int | Float> | Nil` => `[List<Int>, List<Float>, Nil]`
    #   `Dict<String | Int, Float | Nil>` => `[Dict<String, Float>, Dict<String, Nil>, Dict<Int, Float>, Dict<Int, Nil>]`
    def deconstruct_type(type)
      case type
#      when Types::Simple
#        [type]
      when Types::Union
        type.types.map { |t| deconstruct_type(t) }.flatten
#      when Types::Generic
#        first, *rest = type.type_parameters.map { |type_parameter| deconstruct_type(type_parameter) }
#        first.product(*rest).map { |deconstructed_params| Types::Generic.new(type.type_atom, deconstructed_params) }
      else
        [type]
      end
    end
  end
end
