module Mattlang
  class Scope
    attr_reader :enclosing_scope, :functions, :binding

    def initialize(enclosing_scope = nil)
      @enclosing_scope = enclosing_scope
      @functions = Hash.new { |h, k| h[k] = [] }
      @binding = {}
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
        compatible_fn = fns.find do |fn|
          fn.arg_types.zip(types).all? { |fn_type, type| fn_type.subtype?(type) }
        end

        if compatible_fn
          compatible_fn.return_type
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

    def define_function(name, args, return_type, body)
      @functions[[name, args.size]] << Function.new(name, args, return_type, body)
    end

=begin
    def resolve_function(name, arg_types)
      deconstruct_arg_types(arg_types).map do |arg_types|
        key = [name, arg_types]

        if @functions.key?(key)
          @functions[key].return_type
        else
          if !@enclosing_scope.nil?
            @enclosing_scope.resolve_function(name, arg_types)
          else
            raise "No function clause matches '#{name}' with #{arg_types.empty? ? 'no args' : 'arg types (' + args_str(arg_types) + ')'}"
          end
        end
      end.flatten
    end
=end

=begin
    def define_function(name, args, return_type, body)
      function = Function.new(name, args, return_type, body)

      deconstruct_arg_types(args.map(&:last)).each do |arg_types|
        key = [name, arg_types]

        raise "A function clause matching '#{name}' with #{function.arg_types.empty? ? 'no args' : 'arg types (' + function.arg_types.join(', ') + ')'} has already been defined" if @functions.key?(key)
        @functions[key] = function
      end
    end
=end

    def define(name, type)
      @binding[name] = Variable.new(name, type)
    end

    def resolve(name)
      if @binding.key?(name)
        @binding[name].type
      else
        function_key = [name, []]

        if @functions.key?(function_key)
          @functions[function_key].return_type
        else
          if !@enclosing_scope.nil?
            @enclosing_scope.resolve(name)
          else
            raise "Undefined function or local variable '#{name}'"
          end
        end
      end
    end

    private

    # Deconstructs union types into an array of simple or simple generic types
    #   `String` => `[String]`
    #   `Int | Float` => `[Int, Float]`
    #   `List<Int | Float> | Nil` => `[List<Int>, List<Float>, Nil]`
    #   `Dict<String | Int, Float | Nil>` => `[Dict<String, Float>, Dict<String, Nil>, Dict<Int, Float>, Dict<Int, Nil>]`
    def deconstruct_type(type)
      case type
      when Types::Simple
        [type]
      when Types::Union
        type.types.map { |t| deconstruct_type(t) }.flatten
      when Types::Generic
        first, *rest = type.type_parameters.map { |type_parameter| deconstruct_type(type_parameter) }
        first.product(*rest).map { |deconstructed_params| Types::Generic.new(type.type_atom, deconstructed_params) }
      end
    end

=begin
    def deconstruct_arg_types(arg_types)
      return [[]] if arg_types.empty?

      first_arg, *rest_args = *arg_types.map { |t| [*t] }
      first_arg.product(*rest_args)
    end
=end
  end
end
