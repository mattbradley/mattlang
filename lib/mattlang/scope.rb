module Mattlang
  class Scope
    attr_reader :enclosing_scope, :functions, :binding

    def initialize(enclosing_scope = nil)
      @enclosing_scope = enclosing_scope
      @functions = {}
      @binding = {}
    end

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

    def define_function(name, args, return_type, body)
      function = Function.new(name, args, return_type, body)

      deconstruct_arg_types(args.map(&:last)).each do |arg_types|
        key = [name, arg_types]

        raise "A function clause matching '#{name}' with #{function.arg_types.empty? ? 'no args' : 'arg types (' + args_str(function.arg_types) + ')'} has already been defined" if @functions.key?(key)
        @functions[key] = function
      end
    end

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

    def args_str(args)
      args.map { |arg| arg.is_a?(Array) ? arg.join(' | ') : arg.to_s }.join(', ')
    end

    def deconstruct_arg_types(arg_types)
      return [[]] if arg_types.empty?

      first_arg, *rest_args = *arg_types.map { |t| [*t] }
      first_arg.product(*rest_args)
    end
  end
end
