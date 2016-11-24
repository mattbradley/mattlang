module Mattlang
  class Scope
    attr_reader :enclosing_scope, :functions, :binding

    def initialize(enclosing_scope = nil)
      @enclosing_scope = enclosing_scope
      @functions = {}
      @binding = {}
    end

    def resolve_function(name, arg_types)
      key = [name, arg_types]
      
      if @functions.key?(key)
        @functions[key]
      else
        if !@enclosing_scope.nil?
          @enclosing_scope.resolve_function(name, arg_types)
        else
          raise "Undefined function '#{name}' with #{arg_types.empty? ? 'no args' : 'arg types (' + arg_types.join(', ') + ')'}"
        end
      end
    end

    def define_function(name, args, return_type, body)
      function = Function.new(name, args, return_type, body)
      raise "The function '#{name}' with #{function.arg_types.empty? ? 'no args' : 'arg types (' + function.arg_types.join(', ') + ')'} has already been defined" if @functions.key?(function.key)

      @functions[function.key] = function
    end

    def define(name, type)
      @binding[name] = Variable.new(name, type)
    end

    def resolve(name)
      if @binding.key?(name)
        @binding[name]
      else
        function_key = [name, []]

        if @functions.key?(function_key)
          @functions[function_key]
        else
          if !@enclosing_scope.nil?
            @enclosing_scope.resolve(name)
          else
            raise "Undefined function or local variable '#{name}'"
          end
        end
      end
    end
  end
end
