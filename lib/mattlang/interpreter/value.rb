module Mattlang
  class Interpreter
    class Value
      attr_reader :value, :type

      def initialize(value, type)
        @value = value
        @type = type

        raise "Interpreter values cannot be union types" if @type.is_a?(Types::Union)
      end

      def inspect
        "#{value.inspect} : #{type}"
      end

      def replace_type_bindings(type_bindings)
        @type = @type.replace_type_bindings(type_bindings)
        self
      end

      # Used so that Value objects can be used in embeds as `@a + @b` instead of `@a.value + @b.value`
      def method_missing(method, *args)
        args = args.map { |a| a.is_a?(Value) ? a.value : a }
        value.send(method, *args)
      end

      def coerce(other)
        [other, value]
      end
    end
  end
end
