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
        "#{deep_value.inspect} : #{type}"
      end

      def repl_format(value_proc, type_proc)
        if @type.is_a?(Types::Nominal)
          ["#{type_proc.(@type.type_atom.to_s)} #{@value.repl_format(value_proc, type_proc).first}", type_proc.(@type.to_s)]
        elsif @value.respond_to?(:repl_format)
          [@value.repl_format(value_proc, type_proc), type_proc.(@type.to_s)]
        else
          [value_proc.(@value.inspect), type_proc.(@type.to_s)]
        end
      end

      # Used so that Value objects can be used in embeds as `@a + @b` instead of `@a.value + @b.value`
      def method_missing(method, *args)
        args = args.map { |a| a.is_a?(Value) ? a.value : a }
        value.send(method, *args)
      end

      def coerce(other)
        [other, value]
      end

      def deep_value
        @value.is_a?(Value) ? value.deep_value : value
      end
    end
  end
end
