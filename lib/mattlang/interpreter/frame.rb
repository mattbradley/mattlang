module Mattlang
  class Interpreter
    class Frame
      attr_reader :variables, :parent_frame

      def initialize(parent_frame = nil)
        @variables = {}
        @parent_frame = parent_frame
      end

      def [](name)
        if @variables.key?(name)
          @variables[name]
        elsif @parent_frame
          @parent_frame[name]
        end
      end

      def []=(name, value)
        @variables[name] = value
      end

      def all
        (@parent_frame&.all || {}).merge(@variables)
      end

      def dup
        f = Frame.new(parent_frame&.dup)
        f.instance_variable_set(:@variables, @variables.dup)
        f
      end

      def method_missing(method, *args, &block)
        all.send(method, *args, &block)
      end
    end
  end
end
