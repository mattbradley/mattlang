module Mattlang
  module Types
    class Simple < Base
      attr_reader :type_atom

      def initialize(type_atom)
        @type_atom = type_atom
      end

      def subtype?(other)
        self == other
      end

      def ==(other)
        other.is_a?(Simple) && other.type_atom == type_atom
      end

      def concrete_types
        [self]
      end

      def to_s
        type_atom.to_s
      end
    end
  end
end
