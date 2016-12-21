module Mattlang
  module Types
    class Simple < Base
      attr_reader :type_atom

      def initialize(type_atom)
        @type_atom = type_atom
      end

      def ==(other)
        other.is_a?(Simple) && other.type_atom == type_atom
      end

      def type_atoms
        [type_atom]
      end

      def to_s
        type_atom.to_s
      end
    end
  end
end
