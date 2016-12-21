module Mattlang
  module Types
    class Generic < Base
      attr_reader :type_atom
      attr_reader :type_parameters

      def initialize(type_atom, type_parameters)
        @type_atom = type_atom
        @type_parameters = type_parameters
      end

      def ==(other)
        other.is_a?(Generic) && other.type_atom == type_atom && other.type_parameters == type_parameters
      end

      def type_atoms
        ([type_atom] + type_parameters.map(&:type_atoms)).flatten.uniq.sort
      end

      def to_s
        "#{type_atom.to_s}<#{type_parameters.map(&:to_s).join(', ')}>"
      end
    end
  end
end
