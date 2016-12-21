module Mattlang
  module Types
    class Union < Base
      attr_reader :types

      def initialize(types)
        @types = types.uniq
      end

      def ==(other)
        other.is_a?(Union) && types.all? { |t| other.types.include?(t) }
      end

      def type_atoms
        types.map(&:type_atoms).flatten.uniq.sort
      end

      def to_s
        types.map(&:to_s).join(' | ')
      end
    end
  end
end
