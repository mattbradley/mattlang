module Mattlang
  module Types
    class Union < Base
      attr_reader :types

      def initialize(types)
        @types = types.uniq
        raise "A union type cannot be composed of other union types" if @types.any? { |t| t.is_a?(Union) }
      end

      def subtype?(other, type_bindings = nil)
        if other.is_a?(Union)
          other.types.all? { |o| types.any? { |t| t.subtype?(o, type_bindings) } }
        else
          types.any? { |t| t.subtype?(other, type_bindings) }
        end
      end

      def replace_type_bindings(type_bindings)
        Types.combine(types.map { |t| t.replace_type_bindings(type_bindings) })
      end

      def ==(other)
        other.is_a?(Union) && types.all? { |t| other.types.include?(t) }
      end

      def concrete_types
        types.map(&:concrete_types).flatten.uniq
      end

      def to_s
        types.map(&:to_s).sort.join(' | ')
      end
    end
  end
end
