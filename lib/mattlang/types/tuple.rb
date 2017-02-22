module Mattlang
  module Types
    class Tuple < Base
      attr_reader :types

      def initialize(types)
        @types = types

        raise "All types in a tuple must inherit from Types::Base" if !@types.all? { |t| t.is_a?(Types::Base) }
      end

      def subtype?(other, type_bindings = nil, same_parameter_types = false)
        if other.is_a?(Tuple)
          types.size == other.types.size &&
          types.zip(other.types).all? { |t1, t2| t1.subtype?(t2, type_bindings, same_parameter_types) }
        elsif other.is_a?(Union)
          other.types.all? { |t| self.subtype?(t, type_bindings, same_parameter_types) }
        else
          false
        end
      end

      def parameter_type?
        types.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        Tuple.new(types.map { |t| t.replace_type_bindings(type_bindings) })
      end

      def ==(other)
        other.is_a?(Tuple) && other.types == types
      end

      def concrete_types
        types.map(&:concrete_types).flatten.uniq
      end

      def to_s
        "(#{types.map(&:to_s).join(', ')})"
      end
    end
  end
end
