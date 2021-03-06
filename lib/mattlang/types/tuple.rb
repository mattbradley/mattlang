module Mattlang
  module Types
    class Tuple < Base
      attr_reader :types

      def initialize(types)
        @types = types

        raise "All types in a tuple must inherit from Types::Base" if !@types.all? { |t| t.is_a?(Types::Base) }
      end

      def evaluate_subtype(other, type_bindings = nil, same_parameter_types = false)
        other.is_a?(Tuple) &&
          types.size == other.types.size &&
          types.zip(other.types).all? { |t1, t2| t1.subtype?(t2, type_bindings, same_parameter_types) }
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

      def to_s
        "(#{types.map(&:to_s).join(', ')})"
      end
    end
  end
end
