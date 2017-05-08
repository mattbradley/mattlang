module Mattlang
  module Types
    class Intersection < Base
      attr_reader :types

      def initialize(types)
        @types = types.uniq
        raise "An intersection type cannot be composed of other intersection types" if @types.any? { |t| t.is_a?(Intersection) }
        raise "All types in an intersection must inherit from Types::Base" if !@types.all? { |t| t.is_a?(Types::Base) }
      end

      def evaluate_subtype(other, type_bindings = nil, same_parameter_types = false)
        types.all? { |t| t.subtype?(other, type_bindings, same_parameter_types) }
      end

      def parameter_type?
        types.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        Types.intersect(types.map { |t| t.replace_type_bindings(type_bindings) })
      end

      def ==(other)
        other.is_a?(Intersection) && types.all? { |t| other.types.include?(t) }
      end

      def deintersect
        @types
      end

      def deep_record_update(new_types, scope)
        Types.intersect(@types.map { |t| t.deep_record_update(new_types, scope) })
      end

      def ground_types
        @types.flat_map(&:ground_types).uniq
      end

      def to_s
        types.map do |type|
          type.is_a?(Lambda) || type.is_a?(Union) ? "(#{type})" : type.to_s
        end.sort.join(' & ')
      end
    end
  end
end
