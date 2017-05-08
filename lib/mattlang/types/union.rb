module Mattlang
  module Types
    class Union < Base
      attr_reader :types

      def initialize(types)
        @types = types.uniq
        raise "A union type cannot be composed of other union types" if @types.any? { |t| t.is_a?(Union) }
        raise "All types in a union must inherit from Types::Base" if !@types.all? { |t| t.is_a?(Types::Base) }
      end

      def evaluate_subtype(other, type_bindings = nil, same_parameter_types = false)
        ordered_types = types.sort_by { |t| t.parameter_type? ? 1 : 0 }
        ordered_types.any? { |t| t.subtype?(other, type_bindings, same_parameter_types) }
      end

      def parameter_type?
        types.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        Types.union(types.map { |t| t.replace_type_bindings(type_bindings) })
      end

      def ==(other)
        other.is_a?(Union) && types.all? { |t| other.types.include?(t) }
      end

      def matching_types(&matcher)
        super(&matcher) + @types.map do |inner_type|
          inner_type.matching_types(&matcher)
        end.flatten.uniq
      end

      def deunion
        @types
      end

      def deep_record_update(new_types, scope)
        Types.union(@types.map { |t| t.deep_record_update(new_types, scope) })
      end

      def ground_types
        @types.flat_map(&:ground_types).uniq
      end

      def to_s
        types.map do |type|
          type.is_a?(Lambda) | type.is_a?(Intersection) ? "(#{type})" : type.to_s
        end.sort.join(' | ')
      end
    end
  end
end
