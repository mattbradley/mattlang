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

        if other.is_a?(Union)
          other.types.all? do |o|
            if type_bindings
              local_type_bindings = type_bindings.keys.map { |t| [t, nil] }.to_h

              ordered_types.any? do |t|
                t.subtype?(o, local_type_bindings, same_parameter_types)
              end || (next false)

              local_type_bindings.select { |t, b| !b.nil? }.each do |type_parameter, bound_type|
                if type_bindings[type_parameter]
                  type_bindings[type_parameter] = Types.combine([type_bindings[type_parameter], bound_type])
                else
                  type_bindings[type_parameter] = bound_type
                end
              end

              true
            else
              ordered_types.any? do |t|
                t.subtype?(o, nil, same_parameter_types)
              end
            end
          end
        else
          ordered_types.any? { |t| t.subtype?(other, type_bindings, same_parameter_types) }
        end
      end

      def parameter_type?
        types.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        Types.combine(types.map { |t| t.replace_type_bindings(type_bindings) })
      end

      def ==(other)
        other.is_a?(Union) && types.all? { |t| other.types.include?(t) }
      end

      def matching_types(&matcher)
        @types.map do |inner_type|
          inner_type.matching_types(&matcher)
        end.flatten.uniq
      end

      def to_s
        types.map do |type|
          type.is_a?(Lambda) ? "(#{type})" : type.to_s
        end.sort.join(' | ')
      end
    end
  end
end
