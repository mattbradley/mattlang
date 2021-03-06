module Mattlang
  module Types
    # All type categories should inherit from this Base class.
    # All child classes should be immutable.
    class Base
      def nothing?
        false
      end

      def anything?
        false
      end

      def subtype?(other, type_bindings = nil, same_parameter_types = false)
        if anything?
          true
        elsif !(self.is_a?(Simple) && self.parameter_type?) && !same_parameter_types && other.respond_to?(:constraint) && !other.constraint.nil?
          subtype?(other.constraint, type_bindings, same_parameter_types)
        elsif other.is_a?(Union)
          other.types.all? { |t| subtype?(t, type_bindings, same_parameter_types) }
        elsif evaluate_subtype(other, type_bindings, same_parameter_types)
          true
        elsif other.is_a?(Intersection)
          ordered_types = other.types.sort_by { |t| t.parameter_type? ? 1 : 0 }
          ordered_types.any? { |t| subtype?(t, type_bindings, same_parameter_types) }
        elsif other.is_a?(Nominal) && !other.underlying_type.nil?
          subtype?(other.underlying_type, type_bindings, same_parameter_types)
        else
          other.nothing?
        end
      end

      # `true` if this type or any child types (for union or generics) are type parameters
      def parameter_type?
        raise NotImplementedError
      end

      def protocol_type?
        false
      end

      # Accepts a set of type bindings, such as { T: Int, U: List<String> },
      # and replaces all occurrences of simple types T and U with the bound types
      def replace_type_bindings(type_bindings)
        raise NotImplementedError
      end

      def ==(other)
        raise NotImplementedError
      end

      # Returns an array of all inner types in this type for which matcher returns true.
      def matching_types(&matcher)
        yield(self) ? [self] : []
      end

      # Deconstructs a union type into the array of inner types,
      # or just wraps this type in an array if it's not a union type.
      def deunion
        [self]
      end

      # Deconstructs an intersection type into the array of inner types,
      # or just wraps this type in an array if it's not an intersection type.
      def deintersect
        [self]
      end

      def deep_record_update(new_types)
        self
      end

      def to_s
        raise NotImplementedError
      end

      def eql?(other)
        self == other
      end

      def hash
        @hash ||= to_s.hash
      end

      def inspect
        to_s
      end

      private

      # `true` if `other` is the same type as or a subtype of `self`
      def evaluate_subtype?(other, type_bindings = nil, same_parameter_types = false)
        raise NotImplementedError
      end
    end
  end
end
