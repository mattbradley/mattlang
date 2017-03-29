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
        elsif evaluate_subtype(other, type_bindings, same_parameter_types)
          true
        elsif other.is_a?(Nominal)
          subtype?(other.underlying_type, type_bindings, same_parameter_types)
        else
          other.nothing?
        end
      end

      # `true` if this type or any child types (for union or generics) are type parameters
      def parameter_type?
        raise NotImplementedError
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
      # If `all_must_match` is true, then all concrete types (found by visiting
      # union and typedef type trees) must match, otherwise no types are returned.
      def matching_types(all_must_match:, &matcher)
        yield(self) ? [self] : []
      end

      def to_s
        raise NotImplementedError
      end

      def eql?(other)
        self == other
      end

      def hash
        to_s.hash
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
