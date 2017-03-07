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

      # `true` if `other` is the same type as or a subtype of `self`
      def subtype?(other, type_bindings = nil, same_parameter_types = false)
        raise NotImplementedError
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
    end
  end
end
