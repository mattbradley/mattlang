module Mattlang
  module Types
    class Base
      # `true` if `other` is the same type as or a subtype of `self`
      def subtype?(other)
        raise NotImplementedError
      end

      def ==(other)
        raise NotImplementedError
      end

      def eql?(other)
        self == other
      end

      def hash
        to_s.hash
      end

      # Basically types that aren't union types. These types must
      # implement a `type_atom` method.
      def concrete_types
        raise NotImplementedError
      end

      def to_s
        raise NotImplementedError
      end

      def inspect
        to_s
      end
    end
  end
end
