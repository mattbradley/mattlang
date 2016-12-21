module Mattlang
  module Types
    class Base
      def ==(other)
        raise NotImplementedError
      end

      def type_atoms
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
