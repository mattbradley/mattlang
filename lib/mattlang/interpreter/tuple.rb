module Mattlang
  class Interpreter
    class Tuple < Array
      def inspect
        "(#{map(&:value).map(&:inspect).join(', ')})"
      end
    end
  end
end
