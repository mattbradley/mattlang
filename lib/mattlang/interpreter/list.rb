module Mattlang
  class Interpreter
    class List < Array
      def inspect
        map(&:value).inspect
      end
    end
  end
end
