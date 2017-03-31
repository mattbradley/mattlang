module Mattlang
  class Interpreter
    class Tuple < Array
      def inspect
        "(#{map(&:value).map(&:inspect).join(', ')})"
      end

      def repl_format(value_proc, type_proc)
        "(#{map { |v| v.repl_format(value_proc, type_proc).first }.join(', ')})"
      end
    end
  end
end
