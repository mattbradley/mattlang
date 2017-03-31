module Mattlang
  class Interpreter
    class List < Array
      def inspect
        map(&:value).inspect
      end

      def repl_format(value_proc, type_proc)
        "[#{map { |v| v.repl_format(value_proc, type_proc).first }.join(', ')}]"
      end
    end
  end
end
