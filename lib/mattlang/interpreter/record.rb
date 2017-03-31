module Mattlang
  class Interpreter
    class Record < Hash
      def inspect
        "{ #{sort.map { |k, v| "#{k}: #{v.value.inspect}" }.join(', ')} }"
      end

      def repl_format(value_proc, type_proc)
        "{#{sort.map { |k, v| k.to_s + ': ' + v.repl_format(value_proc, type_proc).first }.join(', ')}}"
      end
    end
  end
end
