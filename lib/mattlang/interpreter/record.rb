module Mattlang
  class Interpreter
    class Record < Hash
      def inspect
        "{ #{sort.map { |k, v| "#{k}: #{v.value.inspect}" }.join(', ')} }"
      end
    end
  end
end
