module Mattlang
  class Interpreter
    class Lambda
      attr_reader :args, :frame, :body

      def initialize(args, frame, body)
        @args = args
        @frame = frame
        @body = body
      end
    end
  end
end
