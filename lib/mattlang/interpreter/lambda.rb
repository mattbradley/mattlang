module Mattlang
  class Interpreter
    class Lambda
      attr_reader :args, :type, :frame, :body

      def initialize(args, type, frame, body)
        @args = args
        @type = type
        @frame = frame
        @body = body
      end

      def inspect
        "{ (#{args.zip(type.args).map { |a, t| "#{a}: #{t}" }.join(', ')}) -> ... }"
      end
    end
  end
end
