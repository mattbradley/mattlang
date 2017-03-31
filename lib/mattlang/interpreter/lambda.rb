module Mattlang
  class Interpreter
    class Lambda
      attr_reader :args, :type, :frame, :scope, :context, :body

      def initialize(args, type, frame, scope, context, body)
        @args = args
        @type = type
        @frame = frame
        @scope = scope
        @context = context
        @body = body
      end

      def inspect
        "{ (#{args.zip(type.args).map { |a, t| "#{a}: #{t}" }.join(', ')}) -> ... }"
      end
    end
  end
end
