module Mattlang
  class Semantic
    class Function
      attr_reader :name, :args, :return_type, :body, :type_params
      attr_accessor :mangled_name

      def initialize(name, args, return_type, body, foreign: false, type_params: nil)
        @name = name
        @args = args
        @return_type = return_type
        @body = body
        @type_params = type_params
        @foreign = foreign
      end

      def foreign?
        @foreign == true
      end

      def arg_types
        @arg_types ||= args.map(&:type)
      end

      def generic?
        type_params && !type_params.empty?
      end
    end
  end
end
