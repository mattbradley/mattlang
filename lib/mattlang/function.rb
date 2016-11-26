module Mattlang
  class Function
    attr_reader :name, :args, :return_type, :body

    def initialize(name, args, return_type, body)
      @name = name
      @args = args
      @return_type = return_type
      @body = body
    end

    def arg_types
      @arg_types ||= args.map(&:last)
    end
  end
end
