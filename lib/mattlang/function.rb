module Mattlang
  class Function
    attr_reader :name, :args, :return_type, :body, :type_params

    def initialize(name, args, return_type, body, type_params: nil)
      @name = name
      @args = args
      @return_type = return_type
      @body = body
      @type_params = type_params
    end

    def arg_types
      @arg_types ||= args.map(&:last)
    end

    def generic?
      !type_params.nil?
    end
  end
end
