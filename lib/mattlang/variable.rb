module Mattlang
  class Variable
    attr_reader :name, :type

    def initialize(name, type)
      @name = name

      if type.is_a?(Array)
        @type = type.flatten.uniq.sort
        @type = type.first if type.size == 1
      else
        @type = type
      end
    end
  end
end
