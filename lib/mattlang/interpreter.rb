module Mattlang
  class Interpreter
    attr_reader :semantic

    def initialize(semantic)
      @semantic = semantic
      @frames = []
    end

    def interpret
      @frames << Frame.new


    end
  end
end
