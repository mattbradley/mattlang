module Mattlang
  class Parser
    attr_reader :current_token

    def initialize(source)
      @lexer = Lexer.new(source)
    end

    def next_token
      @lexer.next_token
    end
  end
end
