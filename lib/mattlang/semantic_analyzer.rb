module Mattlang
  class SemanticAnalyzer
    attr_accessor :ast

    def initialize(ast)
      @ast = ast
    end
  end
end
