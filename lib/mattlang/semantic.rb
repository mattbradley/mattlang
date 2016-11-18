module Mattlang
  class Semantic
    attr_reader :ast, :operators, :functions

    def self.debug(source)
      ast = Parser.new(source).parse
      semantic = new(ast)
      semantic.analyze
      puts semantic.ast.inspect
      puts semantic.operators
      puts semantic.functions
    end

    def initialize(ast)
      @ast = ast
      @operators = {}
      @functions = {}
    end
    
    def analyze
      populate_symbols
      @ast = rewrite_exprs(@ast)
    end

    private

    def populate_symbols
      raise "Unexpected node '#{@ast.term}'; expected top-level AST node" if @ast.term != :__top__

      @ast.children.each do |ast|
        if ast.term == :__infix__
          operator, associativity, precedence = ast.children.map(&:term)

          raise "The infix operator '#{operator}' has already been declared" if @operators.key?(operator)

          @operators[operator] = [associativity, precedence]
        elsif ast.term == :__fn__
          signature, body = ast.children
          name = signature.term
          args = signature.children.first.children.map { |arg| [arg.term, arg.children.first.term] }
          return_type = signature.children.last.term

          key = [name, args.map(&:last), return_type]
          raise "The function '#{name}' with type '(#{key[1].join(', ')}) -> #{return_type}' has already been defined" if @functions.key?(key)

          @functions[key] = [args, return_type, body]
        end
      end
    end

    def rewrite_exprs(ast)
      if ast.term == :__expr__
        precedence_climb(ast.children)
      else
        ast.children = ast.children.map { |c| rewrite_exprs(c) } unless ast.children.nil?
        ast
      end
    end

    def precedence_climb(expr_atoms, min_precedence = 0)
      lhs = expr_atoms.shift
      raise "Unexpected empty expr atom" if lhs.nil?

      lhs = precedence_climb(lhs.children) if lhs.term == :__expr__

      loop do
        return lhs if expr_atoms.empty?

        operator = expr_atoms.first

        raise "Unknown operator '#{operator.term}'" unless @operators.key?(operator.term)
        associativity, precedence = @operators[operator.term]

        return lhs if precedence < min_precedence

        expr_atoms.shift

        next_precedence = associativity == :left ? precedence + 1 : precedence
        rhs = precedence_climb(expr_atoms, next_precedence)
        lhs = AST.new(operator.term, [lhs, rhs])
      end
    end
  end
end
