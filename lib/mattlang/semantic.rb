module Mattlang
  class Semantic
    attr_reader :ast, :infix_operators, :functions

    def self.debug(source)
      ast = Parser.new(source).parse
      semantic = new(ast)
      semantic.analyze
      puts semantic.ast.inspect
      puts "Infix Ops: " + semantic.infix_operators.inspect
      puts "Functions: " + semantic.functions.inspect
    end

    def initialize(ast)
      @ast = ast
      @infix_operators = {}
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

          raise "The infix operator '#{operator}' has already been declared" if @infix_operators.key?(operator)

          @infix_operators[operator] = [associativity, precedence]
        end
      end

      @ast.children.each do |ast|
        if ast.term == :__fn__
          signature, body = ast.children
          name = signature.term
          args = signature.children.first.children.map { |arg| [arg.term, arg.children.first.term] }
          return_type = signature.children.last.term

          if signature.meta && signature.meta[:operator]
            if args.count == 2
              raise "Unknown infix operator '#{name}'" unless @infix_operators.key?(name)
            elsif args.count != 1
              raise "The operator function '#{name}' must take only 1 or 2 arguments"
            end
          end

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

        raise "Unknown infix operator '#{operator.term}'" unless @infix_operators.key?(operator.term)
        associativity, precedence = @infix_operators[operator.term]

        return lhs if precedence < min_precedence

        expr_atoms.shift

        next_precedence = associativity == :left ? precedence + 1 : precedence
        rhs = precedence_climb(expr_atoms, next_precedence)
        lhs = AST.new(operator.term, [lhs, rhs])
      end
    end
  end
end
