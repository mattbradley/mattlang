require 'mattlang/scope'
require 'mattlang/function'
require 'mattlang/variable'

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
      @global_scope = Scope.new
    end
    
    def analyze
      populate_symbols
      @ast = rewrite_exprs(@ast)
      #check_scope_and_types
    end

    private

    def populate_symbols
      raise "Unexpected node '#{@ast.term}'; expected top-level node" if @ast.term != :__top__

      @ast.children.each do |node|
        if node.term == :__infix__
          operator, associativity, precedence = node.children.map(&:term)

          raise "The infix operator '#{operator}' has already been declared" if @infix_operators.key?(operator)

          @infix_operators[operator] = [associativity, precedence]
        end
      end

      @ast.children.each do |node|
        if node.term == :__fn__
          signature, body = node.children
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

          @global_scope.define_function(name, args, return_type, body)
        end
      end
    end

    def rewrite_exprs(node)
      if node.term == :__expr__
        precedence_climb(node.children)
      else
        node.children = node.children.map { |c| rewrite_exprs(c) } unless node.children.nil?
        node
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

    def check_scope_and_types
      visit(@ast, @global_scope)
    end

    def visit(node, scope)
      case node.term
      when :__top__, :__block__
        visit_block(node, scope)
      when :__if__
        visit_if(node, scope)
      when :__embed__
        # Type already set by parser
      when :__fn__
        visit_fn(node, scope)
      when :__infix__
        node.type = :Nil
      else
        visit_expr(node, scope)
      end
    end

    def visit_block(node, scope)
      if node.children.empty?
        node.type = :Nil
      else
        node.children.each { |c| visit(c, scope) }
        node.type = node.children.last.type
      end
    end

    def visit_fn(node, scope)

    end

    def visit_if(node, scope)

    end

    def visit_expr(node, scope)
    end
  end
end
