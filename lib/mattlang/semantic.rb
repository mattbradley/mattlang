require 'mattlang/scope'
require 'mattlang/function'
require 'mattlang/variable'

module Mattlang
  class Semantic
    BUILTIN_TYPES = [:Nil, :Bool, :String, :Int, :Float]
    BUILTIN_INFIX_OPERATORS = {
      :'=' => [:right, 0],
      :'.' => [:left, 9]
    }

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
      @types = BUILTIN_TYPES.map { |t| [t, t] }.to_h
      @infix_operators = BUILTIN_INFIX_OPERATORS.dup
      @functions = {}
      @global_scope = Scope.new
    end
    
    def analyze
      populate_symbols
      @ast = rewrite_exprs(@ast)
      check_scope_and_types
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

          raise "Cannot override builtin operator '#{name}'" if BUILTIN_INFIX_OPERATORS.keys.include?(name)

          if signature.meta && signature.meta[:operator]
            if args.count == 2
              raise "Unknown infix operator '#{name}'" unless @infix_operators.key?(name)
            elsif args.count != 1
              raise "The operator function '#{name}' must take only 1 or 2 arguments"
            end
          end

          args.each do |arg, type|
            raise "Unknown type '#{type}' for argument '#{arg}' of function '#{name}'" unless @types.key?(type)
          end

          raise "Unknown return type '#{return_type}' of function '#{name}'" unless @types.key?(return_type)

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
      when :__top__
        visit_block(node, Scope.new(scope))
      when :__block__
        visit_block(node, scope)
      when :__if__
        visit_if(node, scope)
      when :__embed__
        # Type already set by parser
      when :__fn__
        visit_fn(node, scope)
      when :__infix__
        node.type = :Nil
      when :'='
        visit_assignment(node, scope)
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
      signature, body = node.children
      name = signature.term
      args = signature.children.first.children.map { |arg| [arg.term, arg.children.first.term] }
      return_type = signature.children.last.term

      inner_scope = Scope.new(scope)
      args.each { |arg, type| inner_scope.define(arg, type) }
      visit(body, inner_scope)

      raise "Type mismatch; expected return type '#{return_type}' for function '#{name}' but found '#{body.type}'" if return_type != body.type

      node.type = :Nil
    end

    def visit_if(node, scope)
      conditional, then_block, else_block = node.children

      visit(conditional, scope)

      then_scope = Scope.new(scope)
      visit(then_block, then_scope)

      else_scope = Scope.new(scope)
      visit(else_block, else_scope)

      shared_binding = then_scope.binding.keys & else_scope.binding.keys

      shared_binding.each do |name|
        scope.define(name, [then_scope.binding[name].type, else_scope.binding[name].type])
      end

      (then_scope.binding.keys - shared_binding).each do |name|
        if scope.binding.key?(name)
          scope.define(name, [scope.binding[name].type, then_scope.binding[name].type])
        else
          scope.define(name, [:Nil, then_scope.binding[name].type])
        end
      end

      (else_scope.binding.keys - shared_binding).each do |name|
        if scope.binding.key?(name)
          scope.define(name, [scope.binding[name], else_scope.binding[name]])
        else
          scope.define(name, [:Nil, else_scope.binding[name]])
        end
      end

      node.type = [then_block.type, else_block.type]
    end

    def visit_assignment(node, scope)
      lhs, rhs = node.children

      # The variable identifier node should not have
      # any children or have its type already set.
      if lhs.children.nil? && lhs.type.nil?
        visit(rhs, scope)
        scope.define(lhs.term, rhs.type)
        node.type = rhs.type
        lhs.type = rhs.type
      else
        raise "Invalid left-hand-side of assignment operator '='; an identifier was expected"
      end
    end

    def visit_expr(node, scope)
      if !node.children.nil? # Node is a function if it has children
        node.children.each { |c| visit(c, scope) }
        node.type = scope.resolve_function(node.term, node.children.map(&:type)).return_type
      elsif node.type.nil? # Node is an identifier if it doesn't have a type yet
        resolution = scope.resolve(node.term)
        node.type = resolution.is_a?(Function) ? resolution.return_type : resolution.type
      end
    end
  end
end
