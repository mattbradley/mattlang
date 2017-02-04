require 'mattlang/interpreter/value'
require 'mattlang/interpreter/lambda'

module Mattlang
  class Interpreter
    attr_reader :source, :semantic, :current_frame

    def self.debug(source)
      interpreter = new(source)
      last_value = interpreter.interpret
      puts "Binding: #{interpreter.current_frame.inspect}"
      last_value
    end

    def initialize(source)
      kernel = File.read('src/kernel.matt')
      @source = kernel + "\n" + source
      @scopes = []
      @frames = []
      @contexts = []
    end

    def interpret
      ast = Parser.new(@source).parse
      @semantic = Semantic.new(ast)
      @semantic.analyze

      @current_scope = @semantic.global_scope
      @current_frame = {}
      @current_context = {}
      @last_value = execute(@semantic.ast)
    end

    private

    def push_scope(scope)
      @scopes.push(@current_scope)
      @current_scope = scope
    end

    def pop_scope
      @current_scope = @scopes.pop
    end

    def push_frame(type_bindings = nil)
      @frames.push(@current_frame)
      @contexts.push(@current_context)
      @current_frame = {}
      @current_context = type_bindings
    end

    def pop_frame
      @current_frame = @frames.pop
      @current_context = @contexts.pop
    end

    def execute(node)
      case node.term
      when :__top__, :__block__
        execute_block(node)
      when :__if__
        execute_if(node)
      when :__embed__
        execute_embed(node)
      when :__lambda__
        execute_lambda_literal(node)
      when :__module__, :__fn__, :__infix__
        # do nothing
      when :'='
        execute_assignment(node)
      when :__list__
        execute_list(node)
      else
        execute_expr(node)
      end
    end

    def execute_block(node)
      last_value = nil

      node.children.each do |child|
        last_value = execute(child)
      end

      last_value
    end

    def execute_if(node)
      conditional, then_block, else_block = node.children

      if execute(conditional).value == true
        execute(then_block)
      else
        execute(else_block)
      end
    end

    def execute_embed(node)
      obj = Object.new
      obj.define_singleton_method(:cast) { |v, type| Value.new(v.is_a?(Value) ? v.value : v, Parser.new(type).parse_type) }
      @current_frame.each { |k, v| obj.instance_variable_set("@#{k}", v) }

      value = obj.instance_eval(node.children.first.term)

      value.is_a?(Value) ? value : Value.new(value, node.type)
    end

    def execute_lambda_literal(node)
      args, body = node.children

      Value.new(Lambda.new(args.children.map(&:term), @current_frame.dup, body), node.type.replace_type_bindings(@current_context))
    end

    def execute_assignment(node)
      lhs, rhs = node.children

      value = execute(rhs)
      @current_frame[lhs.term] = value
      value
    end

    def execute_list(node)
      Value.new(node.children.map { |c| execute(c) }, node.type)
    end

    def execute_expr(node)
      if node.meta && node.meta[:module]
        term_scope = node.meta[:module].reduce(@current_scope) { |s, m| s.resolve_module(m) }
      end

      if node.children.nil? # Literal, variable, or 0-arity function
        if node.term.is_a?(Symbol)
          if @current_frame.key?(node.term)
            @current_frame[node.term]
          else
            push_scope(term_scope) if term_scope

            function, fn_type_bindings = @current_scope.find_runtime_function(node.term, [])
            value = execute_function(function, [], fn_type_bindings)

            pop_scope if term_scope

            value
          end
        else
          Value.new(node.term, node.type)
        end
      else # Function or lambda call
        args = node.children.map { |arg| execute(arg) }

        if node.term.is_a?(Symbol)
          if (lambda_fn = @current_frame[node.term]) && lambda_fn.type.is_a?(Types::Lambda) && lambda_fn.type.args.size == args.size && lambda_fn.type.args.zip(args).all? { |lambda_arg, arg| lambda_arg.subtype?(arg.type) }
            execute_lambda(lambda_fn.value, args)
          else
            push_scope(term_scope) if term_scope

            function, fn_type_bindings = @current_scope.find_runtime_function(node.term, args.map(&:type))
            value = execute_function(function, args, fn_type_bindings)

            pop_scope if term_scope

            value
          end
        else
          raise "Expected to see an AST here" if !node.term.is_a?(AST)
          lambda_fn = execute(node.term)
          execute_lambda(lambda_fn.value, args)
        end
      end
    end

    def execute_function(function, args, type_bindings)
      push_frame(type_bindings)

      function.args.map(&:first).zip(args).each { |name, value| @current_frame[name] = value }
      value = execute(function.body)
      value.replace_type_bindings(type_bindings) if type_bindings

      pop_frame

      value
    end

    def execute_lambda(lambda_fn, args)
      push_frame

      lambda_fn.frame.each { |name, value| @current_frame[name] = value }
      lambda_fn.args.zip(args).each { |name, value| @current_frame[name] = value }
      value = execute(lambda_fn.body)

      pop_frame

      value
    end
  end
end
