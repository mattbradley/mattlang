require 'mattlang/interpreter/value'
require 'mattlang/interpreter/list'
require 'mattlang/interpreter/tuple'
require 'mattlang/interpreter/record'
require 'mattlang/interpreter/lambda'

module Mattlang
  class Interpreter
    attr_accessor :current_scope, :current_frame

    def self.debug(source)
      ast = Parser.new(source).parse
      semantic = Semantic.new(Dir.pwd)
      ast = semantic.analyze(ast)
      interpreter = new(semantic.global_scope)

      last_value = interpreter.interpret(ast)
      puts "Binding: #{interpreter.current_frame.inspect}"
      last_value
    end

    def self.interpret_file(filename)
      filename = File.realpath(filename)

      ast = Parser.new(File.read(filename)).parse
      semantic = Semantic.new(File.dirname(filename))
      ast = semantic.analyze(ast)

      new(semantic.global_scope).interpret(ast)
    end

    def initialize(current_scope = Scope.new)
      @scopes = []
      @frames = []
      @contexts = []

      @current_scope = current_scope
      @current_frame = {}
      @current_context = {}
    end

    def interpret(ast)
      execute(ast)
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
      when :__require__
        execute_require(node)
      when :__module__
        execute_module(node)
      when :__if__
        execute_if(node)
      when :__embed__
        execute_embed(node)
      when :__lambda__
        execute_lambda_literal(node)
      when :__fn__, :__infix__
        Value.new(nil, Types::Simple.new(:Nil))
      when :'='
        execute_assignment(node)
      when :__list__
        execute_list(node)
      when :__tuple__
        execute_tuple(node)
      when :__record__
        execute_record(node)
      else
        raise "Unknown term #{node.term}" if node.term.is_a?(Symbol) && node.term.to_s.start_with?("__")
        execute_expr(node)
      end
    end

    def execute_block(node)
      last_value = Value.new(nil, Types::Simple.new(:Nil))

      node.children.each do |child|
        last_value = execute(child)
      end

      last_value
    end

    def execute_require(node)
      file, ast = node.children

      push_frame
      execute(ast) unless ast.nil?
      pop_frame

      Value.new(nil, Types::Simple.new(:Nil))
    end

    def execute_module(node)
      _, body = node.children

      execute(body)

      Value.new(nil, Types::Simple.new(:Nil))
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

      Value.new(Lambda.new(args.children.map(&:term), node.type, @current_frame.dup, body), node.type.replace_type_bindings(@current_context))
    end

    def execute_assignment(node)
      lhs, rhs = node.children

      value = execute(rhs)
      @current_frame[lhs.term] = value
      value
    end

    def execute_list(node)
      Value.new(List.new(node.children.map { |c| execute(c) }), node.type)
    end

    def execute_tuple(node)
      Value.new(Tuple.new(node.children.map { |c| execute(c) }), node.type)
    end

    def execute_record(node)
      record = Record.new

      node.children.map do |field_node|
        record[field_node.term] = execute(field_node.children.first)
      end

      Value.new(record, node.type)
    end

    def execute_expr(node)
      if node.meta && node.meta[:module]
        term_scope = node.meta[:module].reduce(@current_scope) { |s, m| s.resolve_module(m.term) }
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
      elsif node.term == :'.' # Member access
        tuple_or_record, member = node.children
        tuple_or_record = execute(tuple_or_record)

        member =
          if tuple_or_record.is_a?(Tuple)
            member.term.to_s.to_i
          else
            member.term
          end

        tuple_or_record[member]
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
