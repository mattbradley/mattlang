require 'mattlang/interpreter/frame'
require 'mattlang/interpreter/pattern_matcher'
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
      @current_frame = Frame.new
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
      @current_frame = Frame.new
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
      when :__case__
        execute_case(node)
      when :__embed__
        execute_embed(node)
      when :__lambda__
        execute_lambda_literal(node)
      when :__fn__, :__infix__, :__typealias__
        Value.new(nil, Types::Simple.new(:Nil))
      when :'='
        execute_match(node)
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

      value =
        if execute(conditional).value == true
          execute(then_block)
        else
          execute(else_block)
        end

      if node.meta && node.meta[:nil_bindings]
        node.meta[:nil_bindings].each { |variable| @current_frame[variable] || (@current_frame[variable] = Value.new(nil, Types::Simple.new(:Nil))) }
      end

      value
    end

    def execute_case(node)
      subject, *pattern_nodes = node.children
      subject_value = execute(subject)

      pattern_nodes.each do |pattern|
        head, branch = pattern.children

        if (bindings = PatternMatcher.case_match(head, subject_value))
          # We use a separate frame for the head bindings and the branch bindings because the
          # wildcard variables bound in the pattern head go out of scope after the branch is
          # executed, but the variables bound in the branch bubble up to the outer frame.
          head_frame = Frame.new(@current_frame)
          branch_frame = Frame.new(head_frame)

          head_frame.variables.merge!(bindings)

          @current_frame = branch_frame

          value = execute(branch)

          # Pop back out to the outer frame
          @current_frame = head_frame.parent_frame

          # Move any variables bound in the branch to the outer frame
          @current_frame.variables.merge!(branch_frame.variables)

          if node.meta && node.meta[:nil_bindings]
            node.meta[:nil_bindings].each { |variable| @current_frame[variable] || (@current_frame[variable] = Value.new(nil, Types::Simple.new(:Nil))) }
          end

          return value
        end
      end

      raise "The value '#{subject_value.inspect}' didn't match any of the case patterns"
    end

    def execute_embed(node)
      context = @current_context
      scope = @current_scope

      obj = Object.new
      obj.define_singleton_method(:cast) do |v, type|
        v = v.is_a?(Value) ? v.value : v
        type = Parser.new(type).parse_type
        type = type.replace_type_bindings(context)
        type = scope.resolve_type(type)
        Value.new(v, type)
      end
      @current_frame.each { |k, v| obj.instance_variable_set("@#{k}", v) }

      value = obj.instance_eval(node.children.first.term)

      value.is_a?(Value) ? value : Value.new(value, node.type)
    end

    def execute_lambda_literal(node)
      args, body = node.children

      Value.new(Lambda.new(args.children.map(&:term), node.type, @current_frame.dup, body), node.type.replace_type_bindings(@current_context))
    end

    def execute_match(node)
      lhs, rhs = node.children
      value = execute(rhs)

      @current_frame.variables.merge!(PatternMatcher.destructure_match(lhs, value))

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
      term_scope = @current_scope.resolve_module_path(node.meta[:module_path].map(&:term)) if node.meta && node.meta[:module_path]

      if node.children.nil? # Literal, variable, or 0-arity function
        if node.term.is_a?(Symbol)
          if (value = @current_frame[node.term])
            value
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
          if tuple_or_record.value.is_a?(Tuple)
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
