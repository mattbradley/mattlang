require 'mattlang/interpreter/frame'
require 'mattlang/interpreter/pattern_matcher'
require 'mattlang/interpreter/value'
require 'mattlang/interpreter/list'
require 'mattlang/interpreter/tuple'
require 'mattlang/interpreter/record'
require 'mattlang/interpreter/lambda'

module Mattlang
  class Interpreter
    class Panic < StandardError; end

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

    def execute(node, tail_position: false)
      case node.term
      when :__top__, :__block__
        execute_block(node, tail_position: tail_position)
      when :__require__
        execute_require(node)
      when :__module__
        execute_module(node)
      when :__if__
        execute_if(node, tail_position: tail_position)
      when :__case__
        execute_case(node, tail_position: tail_position)
      when :__embed__
        execute_embed(node)
      when :__lambda__
        execute_lambda_literal(node)
      when :__fn__, :__infix__, :__type__, :__typealias__, :__protocol__, :__impl__
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
        execute_expr(node, tail_position: tail_position)
      end
    end

    def execute_block(node, tail_position:)
      return Value.new(nil, Types::Simple.new(:Nil)) if node.children.empty?

      node.children[0...-1].each do |child|
        execute(child, tail_position: false)
      end

      execute(node.children.last, tail_position: tail_position)
    end

    def execute_require(node)
      file, ast = node.children

      push_frame
      execute(ast) unless ast.nil?
      pop_frame

      Value.new(nil, Types::Simple.new(:Nil))
    end

    def execute_module(node)
      name, body = node.children

      push_scope(@current_scope.resolve_module(name.term))

      execute(body)

      pop_scope

      Value.new(nil, Types::Simple.new(:Nil))
    end

    def execute_if(node, tail_position:)
      conditional, then_block, else_block = node.children

      value =
        if execute(conditional).value == true
          execute(then_block, tail_position: tail_position)
        else
          execute(else_block, tail_position: tail_position)
        end

      if node.meta && node.meta[:nil_bindings]
        node.meta[:nil_bindings].each { |variable| @current_frame[variable] || (@current_frame[variable] = Value.new(nil, Types::Simple.new(:Nil))) }
      end

      value
    end

    def execute_case(node, tail_position:)
      subject, patterns = node.children

      subject_value = execute(subject)

      patterns.children.each do |pattern|
        head, branch = pattern.children

        if (bindings = PatternMatcher.case_match(head, subject_value))
          # We use a separate frame for the head bindings and the branch bindings because the
          # wildcard variables bound in the pattern head go out of scope after the branch is
          # executed, but the variables bound in the branch bubble up to the outer frame.
          head_frame = Frame.new(@current_frame)
          branch_frame = Frame.new(head_frame)

          head_frame.variables.merge!(bindings)

          @current_frame = branch_frame

          value = execute(branch, tail_position: tail_position)

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

      value.is_a?(Value) ? value : Value.new(value, node.type.replace_type_bindings(@current_context))
    end

    def execute_lambda_literal(node)
      args, body = node.children

      Value.new(Lambda.new(args.children.map(&:term), node.type, @current_frame.dup, @current_scope, @current_context, body), node.type.replace_type_bindings(@current_context))
    end

    def execute_match(node)
      lhs, rhs = node.children
      value = execute(rhs)

      @current_frame.variables.merge!(PatternMatcher.destructure_match(lhs, value))

      value
    end

    def execute_list(node)
      Value.new(List.new(node.children.map { |c| execute(c) }), node.type.replace_type_bindings(@current_context))
    end

    def execute_tuple(node)
      Value.new(Tuple.new(node.children.map { |c| execute(c) }), node.type.replace_type_bindings(@current_context))
    end

    def execute_record(node)
      record = Record.new

      node.children.map do |field_node|
        record[field_node.term] = execute(field_node.children.first)
      end

      Value.new(record, node.type.replace_type_bindings(@current_context))
    end

    def execute_expr(node, tail_position:)
      term_scope = @current_scope.resolve_module_path(node.meta[:module_path]) if node.meta && node.meta[:module_path]

      if node.children.nil? # Literal, variable, 0-arity function, or nullary constructor
        if node.term.is_a?(Symbol)
          if ('A'..'Z').include?(node.term[0])
            term_scope =
              if node.meta && node.meta[:module_path]
                force_scope = true
                @current_scope.resolve_module_path(node.meta[:module_path])
              else
                @current_scope
              end

            type = term_scope.resolve_typedef(node.term, nil, force_scope: force_scope)
            Value.new(nil, type)
          elsif (value = @current_frame[node.term])
            value
          else
            push_scope(term_scope) if term_scope

            function, fn_type_bindings, fn_parent_scope = @current_scope.find_runtime_function(node.term, [])

            result =
              if tail_position
                [function, [], fn_type_bindings, fn_parent_scope]
              else
                execute_function(function, [], fn_type_bindings, fn_parent_scope)
              end

            pop_scope if term_scope

            result
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
          if ('A'..'Z').include?(node.term[0]) # Named constructor
            argument =
              if node.children.count == 0
                nil
              elsif node.children.count > 1
                Value.new(Tuple.new(args), Types::Tuple.new(args.map(&:type)))
              else
                args.first
              end

            term_scope =
              if node.meta && node.meta[:module_path]
                force_scope = true
                @current_scope.resolve_module_path(node.meta[:module_path])
              else
                @current_scope
              end

            type = term_scope.resolve_typedef(node.term, argument&.type, force_scope: force_scope)
            Value.new(argument, type)
          elsif (lambda_fn = @current_frame[node.term]) && lambda_fn.type.is_a?(Types::Lambda) && lambda_fn.type.args.size == args.size && lambda_fn.type.args.zip(args).all? { |lambda_arg, arg| lambda_arg.subtype?(arg.type) }
            execute_lambda(lambda_fn.value, args)
          else
            push_scope(term_scope) if term_scope

            arg_types = args.map { |a| a.type.replace_type_bindings(@current_context) }
            function, fn_type_bindings, fn_parent_scope = @current_scope.find_runtime_function(node.term, arg_types)

            # If this function call is in the tail position, then let's do a
            # tail call elimination. The stack is unwound back to the previous
            # function call, which then calls this function with the provided
            # arguments and type bindings.
            result =
              if tail_position
                [function, args, fn_type_bindings, fn_parent_scope]
              else
                execute_function(function, args, fn_type_bindings, fn_parent_scope)
              end

            pop_scope if term_scope

            result
          end
        else
          raise "Expected to see an AST here" if !node.term.is_a?(AST)
          lambda_fn = execute(node.term)
          execute_lambda(lambda_fn.value, args)
        end
      end
    end

    def execute_function(function, args, type_bindings, parent_scope)
      result = [function, args, type_bindings, parent_scope]

      # The result is an Array if the function body ends in another function
      # that should be tail call eliminated. This while loop keeps looping as
      # long as functions keep returning with functions in the tail position.
      # This keeps Ruby's call stack from overflowing when a recursive function
      # executes itself many times.
      while result.is_a?(Array)
        function, args, type_bindings, parent_scope = result

        push_frame(type_bindings)
        push_scope(parent_scope)

        function.args.zip(args).each do |(name, type), value|
          # If the value is a nominal type, upcast it into the expected type for this argument
          loop do
            if value.type.is_a?(Types::Nominal) &&
              type.replace_type_bindings(type_bindings).matching_types { |t|
                t.is_a?(Types::Nominal) &&
                  t.type_atom == value.type.type_atom &&
                  t.module_path == value.type.module_path &&
                  t.type_parameters.count == value.type.type_parameters.count &&
                  t.type_parameters.zip(value.type.type_parameters).all? { |param, other_param| param.subtype?(other_param, nil, true) }
              }.empty?

              value = value.value
            else
              break
            end
          end

          @current_frame[name] = value
        end

        result = execute(function.body, tail_position: true)

        pop_scope
        pop_frame
      end

      result
    end

    def execute_lambda(lambda_fn, args)
      push_frame(lambda_fn.context)
      push_scope(lambda_fn.scope)

      lambda_fn.frame.each { |name, value| @current_frame[name] = value }
      lambda_fn.args.zip(args).each { |name, value| @current_frame[name] = value }
      value = execute(lambda_fn.body)

      pop_scope
      pop_frame

      value
    end
  end
end
