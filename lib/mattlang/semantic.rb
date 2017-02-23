require 'set'

module Mattlang
  class Semantic
    class Error < CompilerError
      def self.title; 'Semantic Error'; end

      attr_accessor :ast

      def initialize(message, ast = nil)
        @ast = ast
        super(message)
      end
    end

    BUILTIN_SIMPLE_TYPES = [:Nil, :Bool, :String, :Int, :Float, :EmptyList]
    BUILTIN_GENERIC_TYPES = { :List => 1 } # { :TypeAtom => parameter_count }
    BUILTIN_INFIX_OPERATORS = {
      :'=' => [:right, 0],
      :'|>' => [:left, 4],
      :'.' => [:left, 10]
    }

    attr_reader :cwd, :global_scope, :file_scope

    def self.debug(source)
      ast = Parser.new(source).parse
      semantic = new(Dir.pwd)
      ast = semantic.analyze(ast)
      puts ast.inspect
    end

    def initialize(cwd)
      @cwd = cwd
      @required_files = Set.new

      @global_scope = Scope.new

      BUILTIN_INFIX_OPERATORS.each do |op, (associativity, precedence)|
        @global_scope.define_infix_operator(op, associativity, precedence)
      end

      @file_scope = Scope.new(@global_scope)

      @simple_types = BUILTIN_SIMPLE_TYPES.dup
      @generic_types = BUILTIN_GENERIC_TYPES.dup

      # Load the kernel
      analyze(
        AST.new(:__top__, [
          AST.new(:__require__, [
            AST.new("kernel", type: Types::Simple.new(:String))
          ])
        ])
      )
    end

    def analyze(ast)
      raise Error.new("Expected first AST node to be __top__") if ast.term != :__top__

      expand_requires(ast, @cwd)
      hoist_infix_operators(ast, @global_scope)
      ast = rewrite_exprs(ast, @global_scope)
      ast = rewrite_operators(ast)
      hoist_functions(ast, @global_scope)
      check_scope_and_types(ast)

      ast
    end

    private

    def expand_requires(current_ast, relative_to)
      current_ast.children.each do |node|
        if node.term == :__require__
          filename = resolve_require_path(node.children.first.term, relative_to)

          if !@required_files.include?(filename)
            @required_files << filename

            required_ast = Parser.new(File.read(filename), filename: filename).parse
            expand_requires(required_ast, File.dirname(filename))

            node.children << required_ast
          end
        end
      end
    end

    def resolve_require_path(path, relative_to)
      path =
        if path.start_with?('.')
          File.join(relative_to, path)
        else
          File.join($MATT_LOAD_PATH, path)
        end

      path += '.matt' unless path.end_with?('.matt')
      File.realpath(path)
    end

    def hoist_infix_operators(current_ast, scope)
      current_ast.children.each do |node|
        if node.term == :__require__
          _, body = node.children
          hoist_infix_operators(body, scope) unless body.nil?
        elsif node.term == :__module__
          name, body = node.children
          name = name.term

          raise Error.new("The module name '#{name}' must begin with a capital letter") unless ('A'..'Z').include?(name[0])

          module_scope = scope.define_module(name)
          hoist_infix_operators(body, module_scope)
        elsif node.term == :__infix__
          operator, associativity, precedence = node.children.map(&:term)

          begin
            scope.define_infix_operator(operator, associativity, precedence)
          rescue Scope::Error => e
            e.ast = node
            raise e
          end
        end
      end
    end

    def hoist_functions(current_ast, scope)
      current_ast.children.each do |node|
        if node.term == :__require__
          _, body = node.children
          hoist_functions(body, scope) unless body.nil?
        elsif node.term == :__module__
          name, body = node.children
          module_scope = scope.define_module(name.term)
          hoist_functions(body, module_scope)
        elsif node.term == :__fn__
          signature, body = node.children
          name = signature.term
          args = signature.children.map { |arg| [arg.term, arg.type] }
          return_type = signature.type
          type_params = signature.meta && signature.meta[:type_params] || []

          raise Error.new("Cannot override builtin operator '#{name}'") if BUILTIN_INFIX_OPERATORS.keys.include?(name)

          if signature.meta && signature.meta[:operator]
            if args.count == 2
              begin
                scope.resolve_infix_operator(name)
              rescue Scope::Error => e
                e.ast = node
                raise e
              end
            elsif args.count != 1
              raise Error.new("The operator function '#{name}' must take only 1 or 2 arguments")
            end
          end

          type_params.combination(2).each do |t1, t2|
            raise Error.new("Type parameter '#{t1}' has already been defined in function '#{name}'") if t1 == t2
          end

          type_params.each do |type_param|
            raise Error.new("Type parameter '#{type_param}' in function '#{name}' cannot shadow the type already named '#{type_param}'") if @simple_types.include?(type_param)
          end

          args.each do |arg, types|
            [*types].each do |type|
              type.concrete_types.each do |concrete_type|
                if concrete_type.is_a?(Types::Generic)
                  if (arity = @generic_types[concrete_type.type_atom])
                    if concrete_type.type_parameters.size != arity
                      raise Error.new("Mismatched parameter count (#{concrete_type.type_parameters.size} instead of #{arity}) on generic type '#{concrete_type.type_atom}' for argument '#{arg}' of function '#{name}'")
                    end
                  else
                    raise Error.new("Unknown generic type '#{concrete_type.type_atom}' for argument '#{arg}' of function '#{name}'")
                  end
                elsif !(@simple_types + type_params).include?(concrete_type.type_atom)
                  raise Error.new("Unknown type '#{concrete_type}' for argument '#{arg}' of function '#{name}'")
                end
              end
            end
          end

          [*signature.type].each do |type|
            type.concrete_types.each do |concrete_type|
              if concrete_type.is_a?(Types::Generic)
                if (arity = @generic_types[concrete_type.type_atom])
                  if concrete_type.type_parameters.size != arity
                    raise Error.new("Mismatched parameter count (#{concrete_type.type_parameters.size} instead of #{arity}) on generic type '#{concrete_type.type_atom}' for argument '#{arg}' of function '#{name}'")
                  end
                else
                  raise Error.new("Unknown generic return type '#{concrete_type.type_atom}' for function '#{name}'")
                end
              elsif !(@simple_types + type_params).include?(concrete_type.type_atom)
                raise Error.new("Unknown return type '#{concrete_type}' for function '#{name}'")
              end
            end
          end

          scope.define_function(name, args, return_type, body, type_params: type_params.empty? ? nil : type_params)
        end
      end
    end

    def rewrite_exprs(node, scope)
      if node.term == :__expr__
        precedence_climb(node.children, scope)
      elsif node.term == :__module__
        name, body = node.children
        module_scope = scope.define_module(name.term)

        body.children = body.children.map { |c| rewrite_exprs(c, scope) } unless body.children.nil?
        node.children = [name, body]
        node
      else
        node.term = rewrite_exprs(node.term, scope) if node.term.is_a?(AST)
        node.children = node.children.map { |c| rewrite_exprs(c, scope) } unless node.children.nil?
        node
      end
    end

    def rewrite_operators(node)
      if node.term == :'|>'
        lhs, rhs = node.children

        lhs = rewrite_operators(lhs)
        rhs = rewrite_operators(rhs)

        raise Error.new("The pipe operator can only be applied to lambdas or functions") if rhs.term.is_a?(Symbol) && rhs.term != :__lambda__ && rhs.term.to_s.start_with?('__')

        if rhs.term == :__lambda__
          AST.new(rhs, [lhs], token: rhs.token)
        else
          if rhs.children.nil?
            rhs.children = [lhs]
          else
            rhs.children.unshift(lhs)
          end

          rhs
        end
      elsif node.term == :'.'
        lhs, rhs = node.children

        lhs = rewrite_operators(lhs)
        rhs = rewrite_operators(rhs)

        if lhs.children.nil? && lhs.term.is_a?(Symbol) && ('A'..'Z').include?(lhs.term[0])
          mod = lhs.meta && lhs.meta[:module] ? lhs.meta[:module] + [lhs] : [lhs]
          attach_module(mod, rhs)
          rhs
        elsif rhs.children.nil?
          node
        else
          children = rhs.children
          rhs.children = nil
          node.children = [lhs, rhs]

          if rhs.meta && rhs.meta[:no_paren]
            meta = { no_paren: rhs.meta[:no_paren] }
            rhs.meta.delete(:no_paren)
          end

          AST.new(node, children, meta: meta, token: node.token)
        end
      else
        node.term = rewrite_operators(node.term) if node.term.is_a?(AST)
        node.children = node.children.map { |c| rewrite_operators(c) } unless node.children.nil?
        node
      end
    end

    def attach_module(mod, node)
      if node.term.is_a?(AST)
        attach_module(mod, node.term)
      else
        if node.term.to_s.start_with?('__')
          raise Error.new("Invalid member access", node)
        else
          node.meta ||= {}
          node.meta[:module] = mod
        end
      end
    end

    def precedence_climb(expr_atoms, scope, min_precedence = 0)
      lhs = expr_atoms.shift
      raise "Unexpected empty expr atom" if lhs.nil?

      lhs = rewrite_exprs(lhs, scope)

      loop do
        return lhs if expr_atoms.empty?

        operator = expr_atoms.first

        begin
          associativity, precedence = scope.resolve_infix_operator(operator.term)
        rescue Scope::Error => e
          e.ast = operator
          raise e
        end

        return lhs if precedence < min_precedence

        expr_atoms.shift

        next_precedence = associativity == :left ? precedence + 1 : precedence
        rhs = precedence_climb(expr_atoms, scope, next_precedence)
        lhs = AST.new(operator.term, [lhs, rhs], token: operator.token)
      end
    end

    def check_scope_and_types(ast)
      visit(ast, @file_scope)
    end

    def visit(node, scope)
      case node.term
      when :__top__, :__block__
        visit_block(node, scope)
      when :__module__
        visit_module(node, scope)
      when :__require__
        visit_require(node, scope)
      when :__if__
        visit_if(node, scope)
      when :__embed__
        check_embed(node, scope)
      when :__fn__
        visit_fn(node, scope)
      when :__infix__
        node.type = Types::Simple.new(:Nil)
      when :'='
        visit_assignment(node, scope)
      when :'.'
        visit_access(node, scope)
      when :__list__
        visit_list(node, scope)
      when :__tuple__
        visit_tuple(node, scope)
      when :__record__
        visit_record(node, scope)
      when :__lambda__
        visit_lambda(node, scope)
      else
        visit_expr(node, scope)
      end
    end

    def visit_block(node, scope)
      if node.children.empty?
        node.type = Types::Simple.new(:Nil)
      else
        node.children.each { |c| visit(c, scope) }
        node.type = node.children.last.type || Types::Simple.new(:Nil)
      end
    end

    def visit_module(node, scope)
      name, body = node.children

      module_scope = scope.resolve_module(name.term)
      visit(body, module_scope)

      node.type = Types::Simple.new(:Nil)
    end

    def visit_require(node, scope)
      _, body = node.children

      if body
        # Use a separate scope for each required file so that variable binding is hygienic
        require_scope = Scope.new(@global_scope)
        visit(body, require_scope)
      end

      node.type = Types::Simple.new(:Nil)
    end

    def visit_fn(node, scope)
      signature, body = node.children
      name = signature.term
      return_type = signature.type

      inner_scope = Scope.new(scope)
      signature.children.each { |arg| inner_scope.define(arg.term, arg.type) }
      (signature.meta && signature.meta[:type_params] || []).each { |type_param| inner_scope.define_type(type_param) }

      visit(body, inner_scope)

      raise Error.new("Type mismatch; expected return type '#{return_type}' for function '#{name}' but found '#{body.type}'") if return_type != body.type

      node.type = Types::Simple.new(:Nil)
    end

    def visit_if(node, scope)
      conditional, then_block, else_block = node.children

      visit(conditional, scope)
      raise Error.new("If statements only accept boolean conditionals") unless conditional.type == Types::Simple.new(:Bool)

      then_scope = Scope.new(scope)
      visit(then_block, then_scope)

      else_scope = Scope.new(scope)
      visit(else_block, else_scope)

      shared_binding = then_scope.binding.keys & else_scope.binding.keys

      shared_binding.each do |name|
        scope.define(name, Types.combine([then_scope.binding[name].type, else_scope.binding[name].type]))
      end

      (then_scope.binding.keys - shared_binding).each do |name|
        if scope.binding.key?(name)
          scope.define(name, Types.combine([scope.binding[name].type, then_scope.binding[name].type]))
        else
          scope.define(name, Types.combine([Types::Simple.new(:Nil), then_scope.binding[name].type]))
        end
      end

      (else_scope.binding.keys - shared_binding).each do |name|
        if scope.binding.key?(name)
          scope.define(name, Types.combine([scope.binding[name], else_scope.binding[name]]))
        else
          scope.define(name, Types.combine([Types::Simple.new(:Nil), else_scope.binding[name]]))
        end
      end

      node.type = Types.combine([then_block.type, else_block.type])
    end

    def visit_assignment(node, scope)
      lhs, rhs = node.children

      # The variable identifier node should not have any children or have its type already set.
      if lhs.children.nil? && lhs.type.nil?
        visit(rhs, scope)
        scope.define(lhs.term, rhs.type)
        node.type = rhs.type
        lhs.type = rhs.type
      else
        raise Error.new("Invalid left-hand-side of assignment operator '='; an identifier was expected")
      end
    end

    def visit_access(node, scope)
      lhs, rhs = node.children

      visit(lhs, scope)

      if lhs.type.is_a?(Types::Record) && rhs.term.is_a?(Symbol)
        if lhs.type.types_hash.key?(rhs.term)
          node.type = lhs.type.types_hash[rhs.term]
        else
          raise Error.new("Field '#{rhs.term}' is not a member of #{lhs.type}", rhs)
        end
      elsif lhs.type.is_a?(Types::Tuple) && rhs.term.is_a?(Symbol) && (index = rhs.term.to_s.to_i).to_s == rhs.term.to_s
        if index < lhs.type.types.size
          node.type = lhs.type.types[index]
        else
          raise Error.new("Cannot access index #{index} of a #{lhs.type.types.size}-tuple", rhs)
        end
      else
        raise Error.new("Invalid member access", node)
      end
    end

    def visit_list(node, scope)
      node.children.each { |c| visit(c, scope) }

      node.type =
        if node.children.empty?
          Types::Simple.new(:EmptyList)
        else
          Types::Generic.new(:List, [Types.combine(node.children.map(&:type))])
        end
    end

    def visit_tuple(node, scope)
      node.children.each { |c| visit(c, scope) }
      node.type = Types::Tuple.new(node.children.map(&:type))
    end

    def visit_record(node, scope)
      types = node.children.map do |field_node|
        expr_node = field_node.children.first
        visit(expr_node, scope)
        field_node.type = expr_node.type

        [field_node.term, field_node.type]
      end

      node.type = Types::Record.new(types.to_h)
    end

    def visit_lambda(node, scope)
      args, body = node.children

      bound_types = scope.bound_types
      arg_types =
        if bound_types.empty?
          args.children.map { |c| c.type }
        else
          args.children.map { |c| c.type.replace_type_bindings(bound_types) }
        end

      inner_scope = Scope.new(scope)
      args.children.zip(arg_types).each { |arg, type| inner_scope.define(arg.term, type) }

      visit(body, inner_scope)

      return_type = bound_types.empty? ? body.type : body.type.replace_type_bindings(bound_types)

      node.type = Types::Lambda.new(arg_types, return_type)
    end

    def visit_expr(node, scope)
      term_scope =
        if node.meta && node.meta[:module]
          node.meta[:module].reduce(scope) do |s, m|
            begin
              s.resolve_module(m.term)
            rescue Scope::Error => e
              e.ast = m
              raise e
            end
          end
        else
          scope
        end

      if !node.children.nil? # Node is a function or lambda call if it has children (even if it is an empty array)
        if node.term.is_a?(AST)
          visit(node.term, scope)

          raise Error.new("Invalid lambda call") if !node.term.type.is_a?(Types::Lambda)
          raise Error.new("Lambdas must be called with parens") if node.meta && node.meta[:no_paren]

          node.children.each { |c| visit(c, scope) }

          if node.term.type.args.size == node.children.size && node.children.map(&:type).zip(node.term.type.args).all? { |arg_type, lambda_arg| arg_type.subtype?(lambda_arg) }
            node.type = node.term.type.return_type
          else
            raise Error.new("Lambda expected (#{node.term.type.args.join(', ')}) but was called with (#{node.children.map(&:type).join(', ')})")
          end
        else
          arg_types = node.children.map do |c|
            if c.term == :__lambda__ && c.meta && c.meta[:untyped]
              { node: c, arg_count: c.children.first.children.size, candidate_types: [] }
            else
              visit(c, scope)
              c.type
            end
          end

          if arg_types.any? { |c| c.is_a?(Hash) }
            begin
              term_scope.resolve_function(node.term, arg_types, exclude_lambdas: node.meta && node.meta[:no_paren], infer_untyped_lambdas: true)
            rescue Scope::Error => e
              e.ast = node
              raise e
            end

            arg_types.map! do |c|
              if c.is_a?(Hash)
                type_checked_lambda = nil

                candidates = c[:candidate_types].map do |lambda_type|
                  duped_lambda = c[:node].dup
                  duped_lambda.children.first.children.each_with_index { |arg, i| arg.type = lambda_type.args[i] }

                  begin
                    visit(duped_lambda, scope)

                    if lambda_type.return_type.is_a?(Types::Simple) && lambda_type.return_type.parameter_type?
                      inferred_type_binding = { lambda_type.return_type.type_atom => duped_lambda.type.return_type }

                      duped_lambda = c[:node].dup
                      duped_lambda.children.first.children.each_with_index { |arg, i| arg.type = lambda_type.args[i].replace_type_bindings(inferred_type_binding) }

                      visit(duped_lambda, scope)
                    end
                  rescue => e
                    puts "Warning during lambda type inference: #{e}"
                    next nil
                  end

                  type_checked_lambda = duped_lambda
                  duped_lambda.type
                end.compact

                if candidates.size == 0
                  raise Error.new("No type could be inferred for lambda argument sent to function '#{node.term}'")
                elsif candidates.size > 1
                  raise Error.new("Ambiguous type inferred for lambda argument sent to function '#{node.term}'")
                else
                  c[:node].children = type_checked_lambda.children
                  c[:node].type = type_checked_lambda.type
                  candidates.first
                end
              else
                c
              end
            end
          end

          begin
            node.type = term_scope.resolve_function(node.term, arg_types, exclude_lambdas: node.meta && node.meta[:no_paren])
          rescue Scope::Error => e
            e.ast = node
            raise e
          end
        end
      elsif node.type.nil? # Node is an identifier (variable or arity-0 function) if it doesn't have a type yet
        begin
          node.type  = term_scope.resolve(node.term)
        rescue Scope::Error => e
          e.ast = node
          raise e
        end
      end # Else the node is a literal, so do nothing
    end

    def check_embed(node, scope)
      node.type = node.type.replace_type_bindings(scope.bound_types) if !scope.bound_types.empty?

      node.type.concrete_types.each do |concrete_type|
        if concrete_type.is_a?(Types::Generic)
          if (arity = @generic_types[concrete_type.type_atom])
            if concrete_type.type_parameters.size != arity
              raise Error.new("Mismatched parameter count (#{concrete_type.type_parameters.size} instead of #{arity}) on generic type '#{concrete_type.type_atom}' for embed")
            end
          else
            raise Error.new("Unknown generic type '#{concrete_type.type_atom}' for embed")
          end
        elsif !@simple_types.include?(concrete_type.type_atom) && !scope.type_exists?(concrete_type.type_atom)
          raise Error.new("Unknown type '#{concrete_type}' for embed")
        end
      end
    end
  end
end
