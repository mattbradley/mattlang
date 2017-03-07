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
  end
end

require 'mattlang/semantic/pattern_matcher'

module Mattlang
  class Semantic
    include PatternMatcher

    # Value is the number of type parameters
    BUILTIN_TYPES = {
      :Nil       => 0,
      :Bool      => 0,
      :String    => 0,
      :Int       => 0,
      :Float     => 0,
      :List      => 1,
      :Nothing   => 0,
      :Anything  => 0
    }

    BUILTIN_INFIX_OPERATORS = {
      :'=' => [:right, 0],
      :'|>' => [:left, 4],
      :'.' => [:left, 10]
    }

    attr_reader :cwd, :global_scope, :file_scope
    attr_accessor :require_files

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
      @global_scope.native_types = BUILTIN_TYPES.dup

      BUILTIN_INFIX_OPERATORS.each do |op, (associativity, precedence)|
        @global_scope.define_infix_operator(op, associativity, precedence)
      end

      @file_scope = Scope.new(@global_scope)

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
      hoist_types_and_infixes(ast, @global_scope)
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

    def hoist_types_and_infixes(current_ast, scope)
      current_ast.children.each do |node|
        if node.term == :__require__
          _, body = node.children
          hoist_types_and_infixes(body, scope) unless body.nil?
        elsif node.term == :__module__
          name, body = node.children
          name = name.term

          raise Error.new("The module name '#{name}' must begin with a capital letter") unless ('A'..'Z').include?(name[0])

          module_scope = scope.fetch_module(name)
          hoist_types_and_infixes(body, module_scope)
        elsif node.term == :__typealias__
          begin
            typealias = node.children.first
            scope.define_typealias(typealias.term, typealias.type, typealias.meta && typealias.meta[:type_params] || [])
          rescue Scope::Error => e
            e.ast = typealias
            raise e
          end
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
          module_scope = scope.fetch_module(name.term)
          hoist_functions(body, module_scope)
        elsif node.term == :__fn__
          signature, body = node.children
          name = signature.term
          args_count = signature.children.count
          type_params = signature.meta && signature.meta[:type_params] || []

          fn_scope = Scope.new(scope)
          type_params.each { |type_param| fn_scope.define_type_param(type_param)}

          raise Error.new("Cannot override builtin operator '#{name}'") if BUILTIN_INFIX_OPERATORS.keys.include?(name)

          if signature.meta && signature.meta[:operator]
            if args_count == 2
              begin
                fn_scope.resolve_infix_operator(name)
              rescue Scope::Error => e
                e.ast = node
                raise e
              end
            elsif args_count != 1
              raise Error.new("The operator function '#{name}' must take only 1 or 2 arguments")
            end
          end

          # Can't use the same type parameter in the same generic fn
          type_params.combination(2).each do |t1, t2|
            raise Error.new("Type parameter '#{t1}' has already been defined for function '#{name}'") if t1 == t2
          end

          # Make sure that all the types referenced in the fn's args exist
          signature.children.each do |arg|
            begin
              arg.type = fn_scope.resolve_type(arg.type)
            rescue Scope::Error => e
              e.ast = arg
              raise e
            end
          end

          begin
            signature.type = fn_scope.resolve_type(signature.type)
          rescue Scope::Error => e
            e.ast = signature
            raise e
          end

          scope.define_function(name, signature.children.map { |arg| [ arg.term, arg.type ] }, signature.type, body, type_params: type_params.empty? ? nil : type_params)
        end
      end
    end

    def rewrite_exprs(node, scope)
      if node.term == :__expr__
        precedence_climb(node.children, scope)
      elsif node.term == :__module__
        name, body = node.children
        module_scope = scope.fetch_module(name.term)

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
          mod = lhs.meta && lhs.meta[:module_path] ? lhs.meta[:module_path] + [lhs] : [lhs]
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
          node.meta[:module_path] = mod
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
      when :__case__
        visit_case(node, scope)
      when :__embed__
        visit_embed(node, scope)
      when :__fn__
        # Since fn definitions can only exist within module definitions or the
        # top-level, the current scope will always be a module's or the top-level's
        # execution context, and the enclosing scope will be the module's scope or
        # the global scope. The fn is visited in the enclosing scope to isolate it
        # from the current's scope bindings, i.e. the isolated fn scope will only
        # have access to outer fn and module definitions, but not bound variables.
        visit_fn(node, scope.enclosing_scope)
      when :__infix__, :__typealias__
        node.type = Types::Simple.new(:Nil)
      when :'='
        visit_match(node, scope)
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

      # Visit children nodes in an "execution scope". This will keep variable
      # bindings directly inside the module isolated from fn and module definitions.
      visit(body, Scope.new(module_scope))

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

      fn_scope = Scope.new(scope)
      (signature.meta && signature.meta[:type_params] || []).each { |type_param| fn_scope.define_type_param(type_param) }
      signature.children.each { |arg| fn_scope.define(arg.term, arg.type) }

      visit(body, fn_scope)

      raise Error.new("Type mismatch; expected return type '#{return_type}' for function '#{name}' but found '#{body.type}'") if return_type != body.type

      node.type = Types::Simple.new(:Nil)
    end

    def visit_if(node, scope)
      conditional, then_block, else_block = node.children

      visit(conditional, scope)
      raise Error.new("If statements only accept boolean conditionals") unless conditional.type == Types::Simple.new(:Bool)

      branches = [then_block, else_block]
      branch_scopes = branches.map do |branch|
        branch_scope = Scope.new(scope)
        visit(branch, branch_scope)
        branch_scope
      end

      combined_bindings = branch_scopes.reduce({}) do |combined, scope|
        scope.binding.each { |k, v| (combined[k] ||= []) << v }
        combined
      end

      nil_bindings = []

      combined_bindings.each do |variable, bound_types|
        # If at least one branch didn't bind this variable, then the variable's
        # type once the if expr is complete is determined by the outer scope
        # if the variable was bound there, otherwise it's Nil
        if bound_types.count != branch_scopes.count
          if (type = scope.resolve_binding(variable))
            bound_types << type
          else
            nil_bindings << variable
            bound_types << Types::Simple.new(:Nil)
          end
        end

        scope.define(variable, Types.combine(bound_types))
      end

      node.meta ||= {}
      node.meta[:nil_bindings] = nil_bindings
      node.type = Types.combine(branches.map(&:type))
    end

=begin
x = 0

case foo
  a ->
    x = 1
    y = 2
  b ->
    y = 3
    z = 4
  c ->
    w = 5
end

a: (x, y)
  x = 1
  y = 2
  z = nil
  w = nil

b: (y, z)
  x = 0
  y = 3
  z = 4
  w = nil

c: (w)
  x = 0
  y = nil
  z = nil
  w = 5
=end

=begin
case x # x has type T := { r: (Int, { s: Int, msg: String } | Nil), v: String } | (Int, { s: Int, msg: String } )
  nil -> # Error, never matches because Nil is not a subtype of T
  "Hey" -> # Error, never matches because String is not a subtype of T
  (10, _) -> # Matches second type of union inexhaustively

end
=end

    # Pattern matching union type reduction
    # For each pattern, iterate over all types in a union type
    # If the pattern exhaustively matches one of the types, remove it from union for the remaining patterns
    #
    # Example: expression `e` has type `(Int, Int) | Int`
    #
    # case e
    #   (0, y) -> y     # Matches `(Int, Int)` only for a literal `0` in the first position
    #   (x, y) -> x + y # Matches all remaining `(Int, Int)` values, so `(Int, Int)` is removed from the union
    #   x      -> x     # The only remaining type is `Int`, so `x` will have type `Int` in the pattern body
    # end
    #
    # Example with reducing a deeply nested union:
    # expression `e` has type `{ a: (Int, { b: String } | Nil) }`
    #
    # case e
    #   { a: (0, { b: b }) } -> ... # `b : String`; matches type `{ b: String }` in the union only for literal `0`
    #   { a: (1, x) } -> ...        # `x : { b: String } | Nil`
    #   { a: (1, { b: b }) } -> ... # Useless clause; this branch will never be executed because the one
    #                               # before matches for all values this pattern matches. The compiler
    #                               # should throw an error here for this pattern to be removed.
    #   { a: (0, x) }        -> ... # `x : Nil`; `(0, { b: String })` has already been matched, so this must match only `(0, Nil)`
    #   { a: (_, nil)        -> ... # Matches all values in the 1st position, and `nil` in 2nd; therefore this clause
    #                               # completely reduces the type of the candidate to `{ a: (Int, { b: String }) }` for the
    #                               # remaining patterns.
    #   { a: a }             -> ... # `a : (Int, { b: String })`, since `Nil` has been removed by the previous pattern.
    # end
    #
    # Exhaustiveness checking:
    # Check each pattern against all the patterns before it using recursive usefulness algorithm
    # When reaching the wildcard case with a variable, the variable's type in the pattern body
    # will be bound to the union of all types that have incomplete constructors or literals.

    # TODO:
    # blocks = [then_block, else_block]
    # visit each block in new scope
    # build collected hash of { variables => [types] } of all scopes
    # foreach variable in hash, if at least one block does not have that variable bound:
    #   if some outer scope (use resolve_binding) has that variable bound, then add that type to the type array for that variable
    #   else add Nil type to the type array for that variable
    # foreach variable and type array, define the variable in this scope with combined types
    def visit_case(node, scope)
      subject, *patterns = node.children

      visit(subject, scope)

      branch_types = []

      # TODO: ~~Transpose the candidate types and patterns
      # If a type in the union fully matches a pattern, it doesn't need
      # to be checked against any other patterns.~~
      #
      # ^^^ Scratch that. If a type in the union fully matches a pattern,
      # remove it from the union and continue with the remaining types
      # for the rest of the patterns.
      branch_scopes = patterns.map do |pattern|
        head, branch = pattern.children
        head_bindings = check_case_pattern(head, subject.type)

        # A separate scope is used to isolate the variables bound by
        # the pattern head and the variables assigned to in the body.
        # The reason is that we want any variable assignments in the
        # body to exist in the outside scope as well (just like in
        # if expressions), but we want to keep the variables bound
        # in the head to be hygienic (they live only inside the body).
        # If a variable is bound in the pattern head, and then later
        # assigned to in the body, the variable will be available
        # outside the case; those two variables are different even
        # though they share the same name.
        head_scope = Scope.new(scope)
        head_bindings.each { |v, t| head_scope.define(v, t) }

        branch_scope = Scope.new(head_scope)

        visit(branch, branch_scope)
        branch_types << branch.type

        branch_scope
      end

      combined_bindings = branch_scopes.reduce({}) do |combined, scope|
        scope.binding.each { |k, v| (combined[k] ||= []) << v }
        combined
      end

      nil_bindings = []

      combined_bindings.each do |variable, bound_types|
        # If at least one branch didn't bind this variable, then the variable's
        # type once the if expr is complete is determined by the outer scope
        # if the variable was bound there, otherwise it's Nil
        if bound_types.count != branch_scopes.count
          if (type = scope.resolve_binding(variable))
            bound_types << type
          else
            nil_bindings << variable
            bound_types << Types::Simple.new(:Nil)
          end
        end

        scope.define(variable, Types.combine(bound_types))
      end

      node.meta ||= {}
      node.meta[:nil_bindings] = nil_bindings
      node.type = Types.combine(branch_types)
      byebug
    end

    def visit_match(node, scope)
      lhs, rhs = node.children
      visit(rhs, scope)

      pattern_match(lhs, rhs.type).each do |binding, type|
        scope.define(binding, type)
      end

      node.type = rhs.type
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
          Types::Generic.new(:List, [Types.nothing])
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
        if node.meta && node.meta[:module_path]
          force_scope = true

          begin
            scope.resolve_module_path(node.meta[:module_path].map(&:term))
          rescue Scope::Error => e
            e.ast = node
            raise e
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
              term_scope.resolve_function(node.term, arg_types, exclude_lambdas: node.meta && node.meta[:no_paren], infer_untyped_lambdas: true, force_scope: force_scope)
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
            node.type = term_scope.resolve_function(node.term, arg_types, exclude_lambdas: node.meta && node.meta[:no_paren], force_scope: force_scope)
          rescue Scope::Error => e
            e.ast = node
            raise e
          end
        end
      elsif node.type.nil? # Node is an identifier (variable or arity-0 function) if it doesn't have a type yet
        begin
          node.type  = term_scope.resolve(node.term, force_scope: force_scope)
        rescue Scope::Error => e
          e.ast = node
          raise e
        end
      end # Else the node is a literal, so do nothing
    end

    def visit_embed(node, scope)
      node.type = scope.resolve_type(node.type)
    end
  end
end
