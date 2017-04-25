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

require 'mattlang/semantic/function'
require 'mattlang/semantic/protocol'
require 'mattlang/semantic/scope'
require 'mattlang/semantic/pattern_matcher'

module Mattlang
  class Semantic
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
      check_typedefs_and_protocols(ast, @global_scope)
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
          name_node, body = node.children
          name = name_node.term

          raise Error.new("The module name '#{name}' must begin with an uppercase letter") unless ('A'..'Z').include?(name[0])

          begin
            module_scope = scope.fetch_module(name)
          rescue Scope::Error => e
            e.ast = name_node
            raise e
          end

          hoist_types_and_infixes(body, module_scope)
        elsif node.term == :__type__
          begin
            typedef = node.children.first
            scope.define_type(typedef.term, typedef.type, typedef.meta && typedef.meta[:type_params])
          rescue Scope::Error => e
            e.ast = typedef
            raise e
          end
        elsif node.term == :__typealias__
          begin
            typealias = node.children.first
            scope.define_typealias(typealias.term, typealias.type, typealias.meta && typealias.meta[:type_params])
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
        elsif node.term == :__protocol__
          header, fn_nodes = node.children
          type_params = header.meta && header.meta[:type_params] || []

          fns = fn_nodes.children.map do |fn|
            fn_type_params = fn.meta && fn.meta[:type_params] || []
            fn_type_params.each { |t| raise Error.new("Type parameter '#{t}' in protocol fn cannot shadow the type parameter declared in protocol header", fn) if type_params.include?(t) }
          end

          begin
            protocol = scope.define_protocol(header.term, type_params)
          rescue Scope::Error => e
            e.ast = header
            raise e
          end

          node.meta ||= {}
          node.meta[:protocol] = protocol
        end
      end
    end

    def check_typedefs_and_protocols(current_ast, scope)
      current_ast.children.each do |node|
        if node.term == :__require__
          _, body = node.children
          check_typedefs_and_protocols(body, scope) unless body.nil?
        elsif node.term == :__module__
          name, body = node.children
          module_scope = scope.fetch_module(name.term)
          check_typedefs_and_protocols(body, module_scope)
        elsif node.term == :__type__
          typedef = node.children.first

          begin
            scope.resolve_typedef(typedef.term, :check)
          rescue Scope::Error => e
            e.ast = node
            raise e
          end
        elsif node.term == :__typealias__
          typealias = node.children.first

          type =
            if typealias.meta && typealias.meta[:type_params]
              Types::Generic.new(typealias.term, typealias.meta[:type_params].map { Types.nothing })
            else
              Types::Simple.new(typealias.term)
            end

          begin
            scope.resolve_type(type)
          rescue SystemStackError
            # There's probably a better way to check for type recursion
            raise Error.new("Typealiases cannot be mutually recursive; use `type` instead to create a new type", node)
          end
        elsif node.term == :__protocol__
          header, fn_nodes = node.children
          type_params = header.meta && header.meta[:type_params] || []
          protocol_type = type_params.empty? ? Types::Simple.new(header.term) : Types::Generic.new(header.term, type_params.map { |t| Types::Simple.new(t.type_atom) })

          protocol = node.meta[:protocol]

          protocol_scope = Scope.new(scope)
          type_params.each { |t| protocol_scope.define_type_param(t) }

          fn_nodes.children.each do |fn|
            protocol_found = false

            fn.children.each do |arg|
              arg.type.matching_types do |type|
                matching_type =
                  if type.is_a?(Types::Simple)
                    type.type_atom == header.term
                  elsif type.is_a?(Types::Generic)
                    type.type_atom == header.term && type.type_parameters.count == type_params.count && type.type_parameters == protocol_type.type_parameters
                  end

                if matching_type
                  if protocol_found
                    raise Error.new("Each protocol fn must have only one reference of the protocol type '#{protocol_type.to_s}' among its args", fn) if protocol_found
                  end

                  protocol_found = true
                end
              end
            end

            raise Error.new("Each protocol fn must reference the protocol type '#{protocol_type.to_s}' one time somewhere in its args", fn) if !protocol_found

            fn_scope = Scope.new(protocol_scope)

            fn.children.each do |arg|
              begin
                arg.type = fn_scope.resolve_type(arg.type)
              rescue Scope::Error => e
                e.ast = arg
                raise e
              end
            end

            begin
              fn.type = fn_scope.resolve_type(fn.type)
            rescue Scope::Error => e
              e.ast = header
              raise e
            end

            fn_type_params = fn.meta && fn.meta[:type_params] || []
            protocol.define_function(Function.new(fn.term, fn.children.map { |arg| [arg.term, arg.type] }, fn.type, nil, type_params: fn_type_params + type_params))
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
        elsif node.term == :__impl__
          for_node, fn_nodes = node.children
          impl_type_params = node.meta && node.meta[:type_params] || []
          protocol_type = node.type
          for_type = for_node.type

          impl_protocol_type_params = protocol_type.is_a?(Types::Generic) ? protocol_type.type_parameters : []

          begin
            protocol = scope.resolve_protocol(protocol_type.type_atom)
          rescue Scope::Error => e
            e.ast = node
            raise e
          end

          raise Error.new("Protocol '#{protocol.name}' expects #{protocol.type_params.count} type param#{'s' if protocol.type_params.count != 1} but was referenced with #{impl_protocol_type_params.count}", node) if protocol.type_params.count != impl_protocol_type_params.count

          impl_scope = Scope.new(scope)
          impl_type_params.each { |t| impl_scope.define_type_param(t) }
          impl_simple_type_params = impl_type_params.map { |t| [t.type_atom, Types::Simple.new(t.type_atom)] }.to_h

          begin
            associated_types = protocol.type_params.map(&:type_atom).zip(impl_protocol_type_params).map do |ptp, iptp|
              [ptp, impl_scope.resolve_type(iptp).replace_type_bindings(impl_simple_type_params)]
            end.to_h
          rescue Scope::Error => e
            e.ast = node
            raise e
          end

          begin
            resolved_for_type = impl_scope.resolve_type(for_type)
          rescue Scope::Error => e
            e.ast = for_node
            raise e
          end

          fn_nodes.children.map do |fn|
            fn_scope = Scope.new(impl_scope)
            signature, body = fn.children

            fn_type_params = signature.meta && signature.meta[:type_params] || []
            fn_type_params.each { |t| raise Error.new("Type parameter '#{t}' in impl fn cannot shadow the type parameter declared in impl header", fn) if impl_type_params.include?(t) }

            args = signature.children.map do |arg|
              begin
                arg.type = fn_scope.resolve_type(arg.type)
                [arg.term, arg.type]
              rescue Scope::Error => e
                e.ast = arg
                raise e
              end
            end

            return_type =
              begin
                signature.type = fn_scope.resolve_type(signature.type)
              rescue Scope::Error => e
                e.ast = signature
                raise e
              end

            impl_scope.define_function(Function.new(signature.term, args, return_type, body, type_params: fn_type_params + impl_type_params))
          end

          protocol_scope = Scope.new(scope)
          protocol_scope.define_typealias(protocol_type.type_atom, resolved_for_type, protocol.type_params)
          protocol.type_params.each { |t| protocol_scope.define_type_param(t) }

          protocol.functions.values.flatten.each do |required_fn|
            implemented = true

            fn_simple_type_params = required_fn.type_params.map { |t| [t.type_atom, Types::Simple.new(t.type_atom)] }.to_h
            args = required_fn.args.map do |_, t|
              protocol_scope
                .resolve_type(t)
                .replace_type_bindings(associated_types)
                .replace_type_bindings(impl_simple_type_params)
                .replace_type_bindings(fn_simple_type_params)
            end

            begin
              return_type = impl_scope.resolve_function(required_fn.name, args, exclude_lambdas: true, force_scope: true)
              implemented = false unless protocol_scope.resolve_type(required_fn.return_type).subtype?(return_type)
            rescue Scope::Error => e
              implemented = false
            end

            raise Error.new("The implementation of protocol '#{protocol_type}' for '#{for_type}' does not provide a proper '#{required_fn.name}' fn", node) if !implemented
          end

          protocol.define_impl(resolved_for_type, associated_types, impl_scope)

          node.meta ||= {}
          node.meta[:impl_scope] = impl_scope
        elsif node.term == :__fn__
          signature, body = node.children
          name = signature.term
          args_count = signature.children.count
          type_params = signature.meta && signature.meta[:type_params] || []

          fn_scope = Scope.new(scope)

          begin
            type_params.each { |type_param| fn_scope.define_type_param(type_param, all_type_params: type_params.map(&:type_atom))}
          rescue Scope::Error => e
            e.ast = signature
            raise e
          end

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
          else
            raise Error.new("The function name '#{name}' cannot begin with an uppercase letter") if ('A'..'Z').include?(name[0])
          end

          # Can't use the same type parameter in the same generic fn
          type_params.combination(2).each do |t1, t2|
            raise Error.new("Type parameter '#{t1}' has already been declared for function '#{name}'") if t1 == t2
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

          scope.define_function(Function.new(name, signature.children.map { |arg| [arg.term, arg.type] }, signature.type, body, type_params: type_params))
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
          mod = lhs.meta && lhs.meta[:module_path] ? lhs.meta[:module_path] + [lhs.term] : [lhs.term]
          attach_module(mod, rhs)
          rhs
        elsif rhs.children.nil?
          AST.new(node.term, [lhs, rhs], meta: node.meta, token: node.token)
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
        # execution context, and the parent scope will be the module's scope or
        # the global scope. The fn is visited in the parent scope to isolate it
        # from the current scope's bindings, i.e. the isolated fn scope will only
        # have access to outer fn and module definitions, but not bound variables.
        visit_fn(node, scope.parent_scope)
      when :__impl__
        visit_impl(node, scope)
      when :__infix__, :__type__, :__typealias__, :__protocol__
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
      when :__record_update__
        visit_record_update(node, scope)
      when :__lambda__
        visit_lambda(node, scope)
      else
        raise "Unknown term '#{node.term}'" if node.term.is_a?(Symbol) && node.term.to_s.start_with?('__')

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

    def visit_fn(node, scope, extra_type_params: [])
      signature, body = node.children
      name = signature.term
      return_type = signature.type

      fn_scope = Scope.new(scope)
      all_type_params = (signature.meta && signature.meta[:type_params] || []).concat(extra_type_params)
      all_type_params.each { |type_param| fn_scope.define_type_param(type_param, all_type_params: all_type_params.map(&:type_atom)) }
      signature.children.each { |arg| fn_scope.define(arg.term, arg.type) }

      visit(body, fn_scope)

      raise Error.new("Type mismatch; expected return type '#{return_type}' for function '#{name}' but found '#{body.type}'", node) if !return_type.subtype?(body.type, nil, true)

      node.type = Types::Simple.new(:Nil)
    end

    def visit_impl(node, scope)
      for_type, fns = node.children
      impl_type_params = node.meta && node.meta[:type_params] || []
      impl_scope = node.meta[:impl_scope]

      fns.children.each do |fn|
        visit_fn(fn, impl_scope, extra_type_params: impl_type_params)
      end

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

        scope.define(variable, Types.union(bound_types))
      end

      node.meta ||= {}
      node.meta[:nil_bindings] = nil_bindings
      node.type = Types.union(branches.map(&:type))
    end

    def visit_case(node, scope)
      subject, patterns_node = node.children

      visit(subject, scope)
      pattern_heads = patterns_node.children.map { |node| node.children.first }

      begin
        patterns = PatternMatcher.new.generate_case_patterns(pattern_heads, subject.type)
      rescue PatternMatcher::MissingPatternsError => e
        e.ast = node
        raise e
      end

      branch_types = []

      branch_scopes = patterns_node.children.zip(patterns).map do |pattern_node, pattern|
        _head, branch = pattern_node.children

        # A separate scope is used to isolate the variables bound by
        # the pattern head and the variables bound in the body.
        # The reason is that we want any variable assignments in the
        # body to exist in the outside scope as well (just like in
        # if expressions), but we want to keep the variables bound
        # in the head to be hygienic (they live only inside the body).
        # If a variable is bound in the pattern head, and then later
        # assigned to in the body, the variable will be available
        # outside the case; those two variables are different even
        # though they share the same name.
        head_scope = Scope.new(scope)
        pattern.bindings.each { |v, t| head_scope.define(v, t) }

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
        # type once the case expr is complete is determined by the outer scope
        # if the variable was bound there, otherwise it's Nil
        if bound_types.count != branch_scopes.count
          if (type = scope.resolve_binding(variable))
            bound_types << type
          else
            nil_bindings << variable
            bound_types << Types::Simple.new(:Nil)
          end
        end

        scope.define(variable, Types.union(bound_types))
      end

      node.meta ||= {}
      node.meta[:nil_bindings] = nil_bindings
      node.type = Types.union(branch_types)
    end

    def visit_match(node, scope)
      lhs, rhs = node.children
      visit(rhs, scope)

      PatternMatcher.new.destructure_match(lhs, rhs.type).each do |binding, type|
        scope.define(binding, type)
      end

      node.type = rhs.type
    end

    def visit_access(node, scope)
      lhs, rhs = node.children

      visit(lhs, scope)

      if (index = rhs.term.to_s.to_i).to_s == rhs.term.to_s # Tuple index access
        subject_types = lhs.type.matching_types do |type|
          if type.is_a?(Types::Nominal) || type.is_a?(Types::Union)
            false
          elsif type.is_a?(Types::Tuple) && type.types.count >= index + 1
            true
          else
            message =
              if type == lhs.type
                "Cannot access tuple index #{index} on the type '#{type}'"
              else
                "Cannot access tuple index #{index} on the type '#{type}' found nested in type '#{lhs.type}'"
              end

            raise Error.new(message, rhs)
          end
        end

        node.type = Types.union(subject_types.map { |t| t.types[index] })
      else # Record field access
        field = rhs.term

        subject_types = lhs.type.matching_types do |type|
          if type.is_a?(Types::Nominal) || type.is_a?(Types::Union)
            false
          elsif type.is_a?(Types::Record) && type.types_hash.key?(field)
            true
          else
            message =
              if type == lhs.type
                "Cannot access record field '#{field}' on the type '#{type}'"
              else
                "Cannot access record field '#{field}' on the type '#{type}' found nested in type '#{lhs.type}'"
              end

            raise Error.new(message, rhs)
          end
        end

        node.type = Types.union(subject_types.map { |t| t.types_hash[field] })
      end
    end

    def visit_list(node, scope)
      node.children.each { |c| visit(c, scope) }

      node.type =
        if node.children.empty?
          Types::Generic.new(:List, [Types.nothing])
        else
          Types::Generic.new(:List, [Types.union(node.children.map(&:type))])
        end
    end

    def visit_tuple(node, scope)
      node.children.each { |c| visit(c, scope) }
      node.type = Types::Tuple.new(node.children.map(&:type))
    end

    def visit_record(node, scope)
      types_hash = node.children.map do |field_node|
        expr_node = field_node.children.first
        visit(expr_node, scope)
        field_node.type = expr_node.type

        [field_node.term, field_node.type]
      end.to_h

      node.type = Types::Record.new(types_hash)
    end

    def visit_record_update(node, scope)
      subject, fields = node.children

      subject.type = scope.resolve(subject.term)

      subject.type.matching_types do |type|
        if type.is_a?(Types::Union)
          false
        elsif type.is_a?(Types::Nominal) && !type.underlying_type.nil?
          false
        elsif type.is_a?(Types::Record)
          fields.children.each do |field|
            if !type.types_hash.key?(field.term)
              message =
                if type == subject.type
                  "Cannot update the field '#{field.term}' missing in type '#{type}'"
                else
                  "Cannot update the field '#{field.term}' missing in type '#{type}' found nested in type '#{subject.type}'"
                end

              raise Error.new(message, subject)
            end
          end

          true
        else
          message =
            if type == subject.type
              "Cannot perform a record copy on the non-record type '#{type}'"
            else
              "Cannot perform a record copy on the non-record type '#{type}' found nested in type '#{subject.type}'"
            end

          raise Error.new(message, subject)
        end
      end

      updated_types_hash = fields.children.map do |field_node|
        expr_node = field_node.children.first
        visit(expr_node, scope)
        field_node.type = expr_node.type

        [field_node.term, field_node.type]
      end.to_h

      # At this point, subject.type is guaranteed to be a record type,
      # a nominal type with an underlying record type, a union of any
      # of those types, or some deep nesting of nominals and unions of
      # record types. If a nominal type appears, the underlying type
      # must be compatible with the updated field types. Otherwise,
      # the nominal's constructor would no longer accept the updated
      # record type.
      subject.type.matching_types do |type|
        if type.is_a?(Types::Nominal)
          typedef_scope =
            if type.module_path.empty?
              @global_scope
            else
              scope.resolve_module_path(type.module_path)
            end

          type.underlying_type.deunion.each do |inner_type|
            if inner_type.is_a?(Types::Record)
              updated_record_type = Types::Record.new(inner_type.types_hash.merge(updated_types_hash))

              begin
                typedef_scope.resolve_typedef(type.type_atom, updated_record_type, force_scope: true)
              rescue Scope::IncompatibleArgument => e
                raise Error.new("Cannot perform this record update with possible resultant type '#{updated_record_type}' incompatible with type '#{type} = #{e.underlying_type}'", node)
              end
            end
          end
        end
      end

      node.type = subject.type.deep_record_update(updated_types_hash, scope)
    end

    def visit_lambda(node, scope)
      args, body = node.children

      bound_types = scope.bound_types
      arg_types =
        if bound_types.empty?
          args.children.map do |c|
            raise Error.new("Missing parameter type in lambda", c) if c.type.nil?
            scope.resolve_type(c.type)
          end
        else
          args.children.map do |c|
            raise Error.new("Missing parameter type in lambda", c) if c.type.nil?
            scope.resolve_type(c.type).replace_type_bindings(bound_types)
          end
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
            scope.resolve_module_path(node.meta[:module_path])
          rescue Scope::Error => e
            e.ast = node
            raise e
          end
        else
          scope
        end

      if node.term.is_a?(Symbol) && ('A'..'Z').include?(node.term[0]) # Type constructor
        raise Error.new("Type constructors do not accept more than one argument without parentheses", node) if node.children && node.children.count > 1 && node.meta && node.meta[:no_paren]

        node.children&.each { |c| visit(c, scope) }

        argument_type =
          if node.children.nil? || node.children.count == 0
            nil
          elsif node.children.count > 1
            Types::Tuple.new(node.children.map(&:type))
          else
            node.children.first.type
          end

        begin
          node.type = term_scope.resolve_typedef(node.term, argument_type, force_scope: force_scope)
        rescue Scope::Error => e
          e.ast = node
          raise e
        end
      elsif !node.children.nil? # Node is a function or lambda call if it has children (even if it is an empty array)
        if node.term.is_a?(AST)
          visit(node.term, scope)

          raise Error.new("Invalid lambda call") if !node.term.type.is_a?(Types::Lambda)
          raise Error.new("Lambdas must be called with parens") if node.meta && node.meta[:no_paren]

          node.children.each { |c| visit(c, scope) }

          if node.term.type.args.size == node.children.size && node.children.map(&:type).zip(node.term.type.args).all? { |arg_type, lambda_arg| arg_type.subtype?(lambda_arg, nil, true) }
            node.type = node.term.type.return_type
          else
            raise Error.new("Lambda expected (#{node.term.type.args.join(', ')}) but was called with (#{node.children.map(&:type).join(', ')})", node)
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
              if !c.is_a?(Hash)
                c
              else c.is_a?(Hash)
                type_checked_lambda = nil

                candidates = c[:candidate_types].map do |lambda_type, type_bindings|
                  original_lambda_type = lambda_type
                  lambda_type = lambda_type.replace_type_bindings(type_bindings) if type_bindings

                  duped_lambda = c[:node].dup
                  duped_lambda.children.first.children.each_with_index { |arg, i| arg.type = lambda_type.args[i] }

                  # Use the lambda's inferred arg types to infer the lambda's return type,
                  # which is set by visiting (and type checking) the lambda's expressions
                  begin
                    visit(duped_lambda, scope)
                  rescue => e
                    puts "Warning during lambda type inference: #{e}"
                    next nil
                  end

                  # The inferred return type may give us more information about type parameters
                  # used in the lambda's arg types. Here, we try to unify type parameters bound
                  # in the inferred return type with type parameters in the original arg types
                  # used to infer that return type. This may give us different arg types than
                  # before (if new_type_bindings != type_bindings). So, use these new arg types
                  # to re-infer a new return type (and repeat), iteratively finding more general
                  # types to bind to the type parameters until either no changes are made in the
                  # type bindings or the lambda no longer type checks.
                  loop do
                    new_type_bindings = type_bindings&.dup
                    original_lambda_type.subtype?(duped_lambda.type, new_type_bindings)

                    break if new_type_bindings == type_bindings
                    type_bindings = new_type_bindings

                    lambda_type = original_lambda_type.replace_type_bindings(type_bindings)

                    new_duped_lambda = c[:node].dup
                    new_duped_lambda.children.first.children.each_with_index { |arg, i| arg.type = lambda_type.args[i] }

                    begin
                      visit(new_duped_lambda, scope)
                    rescue
                      break
                    end

                    duped_lambda = new_duped_lambda
                  end

                  type_checked_lambda = duped_lambda
                  duped_lambda.type
                end.compact

                if candidates.size == 0
                  raise Error.new("No type could be inferred for this lambda argument", c[:node])
                elsif candidates.size > 1
                  raise Error.new("Ambiguous types inferred for this lambda argument", c[:node])
                else
                  c[:node].children = type_checked_lambda.children
                  c[:node].type = type_checked_lambda.type
                  candidates.first
                end
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
          node.type = term_scope.resolve(node.term, force_scope: force_scope)
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
