module Mattlang
  class Semantic
    class Scope
      class Error < Semantic::Error
        def self.title; 'Scope Error' end
      end

      class NoMatchingFunction < Error; end

      attr_reader :parent_scope, :modules, :infix_operators, :functions, :binding, :type_params
      attr_accessor :native_types

      def initialize(parent_scope = nil, module_name: nil)
        @parent_scope = parent_scope
        @infix_operators = {}
        @functions = Hash.new { |h, k| h[k] = [] }
        @binding = {}
        @native_types = {}
        @type_params = {}
        @typedefs = {}
        @typealiases = {}
        @protocols = {}
        @modules = {}

        @module_name = module_name
      end

      def fetch_module(name)
        if (mod = @modules[name])
          mod
        elsif @protocols.key?(name)
          raise Error.new("You cannot name a module the same name as a protocol at this scope") if @modules.key?(name)
        else
          @modules[name] = Scope.new(self, module_name: name)
        end
      end

      def find_runtime_function(name, arg_types, force_scope: false)
        arg_types.each { |t| "Union type '#{t}' argument for function '#{name}' cannot be used for dispatch at runtime" if t.is_a?(Types::Union) }

        runtime_fn, type_bindings = find_functions(name, arg_types).first

        if runtime_fn
          [runtime_fn, type_bindings, self]
        else
          if @parent_scope && !force_scope
            @parent_scope.find_runtime_function(name, arg_types)
          else
            raise "No runtime function clause matches '#{name}' with #{arg_types.empty? ? 'no args' : 'arg types (' + arg_types.join(', ') + ')'}"
          end
        end
      end

      def find_functions(name, types, all_matches: false)
        fns = @functions.key?([name, types.size]) && @functions[[name, types.size]] || []

        compatible_fns = fns.map do |fn|
          type_bindings = fn.generic? ? fn.type_params.map { |t| [t.type_atom, Types.nothing] }.to_h : nil
          candidate_lambda_types = []

          is_match = fn.arg_types.zip(types).all? do |fn_type, type|
            if type.is_a?(Hash)
              if fn_type.anything?
                candidate_lambda_types << Types::Lambda.new([Types.nothing] * type[:arg_count], Types.anything)
                next true
              elsif fn_type.is_a?(Types::Lambda) && fn_type.args.size == type[:arg_count]
                candidate_lambda_types << fn_type
                next true
              else
                next false
              end
            else
              candidate_lambda_types << nil
            end

            if fn.generic?
              local_type_bindings = fn.type_params.map { |t| [t.type_atom, Types.nothing] }.to_h

              if fn_type.subtype?(type, local_type_bindings)
                local_type_bindings.all? do |type_param, bound_type|
                  if bound_type.nil?
                    true
                  elsif type_bindings[type_param].nil?
                    type_bindings[type_param] = bound_type
                    true
                  else
                    if type_bindings[type_param].subtype?(bound_type, nil, true)
                      true
                    elsif bound_type.subtype?(type_bindings[type_param], nil, true)
                      type_bindings[type_param] = bound_type
                      true
                    else
                      false
                    end
                  end
                end
              else
                false
              end
            else
              fn_type.subtype?(type)
            end
          end

          if is_match
            types.zip(candidate_lambda_types).each do |type, candidate_type|
              if type.is_a?(Hash)
                type[:candidate_types] << [candidate_type, type_bindings&.select { |k, v| !v.nil? }]
              end
            end

            if all_matches
              [fn, type_bindings]
            else
              return [[fn, type_bindings]]
            end
          end
        end.compact
      end

      # TODO: I think there is a bug here with resolving a generic function in the parent scope.
      # It might not bind the type parameters to the correct union type.
      #
      # For instance, this fails if `foo` is in the parent scope
      #
      # fn foo<T>(a: T, b: T) -> List<T>; [a, b] end
      #
      # x = if true; 5 else 5.0 end # Float | Int
      # y = if false; 6 else 6.0 end # Float | Int
      #
      # foo(x, y)
      # # Compilation error. Should this return List<Float | Int>? Or should it fail
      # since `x` could be Int and `y` could be Float, which would break the `T` constraint.
      # I'm leaning towards it returning List<Float | Int>, meaning `T` is bound to Float | Int.
      # I think that would be fine semantically, since `foo` can only send `a` and `b` to other
      # generic functions (and so on, recursively). And, since these generic functions can have
      # their type parameters bound to any type, it shouldn't matter what type is sent to them.
      # (This conclusion should hold until generic type contraints are implemented.)
      def resolve_function(name, arg_types, exclude_lambdas: false, infer_untyped_lambdas: false, force_scope: false)
        if !exclude_lambdas &&
          (binding = resolve_binding(name)) &&
          binding.is_a?(Types::Lambda) &&
          binding.args.size == arg_types.size &&
          arg_types.zip(binding.args).all? { |arg_type, lambda_arg| lambda_arg.subtype?(arg_type, nil, true) }
            return binding.return_type
        end

        first, *rest = arg_types.map { |arg_type| deconstruct_type(arg_type) }
        return_types = (first.nil? ? [[]] : first.product(*rest)).map do |types|
          found_fns = find_functions(name, types, all_matches: arg_types.any? { |t| t.is_a?(Hash) })

          if found_fns.any?
            found_fns.map do |compatible_fn, type_bindings|
              if type_bindings && !infer_untyped_lambdas
                compatible_fn.return_type.replace_type_bindings(type_bindings)
              else
                compatible_fn.return_type
              end
            end
          else
            if @parent_scope && !force_scope
              @parent_scope.resolve_function(name, types, exclude_lambdas: exclude_lambdas, infer_untyped_lambdas: infer_untyped_lambdas)
            else
              types_message =
                if types.empty?
                  'no args'
                else
                  'arg types (' + types.map do |type|
                    if type.is_a?(Hash)
                      if type[:arg_count] == 1
                        '? -> ?'
                      else
                        "(#{Array.new(type[:arg_count], '?').join(', ')}) -> ?"
                      end
                    else
                      type
                    end
                  end.join(', ') + ')'
                end

              raise Error.new("No function clause matches '#{name}' with #{types_message}")
            end
          end
        end.flatten

        Types.union(return_types)
      end

      def function_exists?(name, arg_count)
        @functions.key?([name, types.size]) || @parent_scope && @parent_scope.function_exists?(name, arg_count)
      end

      def define_infix_operator(operator, associativity, precedence)
        raise Error.new("The infix operator '#{operator}' has already been declared at this scope") if @infix_operators.key?(operator)

        @infix_operators[operator] = [associativity, precedence]
      end

      def define_function(fn)
        @functions[[fn.name, fn.args.size]] << fn
      end

      def define_type_param(type_param, all_type_params: [])
        raise Error.new("The type parameter '#{type_param.type_atom}' has already been defined at this scope") if @type_params.key?(type_param)

        # If this type parameter has a constraint, there could be references
        # inside the constraint to this or other type parameters in this list
        # of type parameters to define. So, we have to create a child scope
        # where the type parameter list is defined without constraints
        if type_param.constraint
          constraint_scope = Scope.new(self)
          all_type_params.each { |t| constraint_scope.define_type_param(Types::Simple.new(t, parameter_type: true)) }
          type_param = Types::Simple.new(type_param.type_atom, parameter_type: true, constraint: constraint_scope.resolve_type(type_param.constraint))
        end

        @type_params[type_param.type_atom] = type_param
      end

      def define_type(name, type, type_params)
        raise Error.new("The type '#{name}' has already been defined at this scope by a typealias") if @typealiases.key?(name)
        raise Error.new("The type '#{name}' has already been defined at this scope") if @typedefs.key?(name)
        raise Error.new("A protocol named '#{name}' has already been defined at this scope") if @protocols.key?(name)
        raise Error.new("Type parameters must be used only once") if type_params && type_params.uniq.count != type_params.count

        @typedefs[name] = [type, type_params || []]
      end

      def define_typealias(name, type, type_params)
        raise Error.new("The type '#{name}' has already been defined at this scope by a typealias") if @typealiases.key?(name)
        raise Error.new("The type '#{name}' has already been defined at this scope") if @typedefs.key?(name)
        raise Error.new("A protocol named '#{name}' has already been defined at this scope") if @protocols.key?(name)
        raise Error.new("Type parameters must be used only once") if type_params && type_params.uniq.count != type_params.count

        @typealiases[name] = [type, type_params || []]
      end

      def define_protocol(name, type_params)
        raise Error.new("You cannot name a protocol the same name as a module at this scope") if @modules.key?(name)
        raise Error.new("The type '#{name}' has already been defined at this scope by a typealias") if @typealiases.key?(name)
        raise Error.new("The type '#{name}' has already been defined at this scope") if @typedefs.key?(name)
        raise Error.new("The protocol '#{name}' has already been defined at this scope") if @protocols.key?(name)
        raise Error.new("Type parameters must be used only once") if type_params && type_params.uniq.count != type_params.count

        @protocols[name] = Protocol.new(name, type_params, self, module_path: module_path)
      end

      def define(name, type)
        @binding[name] = type
      end

      def resolve(name, force_scope: false)
        if (variable_type = resolve_binding(name))
          variable_type
        else
          begin
            resolve_function(name, [], force_scope: force_scope)
          rescue Error
            raise Error.new("Undefined function or local variable '#{name}'")
          end
        end
      end

      def resolve_binding(name)
        if @binding.key?(name)
          @binding[name]
        elsif @parent_scope
          @parent_scope.resolve_binding(name)
        else
          nil
        end
      end

      def resolve_module(name, force_scope: false)
        if (mod = @modules[name])
          mod
        elsif @parent_scope && !force_scope
          @parent_scope.resolve_module(name)
        else
          raise Error.new("Undefined module '#{name}'")
        end
      end

      def resolve_module_path(path)
        return self if path.nil? || path.empty?
        return (resolve_protocol(path.first) rescue nil) || resolve_module(path.first) if path.count == 1

        outermost_module_scope = resolve_module(path.first)
        second_innermost = path[1..-2].reduce(outermost_module_scope) { |scope, mod| scope.resolve_module(mod, force_scope: true) }

        (second_innermost.resolve_protocol(path.last) rescue nil) || second_innermost.resolve_module(path.last)
      end

      def resolve_infix_operator(name)
        if (op = @infix_operators[name])
          op
        elsif @parent_scope
          @parent_scope.resolve_infix_operator(name)
        else
          raise Error.new("Undefined infix operator '#{name}'")
        end
      end

      def resolve_typedef(type_atom, argument_type, original_scope = self, force_scope: false)
        if (typedef = @typedefs[type_atom])
          underlying_type = typedef[0] && resolve_type(typedef[0])
          bound_type_params = typedef[1].map { |t| [t.type_atom, Types.nothing] }.to_h

          return if argument_type == :check

          if underlying_type.nil?
            if !argument_type.nil?
              raise Error.new("Type constructor '#{type_atom}' takes no arguments but was passed the type '#{argument_type}'")
            end
          else
            raise Error.new("Type constructor '#{type_atom}' expects an argument but was passed nothing") if argument_type.nil?

            if !underlying_type.subtype?(argument_type, bound_type_params, true)
              raise Error.new("Argument type '#{argument_type}' is incompatible with type constructor '#{type_atom} = #{underlying_type}'")
            end
          end

          Types::Nominal.new(type_atom, typedef[1].map { |t| bound_type_params[t.type_atom] }, underlying_type&.replace_type_bindings(bound_type_params), module_path: module_path)
        elsif @parent_scope && !force_scope
          @parent_scope.resolve_typedef(type_atom, argument_type, original_scope)
        else
          type_prefix = module_path.empty? ? '' : module_path.join('.') + '.'
          raise Error.new("Unknown type '#{type_prefix}#{type_atom}'")
        end
      end

      def resolve_protocol(name, force_scope: false)
        type_params ||= []

        if (protocol = @protocols[name])
          protocol
        elsif @parent_scope && !force_scope
          @parent_scope.resolve_protocol(name)
        else
          raise Error.new("Unknown protocol '#{name}'")
        end
      end

      def resolve_type(type, original_scope = self, ignore_module_path: false, previous_typedefs: [])
        case type
        when Types::Tuple
          Types::Tuple.new(type.types.map { |t| original_scope.resolve_type(t, previous_typedefs: previous_typedefs) })
        when Types::Record
          Types::Record.new(type.types_hash.map { |k, t| [k, original_scope.resolve_type(t, previous_typedefs: previous_typedefs)] }.to_h)
        when Types::Lambda
          Types::Lambda.new(type.args.map { |t| original_scope.resolve_type(t, previous_typedefs: previous_typedefs) }, original_scope.resolve_type(type.return_type, previous_typedefs: previous_typedefs))
        when Types::Union
          Types.union(type.types.map { |t| original_scope.resolve_type(t, previous_typedefs: previous_typedefs) })
        when Types::Intersection
          Types.intersect(type.types.map { |t| original_scope.resolve_type(t, previous_typedefs: previous_typedefs) })
        when Types::Simple, Types::Generic, Types::Nominal
          if !ignore_module_path && !type.module_path.empty?
            resolve_module_path(type.module_path).resolve_type(type, original_scope, ignore_module_path: true, previous_typedefs: previous_typedefs)
          elsif (type_param = @type_params[type.type_atom])
            raise Error.new("Type parameter '#{type.type_atom}' is not a generic type") if type.is_a?(Types::Generic)
            type_param
          elsif (typealias = @typealiases[type.type_atom])
            aliased_type, aliased_type_params = typealias

            if type.is_a?(Types::Simple)
              raise Error.new("Generic type '#{type.type_atom}' is missing type parameters") if aliased_type_params.count != 0
              resolve_type(aliased_type, previous_typedefs: previous_typedefs)
            else
              raise Error.new("Generic type '#{type.type_atom}' requires #{aliased_type_params.count} type parameter#{'s' if aliased_type_params.count > 1}, but was given #{type.type_parameters.count}") if aliased_type_params.count != type.type_parameters.count
              resolved_type_parameters = type.type_parameters.map { |t| original_scope.resolve_type(t, previous_typedefs: previous_typedefs) }
              resolve_type(aliased_type.replace_type_bindings(aliased_type_params.map(&:type_atom).zip(resolved_type_parameters).to_h), previous_typedefs: previous_typedefs)
            end
          elsif (typedef = @typedefs[type.type_atom])
            underlying_type, typedef_params = typedef

            type_cycle =
              previous_typedefs.any? do |previous_type_atom, previous_scope|
                previous_type_atom == type.type_atom && previous_scope == self
              end

            if type.is_a?(Types::Simple)
              raise Error.new("Generic type '#{type.type_atom}' is missing type parameters") if typedef_params.count != 0
              resolved_underlying_type = type_cycle || underlying_type.nil? ? nil : resolve_type(underlying_type, previous_typedefs: previous_typedefs + [[type.type_atom, self]])
              Types::Nominal.new(type.type_atom, [], resolved_underlying_type, module_path: module_path)
            else
              raise Error.new("Generic type '#{type.type_atom}' requires #{typedef_params.count} type parameter#{'s' if typedef_params.count != 1}, but was given #{type.type_parameters.count}") if typedef_params.count != type.type_parameters.count
              resolved_type_parameters = type.type_parameters.map { |t| original_scope.resolve_type(t, previous_typedefs: previous_typedefs + [[type.type_atom, self]]) }
              resolved_underlying_type = type_cycle || underlying_type.nil? ? nil : resolve_type(underlying_type.replace_type_bindings(typedef_params.map(&:type_atom).zip(resolved_type_parameters).to_h), previous_typedefs: previous_typedefs + [[type.type_atom, self]])
              Types::Nominal.new(type.type_atom, resolved_type_parameters, resolved_underlying_type, module_path: module_path)
            end
          elsif (protocol = @protocols[type.type_atom])
            if type.is_a?(Types::Simple)
              Types::Simple.new(type.type_atom, module_path: type.module_path, protocol: protocol)
            else
              protocol_type_params = type.type_parameters.map! { |t| original_scope.resolve_type(t, previous_typedefs: previous_typedefs) }
              raise Error.new("Protocol type '#{type.type_atom}' requires #{protocol_type_params.count} type parameter#{'s' if protocol_type_params.count != 1}, but was given #{type.type_parameters.count}") if protocol_type_params.count != type.type_parameters.count
              Types::Generic.new(type.type_atom, protocol_type_params, module_path: type.module_path, protocol: protocol)
            end
          elsif type.module_path.empty? && (native_type_params_count = @native_types[type.type_atom])
            if type.is_a?(Types::Simple)
              raise Error.new("Generic type '#{type.type_atom}' is missing type parameters") if native_type_params_count != 0
              type
            else
              raise Error.new("Generic type '#{type.type_atom}' requires #{native_type_params_count} type parameter#{'s' if native_type_params_count != 1}, but was given #{type.type_parameters.count}") if native_type_params_count != type.type_parameters.count
              Types::Generic.new(type.type_atom, type.type_parameters.map { |t| original_scope.resolve_type(t) })
            end
          elsif type.is_a?(Types::Simple) && type.parameter_type?
            resolved_constraint = type.constraint.nil? ? nil : original_scope.resolve_type(t.constraint)
            Types::Simple.new(type.type_atom, parameter_type: true, constraint: resolved_constraint)
          elsif @parent_scope
            @parent_scope.resolve_type(type, original_scope)
          else
            type_str = type.is_a?(Types::Simple) ? type.to_s : Types::Simple.new(type.type_atom, module_path: type.module_path).to_s
            raise Error.new("Unknown type '#{type_str}'")
          end
        else
          raise "Cannot resolve type with class '#{type.class}'"
        end
      end

      def bound_types
        (@parent_scope&.bound_types || {}).merge(@type_params.map { |t, _| [t, Types::Simple.new(t, parameter_type: true)] }.to_h)
      end

      def module_path
        @module_path ||= (@parent_scope&.module_path || []) + (@module_name.nil? ? [] : [@module_name])
      end

      private

      # Deconstructs union types into an array of simple or simple generic types
      def deconstruct_type(type)
        case type
        when Types::Union
          type.types.map { |t| deconstruct_type(t) }.flatten
        else
          [type]
        end
      end
    end
  end
end
