module Mattlang
  class Semantic
    class Protocol
      attr_reader :name, :module_path, :type_params, :impls, :parent_scope

      def initialize(name, type_params, parent_scope, module_path: [])
        @name = name
        @module_path = module_path
        @type_params = type_params

        @protocol_scope = Scope.new(parent_scope)
        @impls = []
      end

      def functions
        @protocol_scope.functions
      end

      def define_function(fn)
        @protocol_scope.define_function(fn)
      end

      def define_impl(for_type, associated_types, impl_scope)
        @impls << [for_type, associated_types, impl_scope]
      end

      def implemented_by?(type, referenced_type_params, type_bindings)
        impls.each do |impl_type, associated_types, impl_scope|
          impl_type_bindings = impl_scope.type_params.keys.map { |t| [t, Types.nothing] }.to_h

          if impl_type.subtype?(type, impl_type_bindings)
            remapped_type_params = @type_params.map { |t| associated_types[t.type_atom].replace_type_bindings(impl_type_bindings) }

            return true if referenced_type_params.zip(remapped_type_params).all? { |proto_tp, impl_tp| proto_tp.subtype?(impl_tp, type_bindings, true) }
          end
        end

        false
      end

      def find_runtime_function(fn_name, arg_types)
        arg_types.each { |t| raise "Union type '#{t}' argument for function '#{name}' cannot be used for dispatch at runtime" if t.is_a?(Types::Union) }

        functions[[fn_name, arg_types.count]].each do |fn|
          impl_type, assoc_types, _ = extract_impl_type(fn, arg_types)
          next if impl_type.nothing?
          raise "Union type '#{impl_type}' cannot be used for protocol dispatch at runtime" if impl_type.is_a?(Types::Union)

          _, _, matching_impl_scope = impls.find do |for_type, associated_types, impl_scope|
            for_type.subtype?(impl_type, impl_scope.type_params.keys.map { |t| [t, Types.nothing] }.to_h) &&
              associated_types.all? { |k, t| t.subtype?(assoc_types[k]) }
          end

          raise "At runtime, no implementation could be found of protocol '#{module_prefix}#{@name}' for type '#{impl_type}'" if matching_impl_scope.nil?

          return matching_impl_scope.find_runtime_function(fn_name, arg_types, force_scope: true)
        end

        raise "At runtime, the implementing type could not be determined for protocol '#{@name}'"
      end

      def resolve_function(fn_name, arg_types, exclude_lambdas: true, infer_untyped_lambdas: false, force_scope: false)
        if (fns = functions[[fn_name, arg_types.count]])
          fns.each do |fn|
            extracted_impl_type, extracted_associated_types, parameterized_arg_types = extract_impl_type(fn, arg_types)
            next if extracted_impl_type.nothing?

            extracted_impl_type.deintersect.each do |t|
              (t.respond_to?(:constraint) && !t.constraint.nil? ? t.constraint.deintersect : [t]).each do |t|
                if t.respond_to?(:protocol_type?) && t.protocol_type? && t.protocol == self
                  return @protocol_scope.resolve_function(fn_name, arg_types, exclude_lambdas: true, infer_untyped_lambdas: infer_untyped_lambdas, force_scope: true)
                end
              end
            end

            return_types = extracted_impl_type.deunion.map do |impl_type|
              _, _, matching_impl_scope = impls.find do |for_type, associated_types, impl_scope|
                for_type.subtype?(impl_type, impl_scope.type_params.keys.map { |t| [t, Types.nothing] }.to_h) &&
                  associated_types.all? { |k, t| t.subtype?(extracted_associated_types[k]) }
              end

              raise Scope::Error.new("No implementation could be found of protocol '#{module_prefix}#{@name}' for type '#{impl_type}'") if matching_impl_scope.nil?

              specialized_arg_types = parameterized_arg_types.zip(arg_types).map do |parameterized, arg_type|
                if parameterized == Types::Simple.new(hidden_type_param)
                  impl_type
                else
                  arg_type
                end
              end

              matching_impl_scope.resolve_function(fn_name, specialized_arg_types, exclude_lambdas: true, infer_untyped_lambdas: infer_untyped_lambdas, force_scope: true)
            end

            return Types.union(return_types)
          end

          raise Scope::Error.new("The implementing type could not be determined for protocol '#{@name}'")
        else
          raise Scope::Error.new("Protocol '#{module_prefix}#{@name}' has no fn named '#{fn_name}' with #{arg_types.count} argument#{'s' if arg_types.count != 1}")
        end
      end

      private

      def module_prefix
        @module_prefix ||= @module_path.empty? ? '' : @module_path.join('.') + '.'
      end

      def extract_impl_type(fn, arg_types)
        # Replace the protocol name references in the args with a hidden type parameter
        parameterized_arg_types = fn.args.map { |arg_name, arg_type| parameterizer_scope.resolve_type(arg_type) }

        # Extract the type from the args that should have the protocol implementation
        extracting_type_params = @type_params.map { |t| [t.type_atom, Types.nothing] }.to_h.merge(hidden_type_param => Types.nothing)
        parameterized_arg_types.zip(arg_types).each { |parameterized_arg, arg| parameterized_arg.subtype?(arg, extracting_type_params) if !arg.is_a?(Hash) }
        [extracting_type_params[hidden_type_param], extracting_type_params.reject { |k, _| k == hidden_type_param }, parameterized_arg_types]
      end

      def hidden_type_param
        @hidden_type_param ||= "_#{@name}".to_sym
      end

      def parameterizer_scope
        @parameterizer_scope ||=
          begin
            scope = Scope.new(@protocol_scope)
            type_params.each { |t| scope.define_type_param(t) }
            scope.define_typealias(@name, Types::Simple.new(hidden_type_param, parameter_type: true), @type_params.map { |t| Types::Simple.new("_#{t.type_atom}".to_sym, parameter_type: true) })
            scope
          end
      end
    end
  end
end
