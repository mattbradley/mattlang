module Mattlang
  module Types
    class Nominal < Base
      attr_reader :type_atom, :type_parameters, :underlying_type, :module_path

      def initialize(type_atom, type_parameters, underlying_type, module_path: [])
        @type_atom = type_atom
        @type_parameters = type_parameters
        @underlying_type = underlying_type
        @module_path = module_path

        raise "Type atom must be a symbol" unless @type_atom.is_a?(Symbol)
        raise "All parameter types in generic '#{@type_atom}' must inherit from Types::Base" if !@type_parameters.all? { |t| t.is_a?(Types::Base) }
      end

      def evaluate_subtype(other, type_bindings = nil, same_parameter_types = false)
        other.is_a?(Nominal) && (
          (
            other.type_atom == type_atom &&
            other.module_path == module_path &&
            other.type_parameters.count == type_parameters.count &&
            type_parameters.zip(other.type_parameters).all? { |param, other_param| param.subtype?(other_param, type_bindings, same_parameter_types) }
          ) ||
          (
            other.underlying_type &&
            subtype?(other.underlying_type, type_bindings, same_parameter_types)
          )
        )
      end

      def parameter_type?
        type_parameters.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        Nominal.new(type_atom, type_parameters.map { |t| t.replace_type_bindings(type_bindings) }, underlying_type && underlying_type.replace_type_bindings(type_bindings), module_path: module_path)
      end

      def ==(other)
        other.is_a?(Nominal) && other.type_atom == type_atom && other.module_path == module_path && other.type_parameters == type_parameters
      end

      def matching_types(&matcher)
        super(&matcher) +
          if @underlying_type.nil?
            []
          else
            @underlying_type.matching_types(&matcher)
          end
      end

      def deep_record_update(new_types, scope)
        # Changing the underlying record types of this nominal type may change
        # bound type parameters. So, we have to resolve the typedef again and
        # reconstruct the type using the new updated underlying type.
        typedef_scope = module_path&.any? ? scope.resolve_module_path(module_path) : scope.global_scope
        typedef_scope.resolve_typedef(@type_atom, @underlying_type.deep_record_update(new_types, scope), force_scope: true)
      end

      def to_s
        path = module_path.join('.') + '.' if !module_path.empty?
        type_params = type_parameters.empty? ? "" : "<#{type_parameters.map(&:to_s).join(', ')}>"
        "#{path}#{type_atom}#{type_params}"
      end
    end
  end
end
