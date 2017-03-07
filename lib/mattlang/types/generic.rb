module Mattlang
  module Types
    class Generic < Base
      attr_reader :type_atom, :type_parameters, :module_path

      def initialize(type_atom, type_parameters, module_path: [])
        @type_atom = type_atom
        @type_parameters = type_parameters
        @module_path = module_path

        raise "All parameter types in generic '#{@type_atom}' must inherit from Types::Base" if !@type_parameters.all? { |t| t.is_a?(Types::Base) }
      end

      # All type parameters in a generic are covariant
      def subtype?(other, type_bindings = nil, same_parameter_types = false)
        if other.is_a?(Generic)
          type_atom == other.type_atom &&
          type_parameters.size == other.type_parameters.size &&
          type_parameters.zip(other.type_parameters).all? { |t1, t2| t1.subtype?(t2, type_bindings, same_parameter_types) }
        elsif other.is_a?(Union)
          other.types.all? { |t| self.subtype?(t, type_bindings, same_parameter_types) }
        elsif other.nothing?
          true
        else
          false
        end
      end

      def parameter_type?
        type_parameters.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        Generic.new(type_atom, type_parameters.map { |t| t.replace_type_bindings(type_bindings) })
      end

      def ==(other)
        other.is_a?(Generic) && other.type_atom == type_atom && other.type_parameters == type_parameters
      end

      def to_s
        path = module_path.join('.') + '.' if !module_path.empty?
        "#{path}#{type_atom}<#{type_parameters.map(&:to_s).join(', ')}>"
      end
    end
  end
end
