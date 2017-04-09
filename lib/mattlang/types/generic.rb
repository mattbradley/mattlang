module Mattlang
  module Types
    class Generic < Base
      attr_reader :type_atom, :type_parameters, :module_path

      def initialize(type_atom, type_parameters, protocol: nil, module_path: [])
        @type_atom = type_atom
        @type_parameters = type_parameters
        @module_path = module_path
        @protocol = protocol

        raise "All parameter types in generic '#{@type_atom}' must inherit from Types::Base" if !@type_parameters.all? { |t| t.is_a?(Types::Base) }
      end

      def protocol_type?
        !@protocol.nil?
      end

      # All type parameters in a generic are covariant
      def evaluate_subtype(other, type_bindings = nil, same_parameter_types = false)
        if other.is_a?(self.class) &&
          type_atom == other.type_atom &&
          type_parameters.size == other.type_parameters.size &&
          type_parameters.zip(other.type_parameters).all? { |t1, t2| t1.subtype?(t2, type_bindings, same_parameter_types) }
          true
        elsif protocol_type?
          @protocol.implemented_by?(other, @type_parameters, type_bindings)
        else
          false
        end
      end

      def parameter_type?
        type_parameters.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        self.class.new(type_atom, type_parameters.map { |t| t.replace_type_bindings(type_bindings) }, module_path: module_path)
      end

      def ==(other)
        other.is_a?(self.class) && other.type_atom == type_atom && other.module_path == module_path && other.type_parameters == type_parameters
      end

      def to_s
        path = module_path.join('.') + '.' if !module_path.empty?
        type_params_str = type_parameters.empty? ? '' : "<#{type_parameters.map(&:to_s).join(', ')}>"
        "#{path}#{type_atom}#{type_params_str}"
      end
    end
  end
end
