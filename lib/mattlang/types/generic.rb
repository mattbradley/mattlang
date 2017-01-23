module Mattlang
  module Types
    class Generic < Base
      attr_reader :type_atom, :type_parameters

      def initialize(type_atom, type_parameters)
        @type_atom = type_atom
        @type_parameters = type_parameters

        raise "All parameter types in generic '#{@type_atom}' must inherit from Types::Base" if !@type_parameters.all? { |t| t.is_a?(Types::Base) }
      end

      def subtype?(other, type_bindings = nil)
        if other.is_a?(Generic)
          type_atom == other.type_atom &&
          type_parameters.size == other.type_parameters.size &&
          type_parameters.zip(other.type_parameters).all? { |t1, t2| t1.subtype?(t2, type_bindings) }
        elsif other.is_a?(Union)
          other.types.all? { |t| self.subtype?(t) }
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

      def concrete_types
        ([self] + type_parameters.map(&:concrete_types)).flatten.uniq
      end

      def to_s
        "#{type_atom.to_s}<#{type_parameters.map(&:to_s).join(', ')}>"
      end
    end
  end
end
