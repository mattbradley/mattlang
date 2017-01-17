module Mattlang
  module Types
    class Simple < Base
      attr_reader :type_atom

      def initialize(type_atom, parameter_type: false)
        @type_atom = type_atom
        @parameter_type = parameter_type == true
      end

      def parameter_type?
        @parameter_type
      end

      def subtype?(other, type_bindings = nil)
        if self == other && !other.parameter_type?
          true
        elsif type_bindings&.key?(type_atom)
          type_binding = type_bindings[type_atom]

          if type_binding.nil?
            type_bindings[type_atom] = other
            true
          else
            type_binding == other
          end
        else
          false
        end
      end

      def replace_type_bindings(type_bindings)
        if type_bindings.key?(type_atom)
          type_bindings[type_atom]
        else
          self
        end
      end

      def ==(other)
        other.is_a?(Simple) && other.type_atom == type_atom
      end

      def concrete_types
        [self]
      end

      def to_s
        (parameter_type? ? '@' : '') + type_atom.to_s
      end
    end
  end
end
