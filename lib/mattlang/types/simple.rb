module Mattlang
  module Types
    class Simple < Base
      attr_reader :type_atom

      def initialize(type_atom, parameter_type: false)
        @type_atom = type_atom
        @parameter_type = parameter_type == true
      end

      def parameter_type?
        @parameter_type == true
      end

      def subtype?(other, type_bindings = nil, same_parameter_types = false)
        if self == other && (same_parameter_types || !other.parameter_type?)
          true
        elsif type_bindings&.key?(type_atom) # Is this type a type parameter?
          if (type_binding = type_bindings[type_atom]) # Is this type parameter currently bound to a type?
            type_binding == other
          else
            type_bindings[type_atom] = other # This type parameter isn't bound, so bind it to `other`
            true
          end
        else
          false
        end
      end

      def replace_type_bindings(type_bindings)
        if type_bindings && type_bindings.key?(type_atom)
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
