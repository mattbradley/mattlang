module Mattlang
  module Types
    class Simple < Base
      attr_reader :type_atom, :module_path

      def initialize(type_atom, parameter_type: false, module_path: [])
        @type_atom = type_atom
        @module_path = module_path
        @parameter_type = parameter_type == true
      end

      def parameter_type?
        @parameter_type == true
      end

      def nothing?
        type_atom == :Nothing && module_path.empty?
      end

      def anything?
        type_atom == :Anything && module_path.empty?
      end

      def subtype?(other, type_bindings = nil, same_parameter_types = false)
        if anything?
          true
        elsif self == other && (same_parameter_types || !other.parameter_type?)
          true
        elsif type_bindings&.key?(type_atom) # Is this type a type parameter?
          if (bound_type = type_bindings[type_atom]) # Is this type parameter currently bound to a type?
            # Try to unify the types into a more general type
            if bound_type.subtype?(other, nil, true)
              true
            elsif other.subtype?(bound_type, nil, true)
              type_bindings[type_atom] = other
              true
            else
              type_bindings[type_atom] = Types.combine(bound_type, other)
              true
            end
          else
            type_bindings[type_atom] = other # This type parameter isn't bound, so bind it to `other`
            true
          end
        elsif other.nothing?
          true
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

      def to_s
        path = module_path.join('.') + '.' if !module_path.empty?
        path.to_s + (parameter_type? ? '@' : '') + type_atom.to_s
      end
    end
  end
end
