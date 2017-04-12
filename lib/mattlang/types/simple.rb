module Mattlang
  module Types
    class Simple < Base
      attr_reader :type_atom, :module_path, :protocol, :constraint

      def initialize(type_atom, parameter_type: false, protocol: nil, constraint: nil, module_path: [])
        @type_atom = type_atom
        @module_path = module_path
        @parameter_type = parameter_type == true
        @protocol = protocol
        @constraint = constraint

        raise "Type atom must be a symbol" unless @type_atom.is_a?(Symbol)
      end

      def parameter_type?
        @parameter_type == true
      end

      def protocol_type?
        !@protocol.nil?
      end

      def nothing?
        type_atom == :Nothing && module_path.empty?
      end

      def anything?
        type_atom == :Anything && module_path.empty?
      end

      def evaluate_subtype(other, type_bindings = nil, same_parameter_types = false)
        if self == other && !other.parameter_type?
          true
        elsif self == other && same_parameter_types
          unify_type_bindings(other, type_bindings) if other.parameter_type? && type_bindings
          true
        elsif parameter_type? && type_bindings&.key?(type_atom) # Is this type a type parameter?
          if @constraint
            @constraint.subtype?(other, type_bindings, same_parameter_types)
          else
            unify_type_bindings(other, type_bindings)
            true
          end
        elsif protocol_type?
          @protocol.implemented_by?(other, [], type_bindings)
        else
          false
        end
      end

      def replace_type_bindings(type_bindings)
        if type_bindings && type_bindings.key?(@type_atom)
          type_bindings[@type_atom]
        else
          self
        end
      end

      def ==(other)
        other.is_a?(Simple) && other.type_atom == type_atom && other.module_path == module_path
      end

      def to_s
        path = @module_path.join('.') + '.' if !@module_path.empty?
        path.to_s + (parameter_type? ? '@' : '') + @type_atom.to_s + (@constraint ? " : #{@constraint.to_s}" : '')
      end

      private

      def unify_type_bindings(other, type_bindings)
        if (bound_type = type_bindings[@type_atom]) # Is this type parameter currently bound to a type?
          # Try to unify the types into a more general type
          if bound_type.subtype?(other, nil, true)
            # Types are already unified, so do nothing
          elsif other.subtype?(bound_type, nil, true)
            type_bindings[@type_atom] = other
          else
            type_bindings[@type_atom] = Types.union([bound_type, other])
          end
        else
          type_bindings[@type_atom] = other # This type parameter isn't bound, so bind it to `other`
        end
      end
    end
  end
end
