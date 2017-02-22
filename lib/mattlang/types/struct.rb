module Mattlang
  module Types
    class Struct < Base
      attr_reader :types_hash

      def initialize(types_hash)
        @types_hash = types_hash

        raise "All types in a struct must inherit from Types::Base" if !@types_hash.all? { |k, t| t.is_a?(Types::Base) }
      end

      def subtype?(other, type_bindings = nil, same_parameter_types = false)
        if other.is_a?(Struct)
          types_hash.all? { |k, t| other.types_hash.key?(k) && t.subtype?(other.types_hash[k]) }
        elsif other.is_a?(Union)
          other.types.all? { |t| self.subtype?(t, type_bindings, same_parameter_types) }
        else
          false
        end
      end

      def parameter_type?
        types_hash.values.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        Struct.new(types_hash.map { |k, t| [k, t.replace_type_bindings(type_bindings)] }.to_h)
      end

      def ==(other)
        other.is_a?(Struct) && other.types_hash == types_hash
      end

      def concrete_types
        types_hash.values.map(&:concrete_types).flatten.uniq
      end

      def to_s
        "{ #{types_hash.sort.map { |k, t| "#{k}: #{t}"}.join(', ')} }"
      end
    end
  end
end
