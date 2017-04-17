module Mattlang
  module Types
    class Record < Base
      attr_reader :types_hash

      def initialize(types_hash)
        @types_hash = types_hash

        raise "All types in a record type must inherit from Types::Base" if !@types_hash.all? { |k, t| t.is_a?(Types::Base) }
      end

      def evaluate_subtype(other, type_bindings = nil, same_parameter_types = false)
        other.is_a?(Record) &&
          types_hash.all? { |k, t| other.types_hash.key?(k) && t.subtype?(other.types_hash[k], type_bindings, same_parameter_types) }
      end

      def parameter_type?
        types_hash.values.any?(&:parameter_type?)
      end

      def replace_type_bindings(type_bindings)
        Record.new(types_hash.map { |k, t| [k, t.replace_type_bindings(type_bindings)] }.to_h)
      end

      def deep_record_update(new_types, scope)
        Record.new(@types_hash.merge(new_types))
      end

      def ==(other)
        other.is_a?(Record) && other.types_hash == types_hash
      end

      def to_s
        "{#{types_hash.sort.map { |k, t| "#{k}: #{t}"}.join(', ')}}"
      end
    end
  end
end
