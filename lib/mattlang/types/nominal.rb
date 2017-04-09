module Mattlang
  module Types
    class Nominal < Base
      attr_reader :type_atom, :type_parameters, :underlying_type, :module_path

      def initialize(type_atom, type_parameters, underlying_type, module_path: [])
        @type_atom = type_atom
        @type_parameters = type_parameters
        @underlying_type = underlying_type
        @module_path = module_path

        raise "All parameter types in generic '#{@type_atom}' must inherit from Types::Base" if !@type_parameters.all? { |t| t.is_a?(Types::Base) }
      end

      # type Queue<T> = ...
      # type MyQueue<T> = Queue<T>
      # type DeeperQueue<T> = MyQueue<T>

      # this type: Queue<Int | Float>
      # subtype?: Queue<Int>, yes
      # subtype?: Queue<String>, no
      # subtype?: MyQueue<Int>, yes
      # subtype?: MyQueue<String>, no
      # subtype?: DeeperQueue<Float>, yes
      #
      # Named.subtype?(type)
      # Named == type || (type.is_a Named && subtype?(type.actual_type)
      #
      # type.subtype?(Named)
      # true if type.subtype?(Named.actual_type)

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

      def to_s
        path = module_path.join('.') + '.' if !module_path.empty?
        type_params = type_parameters.empty? ? "" : "<#{type_parameters.map(&:to_s).join(', ')}>"
        "#{path}#{type_atom}#{type_params}"
      end
    end
  end
end
