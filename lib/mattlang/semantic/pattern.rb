module Mattlang
  class Semantic
    class Pattern
      attr_reader :kind, :args, :node, :type, :bindings

      def initialize(kind, args, node, type, bindings)
        @kind = kind
        @args = args
        @node = node
        @type = type
        @bindings = bindings
      end
      
      # Merge the types and bindings of two patterns
      def merge(other)
        raise "Can't merge these two patterns" if @kind != other.kind || @args.count != other.args.count

        merged_args =
          if [:literal, :wildcard].include?(@kind)
            @args
          else
            @args.zip(other.args).map { |arg1, arg2| arg1.merge(arg2) }
          end

        merged_type = Types.combine([@type, other.type])

        merged_bindings =
          [@bindings, other.bindings]
            .reduce({}) do |full_bindings, bindings|
              bindings.each { |variable, bound_type| (full_bindings[variable] ||= []) << bound_type }
              full_bindings
            end
            .map do |variable, types_array|
              [variable, Types.combine(types_array)]
            end
            .to_h

        Pattern.new(@kind, merged_args, @node, merged_type, merged_bindings)
      end

      def matches_type?(candidate_type)
        if @type.is_a?(Types::Union)
          @type.types.any? { |t| t == candidate_type }
        else
          @type == candidate_type
        end
      end

      def inspect(indent = 0)
        str = "  " * indent + "(#{kind} : #{@type}"

        if @kind == :literal
          str += " = #{@args.first.inspect}"
        elsif @kind != :wildcard
          str += "\n"
          str += @args.map { |a| a.inspect(indent + 1) }.join("\n")
          str += "\n"
          str += "  " * indent
        end

        str += ")"
      end
    end
  end
end
