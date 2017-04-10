module Mattlang
  class Semantic
    class Pattern
      attr_reader :kind, :node
      attr_accessor :args, :type, :bindings

      def initialize(kind, args, node, type, bindings)
        raise "Unknown pattern kind '#{kind}'" unless [:constructor, :tuple, :record, :cons, :empty, :wildcard, :literal, :complement, :or].include?(kind)
        raise "An or pattern must have more than one arg" if kind == :or && args.count < 2

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

        merged_type = Types.union([@type, other.type])

        merged_bindings =
          [@bindings, other.bindings]
            .reduce({}) do |full_bindings, bindings|
              bindings.each { |variable, bound_type| (full_bindings[variable] ||= []) << bound_type }
              full_bindings
            end
            .map do |variable, types_array|
              [variable, Types.union(types_array)]
            end
            .to_h

        Pattern.new(@kind, merged_args, @node, merged_type, merged_bindings)
      end

      def matches_type?(candidate_type)
        @type.deunion.any? do |type|
          if type.is_a?(Types::Tuple)
            candidate_type.is_a?(Types::Tuple) && type.types.count == candidate_type.types.count
          elsif type.is_a?(Types::Record)
            candidate_type.is_a?(Types::Record) && (type.types_hash.keys - candidate_type.types_hash.keys).empty?
          else
            type.subtype?(candidate_type, nil, true)
          end
        end
      end

      def reconcile_with_type(new_type)
        case @kind
        when :tuple
          return @type = new_type if new_type.is_a?(Types::Union)

          new_arg_types = new_type.deunion.map(&:types).transpose.map { |arg_types| Types.union(arg_types) }
          new_arg_types.zip(@args).each { |t, a| a.reconcile_with_type(t) }

          @type = Types::Tuple.new(@args.map(&:type))
        when :record
          return @type = new_type if new_type.is_a?(Types::Union)

          min_type_args = @type.deunion.min_by { |t| t.types_hash.count }.types_hash.keys.sort.zip(@args).to_h

          raise "Cannot reconcile the fields in type '#{new_type}' with the fields in record pattern of type '#{@type}'" unless (min_type_args.keys - new_type.types_hash.keys).empty?

          built_type = {}
          new_type.types_hash.each do |field, new_arg_type|
            built_type[field] =
              if (arg = min_type_args[field])
                arg.reconcile_with_type(new_arg_type)
                arg.type
              else
                new_arg_type
              end
          end

          @type = Types::Record.new(built_type)
        when :cons
          return @type = new_type if new_type.is_a?(Types::Union)

          raise "Cannot reconcile the non-list type '#{new_type}' with a cons pattern of type '#{@type}'" unless new_type.is_a?(Types::Generic) && new_type.type_atom == :List && new_type.type_parameters.count == 1

          @args.first.reconcile_with_type(new_type.type_parameters.first)
          @args.last.reconcile_with_type(new_type)

          @type = new_type
        when :empty
          if @type.subtype?(new_type, nil, true)
            @type = new_type
          elsif !new_type.subtype?(@type, nil, true)
            raise "Cannot reconcile type '#{new_type}' with the empty pattern type '#{@type}'"
          end
        when :literal
          raise "Cannot reconcile type '#{new_type}' with the literal pattern type '#{@type}'" unless new_type == @type
        when :wildcard
          if @type.subtype?(new_type, nil, true)
            @type = new_type
            @bindings[@bindings.keys.first] = new_type if @bindings.any?
          elsif !new_type.subtype?(@type, nil, true)
            raise "Cannot reconcile type '#{new_type}' with the wildcard pattern type '#{@type}'"
          end
        when :constructor
          return @type = new_type if new_type.is_a?(Types::Union)

          pattern_type = @type.deunion.first
          raise "Cannot reconcile type '#{new_type}' with a constructor pattern of type '#{@type}'" unless new_type.is_a?(Types::Nominal) && new_type.type_atom == pattern_type.type_atom && new_type.module_path == pattern_type.module_path

          @args.first.reconcile_with_type(new_type.underlying_type) unless new_type.underlying_type.nil?

          @type = new_type
        else
          raise "Cannot reconcile type '#{new_type}' with a pattern with kind '#{@kind}'"
        end
      end

      def distribute_or_patterns(depth: 0)
        if @kind == :literal
          [self]
        elsif @kind == :or
          if depth > 100
            [self]
          else
            @args.map { |arg| arg.distribute_or_patterns(depth: depth + 1) }.flatten
          end
        else
          first, *rest = @args.map { |arg| arg.distribute_or_patterns(depth: depth) }
          if first.nil?
            [self]
          else
            first.product(*rest).map do |new_args|
              Pattern.new(@kind, new_args, @node, @type, @bindings)
            end
          end
        end
      end

      def dup
        duped_args = @args.map { |a| a.dup rescue a }
        Pattern.new(@kind, duped_args, @node, @type, @bindings.dup)
      end

      def to_s
        case @kind
        when :literal then @args.first.inspect
        when :complement
          values = @args.map { |a| a.args.first }

          values_msg =
            case values.count
            when 0 then raise "Expected a complement pattern to have some values"
            when 1 then values[0].inspect
            when 2 then "#{values[0].inspect} and #{values[1].inspect}"
            when 3 then "#{values[0].inspect}, #{values[1].inspect}, and #{values[2].inspect}"
            when 4 then "#{values[0].inspect}, #{values[1].inspect}, #{values[2].inspect}, and #{values[3].inspect}"
            else "#{values[0...3].map(&:inspect).join(', ')}, ..."
            end

          "<#{@type} values besides #{values_msg}>"
        when :wildcard then "_"
        when :tuple then "(#{@args.map(&:to_s).join(', ')})"
        when :record then "{ #{@type.types_hash.keys.sort.zip(@args).map { |k, v| "#{k}: #{v.to_s}" }.join(', ')} }"
        when :cons then "#{@args.first.to_s} :: #{@args.last.to_s}"
        when :empty then "[]"
        when :or then "_"
        when :constructor
          constructor_str = @type.module_path && !@type.module_path.empty? ? "#{@type.module_path.join('.')}." : ""
          constructor_str += @type.type_atom.to_s
          "#{constructor_str} #{@args.first.to_s}"
        else raise "Unknown pattern kind '#{@kind.inspect}'"
        end
      end

      def inspect(indent = 0)
        str = "  " * indent + "(#{kind} : #{@type}"

        if @kind == :literal
          str += " = #{@args.first.inspect}"
        elsif @kind != :wildcard && !@args.empty?
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
