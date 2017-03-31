module Mattlang
  class Interpreter
    module PatternMatcher
      def self.destructure_match(pattern, value)
        case pattern.term
        when :__tuple__
          raise "Expected value to be a tuple" if !value.value.is_a?(Tuple)

          pattern.children.zip(value.value).reduce({}) do |matches, (inner_pattern, inner_value)|
            matches.merge(destructure_match(inner_pattern, inner_value))
          end
        when :__record__
          raise "Expected value to be a record" if !value.value.is_a?(Record)

          pattern.children.reduce({}) do |matches, inner_pattern|
            matches.merge(destructure_match(inner_pattern.children.first, value.value[inner_pattern.term]))
          end
        when :'::'
          raise "Cons destructuring is not supported"
          raise "Expected value to be a list" if !value.value.is_a?(List)

          head, tail = pattern.children

          destructure_match(head, value.value.first)
            .merge(destructure_match(tail, Value.new(value.value[1..-1], value.value.count == 1 ? Types::Generic.new(:List, [Types.nothing]) : value.type)))
        else
          if pattern.term == :_
            {}
          else
            { pattern.term => value }
          end
        end
      end

      # If the pattern matches the value, returns a hash of bindings (could be empty if there are no bindings);
      # otherwise, returns nil
      def self.case_match(pattern, value)
        case pattern.term
        when :__tuple__
          if value.type.is_a?(Types::Tuple) && pattern.children.count == value.type.types.count
            child_bindings = pattern.children.zip(value.value).map do |child_pattern, child_value|
              case_match(child_pattern, child_value) || (break nil) # Bail out early if one of the inner patterns doesn't match
            end

            child_bindings.reduce(&:merge) if child_bindings
          end
        when :__record__
          if value.type.is_a?(Types::Record) && (pattern.children.map(&:term) - value.type.types_hash.keys).empty?
            child_bindings = pattern.children.map do |child_pattern|
              child_value = value.value[child_pattern.term]
              case_match(child_pattern.children.first, child_value) || (break nil)
            end

            child_bindings.reduce(&:merge) if child_bindings
          end
        when :__list__
          if is_a_list?(value.type) && value.value.empty?
            {}
          end
        when :'::'
          if is_a_list?(value.type) && !value.value.empty?
            head_pattern, tail_pattern = pattern.children
            head, *tail = value.value

            if (head_bindings = case_match(head_pattern, head))
              if (tail_bindings = case_match(tail_pattern, Value.new(List.new(tail), value.type)))
                head_bindings.merge(tail_bindings)
              end
            end
          end
        else
          if pattern.term.is_a?(Symbol)
            if ('A'..'Z').include?(pattern.term[0])
              module_path = pattern.meta && pattern.meta[:module_path]

              if value.type.is_a?(Types::Nominal) &&
                pattern.term == value.type.type_atom && (
                  module_path.nil? ||
                  module_path.count <= value.type.module_path.count &&
                  value.type.module_path[-module_path.count..-1] == module_path
                )

                child_pattern =
                  if pattern.children.count > 1
                    AST.new(:__tuple__, pattern.children)
                  else
                    pattern.children.first
                  end

                case_match(child_pattern, value.value)
              end
            elsif pattern.term == :_ # Wildcard pattern does no binding
              {}
            else
              { pattern.term => value } # pattern is a variable
            end
          elsif !pattern.type.nil? # pattern is a literal
            if pattern.type == value.type && pattern.term == value.value
              {}
            else
              nil
            end
          else
            raise "Invalid pattern"
          end
        end
      end

      private

      def self.is_a_list?(type)
        type.is_a?(Types::Generic) && type.type_atom == :List && type.module_path.empty? && type.type_parameters.count == 1
      end
    end
  end
end
