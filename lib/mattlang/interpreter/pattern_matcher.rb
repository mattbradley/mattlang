module Mattlang
  class Interpreter
    module PatternMatcher
      def pattern_match(pattern, value)
        case pattern.term
        when :__tuple__
          raise "Expected value to be a tuple" if !value.value.is_a?(Tuple)

          pattern.children.zip(value.value).reduce({}) do |matches, (inner_pattern, inner_value)|
            matches.merge(pattern_match(inner_pattern, inner_value))
          end
        when :__record__
          raise "Expected value to be a record" if !value.value.is_a?(Record)

          pattern.children.reduce({}) do |matches, inner_pattern|
            matches.merge(pattern_match(inner_pattern.children.first, value.value[inner_pattern.term]))
          end
        when :'::'
          raise "Expected value to be a list" if !value.value.is_a?(List)

          head, tail = pattern.children

          pattern_match(head, value.value.first)
            .merge(pattern_match(tail, Value.new(value.value[1..-1], value.value.count == 1 ? Types::Simple.new(:EmptyList) : value.type)))
        else
          if pattern.term == :_
            {}
          else
            { pattern.term => value }
          end
        end
      end
    end
  end
end
