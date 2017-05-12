module Mattlang
  class Codegen
    class PatternMatcher
      def initialize(codegen)
        @codegen = codegen
      end

      def new_register
        @codegen.new_register
      end

      def destructure_match(pattern, subject_type, subject_register)
        case pattern.term
        when :__tuple__
          subject_type, unwrapping_src, subject_register = @codegen.unwrap_nominals(subject_type, subject_register)

          tuple_types =
            if subject_type.is_a?(Types::Union)
              subject_type.types.map(&:types).transpose.map { |ts| Types.union(ts) }
            else
              subject_type.types
            end

          pattern.children.each_with_index.reduce([unwrapping_src, {}]) do |(src, matched_registers), (inner_pattern, i)|
            inner_src, inner_matches = destructure_match(inner_pattern, tuple_types[i], "VALUE2FIELDS(#{subject_register})[#{i}]")
            [src + inner_src, matched_registers.merge(inner_matches)]
          end
        when :__record__
          byebug
        else
          if pattern.term == :_
            ['', {}]
          else
            ['', { pattern.term => subject_register }]
          end
        end
      end
    end
  end
end
