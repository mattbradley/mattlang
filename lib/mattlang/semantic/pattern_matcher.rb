module Mattlang
  class Semantic
    module PatternMatcher
      class Error < Semantic::Error
        def self.title; 'Pattern Matching Error'; end
      end

      class NoMatchError < Error; end
      
      class InvalidPatternError < Error
        def self.title; 'Invalid Pattern Error'; end
      end

      # Check pattern matching an AST pattern against a candidate type
      def pattern_match(pattern, candidate)
        case pattern.term
        when :__tuple__
          raise NoMatchError.new("Cannot match a tuple with the non-tuple type '#{candidate}'", pattern) if !candidate.is_a?(Types::Tuple)
          raise NoMatchError.new("Cannot match a #{pattern.children.count}-tuple with the #{candidate.types.count}-tuple type '#{candidate}'", pattern) if pattern.children.count != candidate.types.count

          bindings = pattern.children.zip(candidate.types).reduce({}) do |matches, (inner_pattern, inner_type)|
            new_matches = pattern_match(inner_pattern, inner_type)
            matches.merge(new_matches) { |k, _, _| raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k) }
          end

          pattern.type = Types::Tuple.new(pattern.children.map(&:type))
          bindings
        when :__record__
          raise NoMatchError.new("Cannot match a record with the non-record type '#{candidate}'", pattern) if !candidate.is_a?(Types::Record)

          bindings = pattern.children.reduce({}) do |matches, field_node|
            raise NoMatchError.new("Cannot match a record with field '#{field_node.term}' missing in record type '#{candidate}'", pattern) if !candidate.types_hash.key?(field_node.term)

            new_matches = pattern_match(field_node.children.first, candidate.types_hash[field_node.term])
            matches.merge(new_matches) { |k, _, _| raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k) }
          end

          types_hash = pattern.children.map do |field_node|
            field_node.type = field_node.children.first.type
            [field_node.term, field_node.type]
          end.to_h

          pattern.type = Types::Record.new(types_hash)
          bindings
        when :'::'
          raise NoMatchError.new("Cannot perform a cons match with the non-list type '#{candidate}'", pattern) if !is_a_list?(candidate)

          head, tail = pattern.children

          bindings =
            pattern_match(head, candidate.type_parameters.first)
            .merge(pattern_match(tail, Types::Union.new([candidate, Types::Simple.new(:EmptyList)]))) do |k, _, _|
              raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k)
            end

          pattern.type = candidate
          bindings
        else
          if pattern.children.nil? && pattern.type.nil? && pattern.term.is_a?(Symbol)
            pattern.type = candidate

            if pattern.term == :_ # Wildcard pattern does no binding
              {}
            else # Simple identifier match, aka variable binding
              { pattern.term => candidate }
            end
          else
            raise InvalidPatternError.new("Invalid pattern on left-hand-side of = operator", pattern)
          end
        end
      end

      def try_pattern_match(pattern, candidate)
        pattern_match(pattern, candidate)
      rescue NoMatchError
        false
      end

      private

      def is_a_list?(type)
        type.is_a?(Types::Generic) && type.type_atom == :List && type.module_path.empty? && type.type_parameters.count == 1
      end
    end
  end
end
