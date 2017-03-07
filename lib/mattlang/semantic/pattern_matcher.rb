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

      def check_redundancy(patterns, type)
      end
      
      # Check if a pattern is compatible with a type
      #   (Maybe we'll ignore assignment patterns for now)
      #   For assignment patterns, the pattern must be a supertype of the candidate type
      #     (_, _) = (Int, Float) | (Int, String)
      #       deconstructs to (Int, Float | String)
      #     { a: _, b: _ } = { a: Int, b: String, c: Bool } | {a: Float, b: String, d: Int }
      #       deconstructs to { a: Int | Float, b: String }
      #
      #   For case patterns, the pattern must be a subtype of the candidate type

      def check_case_pattern(pattern, candidate)
        candidate_types = candidate.is_a?(Types::Union) ? candidate.types : [candidate]
        bindings = candidate_types.map { |type| try_pattern_match(pattern, type) }

        raise NoMatchError.new("Cannot match this pattern with the type '#{candidate}'", pattern) if bindings.all?(&:nil?)

        unified_bindings = bindings.compact.reduce({}) do |h, b|
          b.each do |variable, type|
            (h[variable] ||= []) << type
          end

          h
        end.map { |v, ts| [v, Types.combine(ts)] }.to_h
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

          pattern.type = combine_if_not_nil(pattern.type, Types::Tuple.new(pattern.children.map(&:type)))
          bindings
        when :__record__
          raise NoMatchError.new("Cannot match a record with the non-record type '#{candidate}'", pattern) if !candidate.is_a?(Types::Record)

          bindings = pattern.children.reduce({}) do |matches, field_node|
            raise NoMatchError.new("Cannot match a record with field '#{field_node.term}' missing in record type '#{candidate}'", pattern) if !candidate.types_hash.key?(field_node.term)

            new_matches = pattern_match(field_node.children.first, candidate.types_hash[field_node.term])
            matches.merge(new_matches) { |k, _, _| raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k) }
          end

          types_hash = pattern.children.map do |field_node|
            field_node.type = combine_if_not_nil(field_node.type, field_node.children.first.type)
            [field_node.term, field_node.type]
          end.to_h

          pattern.type = combine_if_not_nil(pattern.type, Types::Record.new(types_hash))
          bindings
        when :'::'
          raise NoMatchError.new("Cannot perform a cons match with the non-list type '#{candidate}'", pattern) if !is_a_list?(candidate)

          head, tail = pattern.children

          bindings =
            pattern_match(head, candidate.type_parameters.first)
            .merge(pattern_match(tail, Types::Union.new([candidate, Types::Generic.new(:List, [Types.nothing])]))) do |k, _, _|
              raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k)
            end

          pattern.type = combine_if_not_nil(pattern.type, candidate)
          bindings
        else
          if pattern.children.nil? && pattern.term.is_a?(Symbol)
            pattern.type = combine_if_not_nil(pattern.type, candidate)

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
        nil
      end

      private

      # Determines if the pattern_vector is useful with respect to the pattern_matrix
      #
      # TODO: maybe make another method that checks an AST against a type and constructs a pattern out of it
      # I.e.: { body: b, headers: nil, code: (404, c) } against type { body: String, code: (Int, String), headers: List<(String, String)> | Nil, url: String }
      # might be converted to the following pattern:
      #
      # [:Record,
      #   [
      #     [:Wildcard, :b],
      #     [:Tuple,
      #       [
      #         [:Literal, 404],
      #         [:Wildcard, :c]
      #       ]
      #     ],
      #     [:Literal, nil],
      #     [:Wildcard, :_]
      #   ]
      # ]
      #
      # where the order of child patterns in record is determined by alpha order of the keys. A wildcard is used for the omitted `url` key.
      #
      # case e # (Int, Float) | (String, String)
      #   (0, x) # (Int, Float) only
      
      # Check if a pattern satisfies a type and construct a pattern tree
      def analyze_pattern(pattern, type)
        case pattern.term
        when :__tuple__
          tuple_types = (type.is_a?(Types::Union) ? type.types : [type]).select { |t| t.is_a?(Tuple) && t.type.count == pattern.children.count }

          raise NoMatchError.new("Cannot match a #{pattern.children.count}-tuple with the type '#{type}'", pattern) if tuple_types.empty?

          # TODO: check the inner patterns against each possible tuple type to see if it matches (catch any errors)
          # for all matching types, any variables bound in the pattern need to have their types merged
          # the type of this pattern is the union of all matching tuple types
          # if none of the tuple types match this pattern, throw an error
          [:Tuple, pattern.children.zip(type.types).map { |inner_pattern, inner_type| analyze_pattern(inner_pattern, inner_type) }]
        when :__record__
          raise NoMatchError.new("Cannot match a record with the non-record type '#{type}'", pattern) if !type.is_a?(Types::Record)

=begin
          bindings = pattern.children.reduce({}) do |matches, field_node|
            raise NoMatchError.new("Cannot match a record with field '#{field_node.term}' missing in record type '#{type}'", pattern) if !type.types_hash.key?(field_node.term)

            new_matches = pattern_match(field_node.children.first, type.types_hash[field_node.term])
            matches.merge(new_matches) { |k, _, _| raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k) }
          end

          types_hash = pattern.children.map do |field_node|
            field_node.type = combine_if_not_nil(field_node.type, field_node.children.first.type)
            [field_node.term, field_node.type]
          end.to_h

          pattern.type = combine_if_not_nil(pattern.type, Types::Record.new(types_hash))
          bindings
=end
          
        when :'::'
          raise NoMatchError.new("Cannot perform a cons match with the non-list type '#{type}'", pattern) if !is_a_list?(type)

          head, tail = pattern.children

          bindings =
            pattern_match(head, type.type_parameters.first)
            .merge(pattern_match(tail, Types::Union.new([type, Types::Generic.new(:List, [Types.nothing])]))) do |k, _, _|
              raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k)
            end

          pattern.type = combine_if_not_nil(pattern.type, type)
          bindings
        else
          if pattern.children.nil? && pattern.term.is_a?(Symbol)
            pattern.type = combine_if_not_nil(pattern.type, type)

            if pattern.term == :_ # Wildcard pattern does no binding
              {}
            else # Simple identifier match, aka variable binding
              { pattern.term => type }
            end
          else
            raise InvalidPatternError.new("Invalid pattern on left-hand-side of = operator", pattern)
          end
        end
      end
      
      def check_type(constructor, arg

      def is_useful(pattern_matrix, pattern_vector, type)
        return true if pattern_matrix.empty?
        return false if pattern_vector.empty?

        raise "The pattern matrix and pattern vector should have the same width" if pattern_matrix.first.length != pattern_vector.length

        first_pattern, rest_patterns = pattern_vector

      end

      def is_a_list?(type)
        type.is_a?(Types::Generic) && type.type_atom == :List && type.module_path.empty? && type.type_parameters.count == 1
      end

      def combine_if_not_nil(type1, type2)
        if type1.nil?
          type2
        elsif type2.nil?
          type1
        else
          Types.combine([type1, type2])
        end
      end
    end
  end
end
