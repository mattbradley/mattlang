require "mattlang/semantic/pattern"

module Mattlang
  class Semantic
    module PatternMatcher
      class Error < Semantic::Error
        def self.title; 'Pattern Matching Error'; end
      end

      class NoMatchError < Error; end
      class CannotUnifyTypesError < Error; end
      
      class InvalidPatternError < Error
        def self.title; 'Invalid Pattern'; end
      end

      class RedundantPatternError < Error
        def self.title; 'Redundant Pattern'; end
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

      def check_patterns(patterns, type)
        check_redundance(patterns, type)
        check_exhaustiveness(patterns, type)
      end

      def check_redundance(patterns, type)
        matrix = []

        patterns.each do |pattern|
          if useful?(matrix, [pattern], [type])
            matrix << [pattern]
          else
            raise RedundantPatternError.new("This pattern is redudant and must be removed", pattern.nod)
          end
        end
      end

      def check_exhaustiveness(patterns, type)

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
      # case e # { body: String, code: Int } | { body: String }
      #   { body: b } -> ...
      #   { body: b, code: c } -> ... # Should be useless
      
      # Check if a pattern satisfies a type and construct a pattern tree
      def self.build_pattern(node, type)
        case node.term
        when :__tuple__
          # Only match against n-tuple types (this type if it's an n-tuple, or any child n-tuple types if this type is a union)
          tuple_types = (type.is_a?(Types::Union) ? type.types : [type]).select { |t| t.is_a?(Types::Tuple) && t.types.count == node.children.count }
          raise NoMatchError.new("Cannot match this #{node.children.count}-tuple with the type '#{type}'", node) if tuple_types.empty?

          # Recursively check the child types of each candidate type with the respective child patterns and determine the variables bound in the children
          # Throw an error if any variable is bound more than once in this pattern
          possible_patterns = tuple_types.map do |tuple_type|
            begin
              child_patterns = node.children.zip(tuple_type.types).map { |child, child_type| build_pattern(child, child_type) }

              Pattern.new(:tuple, child_patterns, node, Types::Tuple.new(child_patterns.map(&:type)), merge_bindings(child_patterns))
            rescue NoMatchError
              nil
            end
          end.compact

          # If no types in the union matched the subpatterns, throw an error
          raise NoMatchError.new("Cannot match this pattern with the type '#{type}'", node) if possible_patterns.empty?

          possible_patterns.reduce(&:merge)
        when :__record__
          # Only match against record types (or records types in a union) that contain all of this pattern's fields
          required_fields = node.children.map(&:term)
          record_types = (type.is_a?(Types::Union) ? type.types : [type]).select { |t| t.is_a?(Types::Record) && (required_fields - t.types_hash.keys).empty? }
          raise NoMatchError.new("Cannot match this record pattern with the type '#{type}'", node) if record_types.empty?

          possible_patterns = record_types.map do |record_type|
            begin
              ordered_fields = record_type.types_hash.sort
              built_type = {}

              child_patterns =
                ordered_fields.map do |field, child_type|
                  # A child pattern only includes its declared fields in its args, even if there are additional fields in the pattern's type
                  if (child = node.children.find { |c| c.term == field })
                    child_pattern = build_pattern(child.children.first, child_type)
                    built_type[field] = child_pattern.type

                    child_pattern
                  else
                    built_type[field] = child_type

                    nil
                  end
                end.compact

              Pattern.new(:record, child_patterns, node, Types::Record.new(built_type), merge_bindings(child_patterns))
            rescue NoMatchError
              nil
            end
          end.compact

          # If no types in the union matched the subpatterns, throw an error
          raise NoMatchError.new("Cannot match this pattern with the type '#{type}'", node) if possible_patterns.empty?

          possible_patterns.reduce(&:merge)
        when :'::'
          raise "Cons pattern not implemented"
        else
          raise InvalidPatternError.new("Invalid pattern", node) if !node.children.nil?

          if node.term.is_a?(Symbol) # pattern is a variable
            bindings =
              if node.term == :_ # Wildcard pattern does no binding
                {}
              else
                { node.term => type }
              end

            Pattern.new(:wildcard, [], node, type, bindings)
          elsif !node.type.nil? # pattern is a literal
            raise NoMatchError.new("Cannot match a literal '#{node.type}' with the type '#{type}'", node) if !type.subtype?(node.type)

            Pattern.new(:literal, [node.term], node, node.type, {})
          else
            raise InvalidPatternError.new("Invalid pattern", node)
          end
        end
      end

      def self.merge_bindings(patterns)
        merged_bindings = {}

        patterns.each do |pattern|
          merged_bindings.merge!(pattern.bindings) do |v, _, _|
            raise InvalidPatternError.new("Cannot bind to the variable '#{v}' more than once in the same pattern", pattern.node)
          end
        end

        merged_bindings
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

      # Returns [] if the matrix is exhaustive, otherwise it returns an array
      # of missing patterns that may be needed to make the patterns exhaustive
      def self.exhaustive?(matrix, types, debugger: false)
        byebug if debugger
        if matrix.empty?
          types.map { |arg_type| Pattern.new(:wildcard, [], nil, arg_type, {}) }
        elsif types.count == 0
          []
        else
          first_type, *rest_types = types
          pattern_types = (first_type.is_a?(Types::Union) ? first_type.types : [first_type])

          complete, missing_types = complete?(matrix.map(&:first), pattern_types)

          if complete
            pattern_types.each do |pattern_type|
              arg_types =
                case pattern_type
                when Types::Tuple then pattern_type.types
                when Types::Record then pattern_type.types_hash.sort.map(&:last)
                else raise "Unexpected pattern type '#{pattern_type}'; expected a tuple or record type"
                end

              missing_patterns = exhaustive?(specialize(pattern_type, matrix), arg_types + rest_types)

              if !missing_patterns.empty?
                child_patterns = pattern_vector[0...arg_types.count]
                rest_patterns = pattern_vector[arg_types.count..-1]

                missing_pattern =
                  case pattern_type
                  when Types::Tuple then Pattern.new(:tuple, child_patterns, nil, pattern_type, {})
                  when Types::Record then Pattern.new(:record, child_patterns, nil, pattern_type, {})
                  end

                return [missing_pattern] + rest_patterns
              end
            end

            []
          else
            missing_patterns = exhaustive?(to_default(matrix), rest_types)

            if missing_patterns.empty?
              []
            else
              if missing_types.count == pattern_types.count
                [Pattern.new(:wildcard, [], nil, first_type, {})] + missing_patterns
              else
                missing_type = missing_types.sort.first

                missing_pattern =
                  case missing_type
                  when Types::Tuple then Pattern.new(:tuple, child_patterns, nil, missing_type, {})
                  when Types::Record then Pattern.new(:record, child_patterns, nil, missing_type, {})
                  else raise "Unexpected missing type '#{missing_type}'; expected a tuple or record type"
                  end

                [missing_pattern] + missing_patterns
              end
            end
          end
        end
      end

      def self.useful?(pattern_matrix, pattern_vector, types, debugger: false)
        byebug if debugger
        return true if pattern_matrix.empty?

        raise "The pattern matrix and pattern vector should have the same width" if pattern_matrix.any? { |row| row.count != pattern_vector.count }

        return false if pattern_vector.empty?

        first_pattern, *rest_patterns = pattern_vector
        first_type, *rest_types = types

        case first_pattern.kind
        when :literal
          useful?(specialize_literal(first_pattern, pattern_matrix), rest_patterns, rest_types)
        when :wildcard
          pattern_types = (first_type.is_a?(Types::Union) ? first_type.types : [first_type])

          complete, _ = complete?(pattern_matrix.map(&:first), pattern_types)

          if complete
            pattern_types.any? do |type|
              if type.is_a?(Types::Tuple) || type.is_a?(Types::Record)
                arg_types =
                  if type.is_a?(Types::Tuple)
                    type.types
                  else
                    type.types_hash.sort.map(&:last)
                  end

                wildcards = arg_types.map { |arg_type| Pattern.new(:wildcard, [], first_pattern.node, arg_type, {}) }

                useful?(specialize(type, pattern_matrix), wildcards + rest_patterns, arg_types + rest_types)
              else
                false
              end
            end
          else
            useful?(to_default(pattern_matrix), rest_patterns, rest_types)
          end
        else
          # A constructed pattern like a tuple or record
          (first_pattern.type.is_a?(Types::Union) ? first_pattern.type.types : [first_pattern.type]).any? do |first_pattern_type|
            first_pattern_args = first_pattern_type.is_a?(Types::Record) ? refine_record_args(first_pattern, first_pattern_type) : first_pattern.args
            useful?(specialize(first_pattern_type, pattern_matrix), first_pattern_args + rest_patterns, first_pattern_args.map(&:type) + rest_types)
          end
        end
      end

      # case e # e : { body: String } | { body: String, code: Int | Nil }
      #   { body: b, code: c } -> ... # { body: String, code: Int | Nil }
      #   { body: b, code: nil } -> ... # { body: String, code: Nil }
      #   { body: b } -> ... # { body: String } | { body: String, code: Int }
      #
      # case e # e : (Int, Bool) | (String, Bool)
      #   (x, false) -> ... # (String, Bool)
      #   (0, y) -> ... # (Int, Bool)
      #
      # case e # e : Int | (Int, Int)
      #   (x, y) -> ... # (Int, Int)
      #   x -> ... # Int
      #
      # case e # e : (Int, Int) | { x: Int, y: Int }
      #   { x: x, y: y } -> ... # { x: Int, y: Int }
      #   (x, y) -> ... # (Int, Int)

      def self.specialize(constructed_type, matrix)
        constructed_types = constructed_type.is_a?(Types::Union) ? constructed_type.types : [constructed_type]
        
        new_matrix = []

        matrix.each do |row|
          first_pattern, *rest_patterns = row

          case first_pattern.kind
          when :literal
            # Do nothing
          when :wildcard
            new_matrix << arg_types_from_type(constructed_type).map { |arg_type| Pattern.new(:wildcard, [], first_pattern.node, arg_type, {}) } + rest_patterns
          when :tuple
            if constructed_types.any? { |t| t.is_a?(Types::Tuple) && t.types.count == first_pattern.args.count }
              new_matrix << first_pattern.args + rest_patterns
            end
          when :record
            (first_pattern.type.is_a?(Types::Union) ? first_pattern.type.types : [first_pattern.type]).each do |pattern_type|
              if constructed_types.any? { |t| t.is_a?(Types::Record) && t.types_hash.keys.sort == pattern_type.types_hash.keys.sort }
                first_pattern_args = refine_record_args(first_pattern, constructed_type)
                new_matrix << first_pattern_args + rest_patterns
              end
            end
          else
            # Do nothing
          end
        end

        new_matrix
      end

      def self.specialize_literal(literal_pattern, matrix)
        matrix.map do |row|
          first_pattern, *rest_patterns = row

          case first_pattern.kind
          when :literal
            # Keep the rest of the row if the first pattern matches the literal pattern
            if literal_pattern.type == first_pattern.type && literal_pattern.args.first == first_pattern.args.first
              rest_patterns
            else
              nil
            end
          when :wildcard
            rest_patterns
          else
            nil
          end
        end.compact
      end

      # Returns a tuple [complete?, missing_types]
      def self.complete?(patterns, types)
        types = types.dup
        missing_types = []

        while !types.empty?
          type = types.shift
          missing_types << type if !patterns.any? { |p| p.kind != :literal && p.kind !=:wildcard && p.matches_type?(type) }
        end

        [missing_types.empty?, missing_types]
      end

      def self.to_default(matrix)
        matrix.map do |row|
          first_pattern, *rest_patterns = row
          first_pattern.kind == :wildcard ? rest_patterns : nil
        end.compact
      end

      def self.refine_record_args(pattern, target_type)
        types = pattern.type.is_a?(Types::Union) ? pattern.type.types : [pattern.type]
        min_type_args = types.min_by { |t| t.types_hash.count }.types_hash.keys.sort.zip(pattern.args).to_h

        target_type.types_hash.keys.sort.map do |field|
          min_type_args[field] || Pattern.new(:wildcard, [], pattern.node, Types.combine(types.map { |t| t.types_hash[field] }.compact), {})
        end
      end

      def self.arg_types_from_type(type)
        types = type.is_a?(Types::Union) ? type.types : [type]

        # Check each pair of types to make sure they are all compatible
        #   * A pattern shouldn't match a tuple type and a record type
        #   * A pattern shouldn't match a 2-tuple type and a 3-tuple type
        #   * A pattern shouldn't match two record types with incompatible fields
        types.combination(2).each do |t1, t2|
          if t1.is_a?(Types::Tuple)
            cannot_unify(t1, t2) unless t2.is_a?(Types::Tuple)
            cannot_unify(t1, t2) unless t1.types.count == t2.types.count
          elsif t1.is_a?(Types::Record)
            cannot_unify(t1, t2) unless t2.is_a?(Types::Record)
            cannot_unify(t1, t2) unless (t1.types_hash.keys - t2.types_hash.keys).empty? || (t2.types_hash.keys - t1.types_hash.keys).empty?
          else
            raise "Invalid type in constructed pattern '#{type}'"
          end
        end

        if types.first.is_a?(Types::Tuple)
          types.map(&:types).transpose.map { |arg_types| Types.combine(arg_types) }
        else
          types.min_by { |t| t.types_hash.count }.types_hash.sort.map do |field, _|
            Types.combine(types.map { |t| t.types_hash[field] }.compact)
          end
        end
      end

      def self.cannot_unify(t1, t2)
        raise CannotUnifyTypesError.new("Cannot unify pattern types '#{t1}' and '#{t2}' in '#{type}'")
      end
    end
  end
end
