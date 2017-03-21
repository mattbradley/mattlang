require "mattlang/semantic/pattern"

# Modeled after the algorithms in "Warnings for pattern matching" (http://moscova.inria.fr/~maranget/papers/warn/warn.pdf) and the pattern matching
# implementation of Elm's compiler (https://github.com/elm-lang/elm-compiler/blob/a672aa74e6c23757b50fe6de5b24cb945c5be9a2/src/Nitpick/PatternMatches.hs).
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

      class MissingPatternsError < Error
        def self.title; 'Missing Patterns'; end
      end

      def self.destructure_match(pattern, candidate)
        case pattern.term
        when :__tuple__
          raise NoMatchError.new("Cannot match a tuple with the non-tuple type '#{candidate}'", pattern) if !candidate.is_a?(Types::Tuple)
          raise NoMatchError.new("Cannot match a #{pattern.children.count}-tuple with the #{candidate.types.count}-tuple type '#{candidate}'", pattern) if pattern.children.count != candidate.types.count

          bindings = pattern.children.zip(candidate.types).reduce({}) do |matches, (inner_pattern, inner_type)|
            new_matches = destructure_match(inner_pattern, inner_type)
            matches.merge(new_matches) { |k, _, _| raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k) }
          end

          pattern.type = combine_if_not_nil(pattern.type, Types::Tuple.new(pattern.children.map(&:type)))
          bindings
        when :__record__
          raise NoMatchError.new("Cannot match a record with the non-record type '#{candidate}'", pattern) if !candidate.is_a?(Types::Record)

          bindings = pattern.children.reduce({}) do |matches, field_node|
            raise NoMatchError.new("Cannot match a record with field '#{field_node.term}' missing in record type '#{candidate}'", pattern) if !candidate.types_hash.key?(field_node.term)

            new_matches = destructure_match(field_node.children.first, candidate.types_hash[field_node.term])
            matches.merge(new_matches) { |k, _, _| raise InvalidPatternError.new("Cannot bind to the variable '#{k}' more than once in the same pattern", pattern) if matches.key?(k) }
          end

          types_hash = pattern.children.map do |field_node|
            field_node.type = combine_if_not_nil(field_node.type, field_node.children.first.type)
            [field_node.term, field_node.type]
          end.to_h

          pattern.type = combine_if_not_nil(pattern.type, Types::Record.new(types_hash))
          bindings
        when :'::'
          # Removing this pattern for now. Maybe it can be added back in the future
          # if there is a way to type check an empty list versus an populated list.
          raise InvalidPatternError.new("Cons destructuring is not supported")
          raise NoMatchError.new("Cannot perform a cons match with the non-list type '#{candidate}'", pattern) if !is_a_list?(candidate)

          head, tail = pattern.children

          bindings =
            destructure_match(head, candidate.type_parameters.first)
            .merge(destructure_match(tail, Types::Union.new([candidate, Types::Generic.new(:List, [Types.nothing])]))) do |k, _, _|
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

      def self.generate_case_patterns(pattern_nodes, type)
        patterns = pattern_nodes.map { |node| build_pattern(node, type) }
        matrix = []

        patterns.each do |pattern|
          if useful?(matrix, [pattern], [type])
            matrix << [pattern]
          else
            raise RedundantPatternError.new("This pattern is redundant and must be removed.", pattern.node)
          end
        end

        if (missing_patterns = exhaustive?(matrix, [type]))
          missing_msg = missing_patterns.map { |p| "    #{p}" }.join("\n")
          raise MissingPatternsError.new("The case does not have branches for all possible values for type '#{type}'.\nHere are some example patterns that aren't matched:\n\n#{missing_msg}")
        end

        patterns
      end

      private

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

          common_fields = record_types.map(&:types_hash).map(&:keys).reduce(&:&)

          possible_patterns = record_types.map do |record_type|
            begin
              ordered_fields = record_type.types_hash.sort
              built_type = {}

              child_patterns =
                ordered_fields.map do |field, child_type|
                  # A record pattern only includes args that are common across all record types in the union.
                  # A wildcard is used if a field is missing from the pattern node.
                  if (child = node.children.find { |c| c.term == field })
                    child_pattern = build_pattern(child.children.first, child_type)
                    built_type[field] = child_pattern.type

                    child_pattern
                  elsif common_fields.include?(field)
                    built_type[field] = child_type
                    Pattern.new(:wildcard, [], nil, child_type, {})
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
        when :__list__
          raise InvalidPatternError.new("A case pattern cannot include a non-empty list") unless node.children.empty?

          list_types = (type.is_a?(Types::Union) ? type.types : [type]).select { |t| t.is_a?(Types::Generic) && t.type_atom == :List }
          raise NoMatchError.new("Cannot match the list pattern with the type '#{type}'", node) if list_types.empty?

          Pattern.new(:empty, [], node, type, {})
        when :'::'
          list_types = (type.is_a?(Types::Union) ? type.types : [type]).select { |t| is_a_list?(t) }
          raise NoMatchError.new("Cannot match the :: pattern with the type '#{type}'", node) if list_types.empty?

          possible_patterns = list_types.map do |list_type|
            begin
              head_node, tail_node = node.children
              head_pattern = build_pattern(head_node, list_type.type_parameters.first)
              tail_pattern = build_pattern(tail_node, list_type)
              pattern_type = Types::Generic.new(:List, [Types.combine([head_pattern.type, tail_pattern.type.type_parameters.first])])

              Pattern.new(:cons, [head_pattern, tail_pattern], node, pattern_type, merge_bindings([head_pattern, tail_pattern]))
            rescue NoMatchError
              nil
            end
          end.compact

          raise NoMatchError.new("Cannot match this pattern with the type '#{type}'", node) if possible_patterns.empty?

          possible_patterns.reduce(&:merge)
        else
          raise InvalidPatternError.new("A case pattern cannot include function calls", node) if !node.children.nil?

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

      def self.is_a_list?(type)
        type.is_a?(Types::Generic) && type.type_atom == :List && type.module_path.empty? && type.type_parameters.count == 1
      end

      def self.combine_if_not_nil(type1, type2)
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
      def self.exhaustive?(matrix, types)
        if matrix.empty?
          types.map { |arg_type| Pattern.new(:wildcard, [], nil, arg_type, {}) }
        elsif types.empty?
          nil
        else
          first_type, *rest_types = types
          pattern_types = (first_type.is_a?(Types::Union) ? first_type.types : [first_type])

          complete, missing_types = complete?(matrix.map(&:first), pattern_types)

          if complete
            pattern_types.each do |pattern_type|
              if pattern_type.is_a?(Types::Tuple) || pattern_type.is_a?(Types::Record)
                arg_types =
                  case pattern_type
                  when Types::Tuple then pattern_type.types
                  when Types::Record then pattern_type.types_hash.sort.map(&:last)
                  end

                if (missing_patterns = exhaustive?(specialize(pattern_type, matrix), arg_types + rest_types))
                  child_patterns = missing_patterns[0...arg_types.count]
                  rest_patterns = missing_patterns[arg_types.count..-1]

                  missing_pattern =
                    case pattern_type
                    when Types::Tuple then Pattern.new(:tuple, child_patterns, nil, pattern_type, {})
                    when Types::Record then Pattern.new(:record, child_patterns, nil, pattern_type, {})
                    end

                  return [missing_pattern] + rest_patterns
                end
              elsif pattern_type.is_a?(Types::Generic)
                if is_a_list?(pattern_type)
                  if (missing_patterns = exhaustive?(specialize(pattern_type, matrix, :cons), [pattern_type.type_parameters.first, pattern_type] + rest_types))
                    return [Pattern.new(:cons, missing_patterns[0...2], nil, pattern_type, {})] + missing_patterns[2..-1]
                  elsif (missing_patterns = exhaustive?(specialize(pattern_type, matrix, :empty), rest_types))
                    return [Pattern.new(:empty, [], nil, pattern_type, {})] + missing_patterns
                  end
                else
                  raise "Didn't expect generic type '#{pattern_type}' to have a complete constructor"
                end
              elsif pattern_type.is_a?(Types::Simple)
                if pattern_type.type_atom == :Bool
                  [true, false].each do |bool_value|
                    bool_literal = Pattern.new(:literal, [bool_value], nil, pattern_type, {})
                    missing_patterns = exhaustive?(specialize_literal(bool_literal, matrix), rest_types)

                    return [bool_literal] + missing_patterns if !missing_patterns.nil?
                  end
                elsif pattern_type.type_atom == :Nil
                  nil_literal = Pattern.new(:literal, [nil], nil, pattern_type, {})
                  missing_patterns = exhaustive?(specialize_literal(nil_literal, matrix), rest_types)

                  return [nil_literal] + missing_patterns if !missing_patterns.nil?
                else
                  raise "Didn't expect simple type '#{pattern_type}' to have a complete constructor"
                end
              else
                raise "Didn't expect type '#{pattern_type}' to have a complete constructor"
              end
            end

            nil
          else
            missing_patterns = exhaustive?(to_default(matrix), rest_types)

            if missing_patterns.nil?
              nil
            else
              more_missing_patterns =
                missing_types.map do |missing_type|
                  case missing_type
                  when Types::Tuple
                    child_patterns = missing_type.types.map { |t| Pattern.new(:wildcard, [], nil, t, {}) }
                    Pattern.new(:tuple, child_patterns, nil, missing_type, {})
                  when Types::Record
                    child_patterns = missing_type.types_hash.sort.map(&:last).map { |t| Pattern.new(:wildcard, [], nil, t, {}) }
                    Pattern.new(:record, child_patterns, nil, missing_type, {})
                  when Types::Generic
                    if is_a_list?(missing_type)
                      missing_list_patterns = []

                      if !matrix.map(&:first).any? { |p| p.kind == :empty && missing_type.subtype?(p.type) }
                        missing_list_patterns << Pattern.new(:empty, [], nil, missing_type, {})
                      end

                      if !matrix.map(&:first).any? { |p| p.kind == :cons && missing_type.subtype?(p.type) }
                        missing_list_patterns << Pattern.new(:cons, [Pattern.new(:wildcard, [], nil, missing_type.type_parameters.first, {}), Pattern.new(:wildcard, [], nil, missing_type, {})], nil, missing_type, {})
                      elsif (missing_cons_patterns = exhaustive?(specialize(missing_type, matrix, :cons), [missing_type.type_parameters.first, missing_type]))
                        missing_list_patterns << Pattern.new(:cons, missing_cons_patterns, nil, missing_type, {})
                      end

                      missing_list_patterns
                    else
                      Pattern.new(:wildcard, [], nil, missing_type, {})
                    end
                  else
                    child_patterns = matrix.map(&:first).select { |p| p.kind == :literal && missing_type.subtype?(p.type) }.sort_by(&:type)

                    if child_patterns.empty?
                      if missing_type.is_a?(Types::Simple) && missing_type.type_atom == :Nil
                        Pattern.new(:literal, [nil], nil, missing_type, {})
                      elsif missing_type.is_a?(Types::Simple) && missing_type.type_atom == :Bool
                        [
                          Pattern.new(:literal, [true], nil, missing_type, {}),
                          Pattern.new(:literal, [false], nil, missing_type, {})
                        ]
                      else
                        Pattern.new(:wildcard, [], nil, missing_type, {})
                      end
                    else
                      if missing_type.is_a?(Types::Simple) && missing_type.type_atom == :Bool
                        Pattern.new(:literal, [child_patterns.first.args.first == false], nil, missing_type, {})
                      else
                        Pattern.new(:complement, child_patterns, nil, missing_type, {})
                      end
                    end
                  end
                end.flatten

              this_missing_pattern =
                if more_missing_patterns.count > 1
                  Pattern.new(:or, more_missing_patterns, nil, nil, {})
                else
                  more_missing_patterns.first
                end

              [this_missing_pattern] + missing_patterns
            end
          end
        end
      end

      def self.useful?(pattern_matrix, pattern_vector, types)
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

          useful_types = pattern_types.map do |pattern_type|
            complete, _ = complete?(pattern_matrix.map(&:first), [pattern_type])

            if complete
              if pattern_type.is_a?(Types::Tuple) || pattern_type.is_a?(Types::Record)
                arg_types =
                  if pattern_type.is_a?(Types::Tuple)
                    pattern_type.types
                  else
                    pattern_type.types_hash.sort.map(&:last)
                  end

                wildcards = arg_types.map { |arg_type| Pattern.new(:wildcard, [], first_pattern.node, arg_type, {}) }

                # Rebuild this pattern type from the child wildcards, since they could
                # have had some types eliminated
                if useful?(specialize(pattern_type, pattern_matrix), wildcards + rest_patterns, arg_types + rest_types)
                  if pattern_type.is_a?(Types::Tuple)
                    Types::Tuple.new(wildcards.map(&:type))
                  else
                    Types::Record.new(pattern_type.types_hash.sort.keys.zip(wildcards.map(&:type)).to_h)
                  end
                end
              elsif pattern_type.is_a?(Types::Simple) && pattern_type.type_atom == :Bool
                pattern_type if [true, false].any? do |bool_value|
                  useful?(specialize_literal(Pattern.new(:literal, [bool_value], nil, pattern_type, {}), pattern_matrix), rest_patterns, rest_types)
                end
              elsif pattern_type.is_a?(Types::Simple) && pattern_type.type_atom == :Nil
                pattern_type if useful?(specialize_literal(Pattern.new(:literal, [nil], nil, pattern_type, {}), pattern_matrix), rest_patterns, rest_types)
              elsif is_a_list?(pattern_type)
                if useful?(specialize(pattern_type, pattern_matrix, :empty), rest_patterns, rest_types)
                  pattern_type
                else
                  arg_types = [pattern_type.type_parameters.first, pattern_type]
                  wildcards = arg_types.map { |t| Pattern.new(:wildcard, [], nil, t, {}) }
                  pattern_type if useful?(specialize(pattern_type, pattern_matrix, :cons), wildcards + rest_patterns, arg_types + rest_types)
                end
              else
                raise "Cannot specialize type '#{pattern_type}'"
              end
            else
              pattern_type if useful?(to_default(pattern_matrix), rest_patterns, rest_types)
            end
          end.compact

          if useful_types.any?
            # Type elimination: if this wildcard is a named variable (meaning not an underscore),
            # then rebind the variable to the union of the useful types. Types that would make
            # this wildcard pattern useless can be eliminated since any values of those types
            # would have been caught by previous patterns.
            new_type = Types.combine(useful_types)
            first_pattern.bindings[first_pattern.bindings.keys.first] = new_type if !first_pattern.bindings.empty?
            first_pattern.type = new_type

            true
          else
            false
          end
        else # A constructed pattern like a tuple or record
          useful_patterns =
            (first_pattern.type.is_a?(Types::Union) ? first_pattern.type.types : [first_pattern.type]).map do |first_pattern_type|
              duped_first_pattern = first_pattern.dup
              duped_first_pattern.reconcile_with_type(first_pattern_type)
              first_pattern_args = first_pattern_type.is_a?(Types::Record) ? specialize_record_args(duped_first_pattern, first_pattern_type) : duped_first_pattern.args

              if useful?(specialize(first_pattern_type, pattern_matrix, duped_first_pattern.kind), first_pattern_args + rest_patterns, first_pattern_args.map(&:type) + rest_types)
                duped_first_pattern.bindings = merge_bindings(first_pattern_args)
                duped_first_pattern
              end
            end.compact

          if useful_patterns.any?
            merged_useful_patterns = useful_patterns.reduce(:merge)
            first_pattern.args = merged_useful_patterns.args
            first_pattern.bindings = merged_useful_patterns.bindings
            first_pattern.type = merged_useful_patterns.type

            true
          else
            false
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

      def self.specialize(constructed_type, matrix, kind = nil)
        new_matrix = []

        matrix.each do |row|
          first_pattern, *rest_patterns = row

          case first_pattern.kind
          when :literal
            # Do nothing
          when :wildcard
            new_matrix << arg_types_from_type(constructed_type, kind).map { |arg_type| Pattern.new(:wildcard, [], first_pattern.node, arg_type, {}) } + rest_patterns
          when :tuple
            if constructed_type.is_a?(Types::Tuple) && constructed_type.types.count == first_pattern.args.count
              new_matrix << first_pattern.args + rest_patterns
            end
          when :record
            (first_pattern.type.is_a?(Types::Union) ? first_pattern.type.types : [first_pattern.type]).each do |pattern_type|
              if constructed_type.is_a?(Types::Record) && constructed_type.types_hash.keys.sort == pattern_type.types_hash.keys.sort
                first_pattern_args = specialize_record_args(first_pattern, constructed_type)
                new_matrix << first_pattern_args + rest_patterns
              end
            end
          when :empty
            if kind == :empty
              new_matrix << rest_patterns
            end
          when :cons
            if kind == :cons && is_a_list?(constructed_type)
              new_matrix << first_pattern.args + rest_patterns
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
        missing_types = types.select do |type|
          if is_a_list?(type)
            !patterns.any? { |p| p.kind == :cons && type.subtype?(p.type) } || 
              !patterns.any? { |p| p.kind == :empty && type.subtype?(p.type) }
          elsif type.is_a?(Types::Simple) && type.type_atom == :Bool
            ![true, false].all? do |bool_value|
              patterns.any? { |p| p.kind == :literal && p.type == type && p.args.first == bool_value }
            end
          elsif type.is_a?(Types::Simple) && type.type_atom == :Nil
            !patterns.any? { |p| p.kind == :literal && p.type == type && p.args.first == nil }
          else
            !patterns.any? { |p| p.kind != :literal && p.kind !=:wildcard && p.matches_type?(type) }
          end
        end

        [missing_types.empty?, missing_types]
      end

      def self.to_default(matrix)
        matrix.map do |row|
          first_pattern, *rest_patterns = row
          first_pattern.kind == :wildcard ? rest_patterns : nil
        end.compact
      end

      def self.specialize_record_args(pattern, target_type)
        types = pattern.type.is_a?(Types::Union) ? pattern.type.types : [pattern.type]
        min_type_args = types.min_by { |t| t.types_hash.count }.types_hash.keys.sort.zip(pattern.args).to_h

        target_type.types_hash.keys.sort.map do |field|
          min_type_args[field] || Pattern.new(:wildcard, [], pattern.node, Types.combine(types.map { |t| t.types_hash[field] }.compact), {})
        end
      end

      def self.arg_types_from_type(type, kind = nil)
        case type
        when Types::Tuple
          type.types
        when Types::Record
          type.types_hash.sort.values
        else
          if is_a_list?(type)
            if kind == :empty
              []
            else
              [type.type_parameters.first, type]
            end
          else
            raise "Cannot get the arg types for type '#{type}'"
          end
        end
      end
    end
  end
end
