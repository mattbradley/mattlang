module Mattlang
  class Codegen
    class PatternMatcher
      include Utils

      def initialize(codegen)
        @codegen = codegen
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
          subject_type, unwrapping_src, subject_register = @codegen.unwrap_nominals(subject_type, subject_register)
          pattern_fields = pattern.children.map(&:term)

          record_types =
            if subject_type.is_a?(Types::Union)
              subject_type.types.reduce(Hash.new { |h, k| h[k] = [] }) do |ts, record_type|
                record_type.types_hash.each do |field, t|
                  ts[field] << t if pattern_fields.include?(field)
                end

                ts
              end.map { |field, ts| [field, Types.union(ts)] }.to_h
            else
              subject_type.types_hash.select { |field, _| pattern_fields.include?(field) }
            end

          pattern.children.reduce([unwrapping_src, {}]) do |(src, matched_registers), inner_pattern|
            type_id_to_field_index = subject_type.deunion.map { |t| [@codegen.type_ids[t], t.canonical_field_order[inner_pattern.term]] }

            if type_id_to_field_index.count == 1
              _, field_index = type_id_to_field_index.first
              pattern_register = "VALUE2FIELDS(#{subject_register})[#{field_index}]"
              index_register = field_index
            else
              index_register = @codegen.new_register
              src += "int #{index_register};\n"

              type_id_to_field_index.each_with_index do |(type_id, field_index), i|
                if i == 0
                  src += "if (TYPEOF(#{subject_register}) == #{type_id}) {\n"
                elsif i == type_id_to_field_index.count - 1
                  src += "else {\n"
                else
                  src += "else if (TYPEOF(#{subject_register}) == #{type_id}) {\n"
                end

                src += indent("#{index_register} = #{field_index};\n")
                src += "}\n"
              end
            end

            inner_src, inner_matches = destructure_match(inner_pattern.children.first, record_types[inner_pattern.term], "VALUE2FIELDS(#{subject_register})[#{index_register}]")
            [src + inner_src, matched_registers.merge(inner_matches)]
          end
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
