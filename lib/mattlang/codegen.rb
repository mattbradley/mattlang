require 'set'
require 'mattlang/codegen/utils'
require 'mattlang/codegen/pattern_matcher'

module Mattlang
  class Codegen
    include Utils

    NAMESPACE_PREFIX = 'mt'
    VALUE_TYPE = "#{NAMESPACE_PREFIX}_Value"

    OPERATOR_NAMES = {
      '@' => 'at',
      '.' => 'dot',
      '+' => 'plus',
      '-' => 'minus',
      '!' => 'bang',
      '^' => 'caret',
      '~' => 'tilde',
      '*' => 'star',
      '/' => 'slash',
      '<' => 'langle',
      '>' => 'rangle',
      '|' => 'pipe',
      '&' => 'amp',
      '=' => 'equal',
      ':' => 'colon',
      '%' => 'percent'
    }

    BUILTIN_TYPES = {
      Nothing: 0,
      Nil: 1,
      Bool: 2,
      Int: 3,
      Float: 4,
      String: 5
    }.map { |k, v| [Types::Simple.new(k), v] }.to_h

    attr_reader :type_ids, :source

    def self.compile(source)
      ast = Parser.new(source).parse
      semantic = Semantic.new(Dir.pwd)
      ast = semantic.analyze_with_kernel(ast)
      codegen = new(semantic.global_scope)
      codegen.generate(ast)
      codegen.source
    end

    def initialize(global_scope)
      @global_scope = global_scope
      @source = %Q(#include "../src/foreign/kernel.h"\n\n)
      @register_index = 0
      @pattern_matcher = PatternMatcher.new(self)
      @bound_registers = {}
    end

    def generate(ast)
      @type_ids = enumerate_types(ast).merge(BUILTIN_TYPES)
      mangle_fn_names(@global_scope)

      prototypes, definitions = generate_fns(@global_scope)
      @source += prototypes + "\n" + definitions

      main, _ = codegen(ast)

      @source += "int main() {\n#{indent(main)}}\n"
    end

    def new_register
      name = "#{NAMESPACE_PREFIX}__v#{@register_index}"
      @register_index += 1
      name
    end

    def unwrap_nominals(type, register)
      if type.is_a?(Types::Union)
        nominals, others = type.types.partition { |t| t.is_a?(Types::Nominal) }

        if nominals.empty?
          [type, '', register]
        else
          unwrapped_register = new_register
          src = "#{VALUE_TYPE} #{unwrapped_register};\n"

          inner_types = []

          nominals.each_with_index do |nominal, i|
            inner_type, inner_src, inner_register = unwrap_nominals(nominal, register)
            inner_types << inner_type

            if i == 0
              src += "if (TYPEOF(#{register}) == #{@type_ids[nominal]}) {\n"
            elsif i == nominals.count - 1 && nominals.count == type.types.count
              src += "else {\n"
            else
              src += "else if (TYPEOF(#{register}) == #{@type_ids[nominal]}) {\n"
            end

            src += indent(inner_src)
            src += indent("#{unwrapped_register} = #{inner_register};\n")
            src += "}\n"
          end

          if nominals.count != type.types.count
            src += "else {\n"
            src += indent("#{unwrapped_register} = #{register};\n")
            src += "}\n"
          end

          [Types.union(inner_types + others), src, unwrapped_register]
        end
      elsif type.is_a?(Types::Nominal) && type.underlying_type
        unwrapped_register = new_register
        src = "#{VALUE_TYPE} #{unwrapped_register} = VALUE2FIELDS(#{register})[0];\n"

        unwrapped_type, unwrapped_src, unwrapped_register = unwrap_nominals(type.underlying_type, unwrapped_register)
        [unwrapped_type, src + unwrapped_src, unwrapped_register]
      else
        [type, '', register]
      end
    end

    private

    def enumerate_types(ast)
      @type_set = Set.new
      visit_types(ast)

      max_builtin_type = BUILTIN_TYPES.values.max
      (@type_set - BUILTIN_TYPES.keys).each_with_index.map { |t, i| [t, i + max_builtin_type + 1] }.to_h
    end

    def visit_types(ast)
      @type_set |= ast.type.ground_types if ast.type

      visit_types(ast.term) if ast.term.is_a?(AST)

      if ast.term != :__type__ && ast.term != :__typealias__ && ast.children
        ast.children.each { |c| visit_types(c) }
      end
    end

    def mangle_fn_names(scope)
      scope.functions.each do |_, fns|
        if fns.count == 1
          fns.first.mangled_name = mangle_fn_name(fns.first.name, fns.first.args.count, scope.module_path, nil) unless fns.first.foreign?
        else
          fns.each_with_index.each { |fn, i| fn.mangled_name = mangle_fn_name(fn.name, fn.args.count, scope.module_path, i) unless fn.foreign? }
        end
      end

      scope.modules.each { |_, mod| mangle_fn_names(mod) }
    end

    def mangle_fn_name(fn_name, arity, module_path, index)
      parts = module_path.map { |m| mangle_name(m) }
      parts << mangle_name(fn_name)
      parts << arity
      parts << index if index
      "#{NAMESPACE_PREFIX}__#{parts.join('_')}"
    end

    def mangle_name(sym)
      s = sym.to_s

      if OPERATOR_NAMES.key?(s[0])
        'op_' + s.chars.map { |c| OPERATOR_NAMES[c] || raise("Don't know how to mangle operator char '#{c}'") }.join('_')
      else
        if ['?', '!'].include?(s[-1])
          "#{s.size - 1}#{s[0...-1]}_#{OPERATOR_NAMES[s[-1]]}"
        else
          "#{s.size}#{s}"
        end
      end
    end

    def generate_fns(scope)
      prototypes = ''
      definitions = ''

      scope.functions.each do |_, fns|
        fns.each do |fn|
          next if fn.foreign?

          arg_srcs = fn.args.map do |arg|
            key = :"#{arg.meta[:scope_id]}_#{arg.term}"
            arg_register = @bound_registers[key] || (@bound_registers[key] = new_register)

            "#{VALUE_TYPE} #{arg_register}"
          end

          body_src, return_var = codegen(fn.body)

          prototypes += "#{VALUE_TYPE} #{fn.mangled_name}(#{arg_srcs.join(', ')});\n"
          definitions += "#{VALUE_TYPE} #{fn.mangled_name}(#{arg_srcs.join(', ')}) {\n#{indent(body_src)}    return #{return_var};\n}\n\n"
        end
      end

      scope.modules.each do |_, mod|
        ps, ds = generate_fns(mod)
        prototypes += ps
        definitions += ds
      end

      [prototypes, definitions]
    end

    def codegen(node)
      case node.term
      when :__top__, :__block__
        codegen_block(node)
      when :__require__
        codegen_require(node)
      when :__module__
        codegen_module(node)
      when :__if__
        codegen_if(node)
      when :__case__
        raise
        #codegen_case(node)
      when :__embed__
        raise "NO EMBEDS!"
      when :__lambda__
        raise
        #codegen_lambda_literal(node)
      when :__fn__, :__foreign_fn__, :__infix__, :__type__, :__typealias__, :__protocol__, :__impl__
        ['', nil]
      when :'='
        codegen_match(node)
      when :'.'
        codegen_access(node)
      when :__list__
        #raise
        #codegen_list(node)
        ['', 'NULL']
      when :__tuple__
        codegen_tuple(node)
      when :__record__
        codegen_record(node)
      when :__record_update__
        raise
        #codegen_record_update(node)
      else
        raise "Unknown term #{node.term}" if node.term.is_a?(Symbol) && node.term.to_s.start_with?("__")
        codegen_expr(node)
      end
    end

    def codegen_match(node)
      pattern, subject = node.children

      src, subject_register = codegen(subject)

      matcher_src, matched_registers = @pattern_matcher.destructure_match(pattern, subject.type, subject_register)

      matched_registers.each do |variable_name, reg|
        key = :"#{node.meta[:scope_id]}_#{variable_name}"
        if (already_bound_reg = @bound_registers[key])
          src += "#{already_bound_reg} = #{reg};\n"
        else
          fresh_register = new_register
          src += "#{VALUE_TYPE} #{fresh_register} = #{reg};\n"
          @bound_registers[key] = reg
        end
      end

      [src + matcher_src, subject_register]
    end

    def codegen_access(node)
      subject, field = node.children

      src, subject_register = codegen(subject)

      subject_type, unwrapping_src, subject_register = unwrap_nominals(subject.type, subject_register)
      src += unwrapping_src

      register = new_register
      src += "#{VALUE_TYPE} #{register};\n"

      if subject_type.is_a?(Types::Union) && subject_type.types.first.is_a?(Types::Record)
        subject_type.types.each_with_index do |record_type, i|
          if i == 0
            src += "if (TYPEOF(#{subject_register}) == #{@type_ids[record_type]}) {\n"
          elsif i == subject_type.types.count - 1
            src += "else {\n"
          else
            src += "else if (TYPEOF(#{subject_register}) == #{@type_ids[record_type]}) {\n"
          end

          src += indent("#{register} = VALUE2FIELDS(#{subject_register})[#{record_type.canonical_field_order[field.term]}];\n")
          src += "}\n"
        end
      elsif subject_type.is_a?(Types::Record)
        src += "#{register} = VALUE2FIELDS(#{subject_register})[#{subject_type.canonical_field_order[field.term]}];\n"
      else
        src += "#{register} = VALUE2FIELDS(#{subject_register})[#{field.term}];\n"
      end

      [src, register]
    end

    def codegen_record(node)
      fields_register = new_register
      src = "#{VALUE_TYPE}* #{fields_register} = ALLOC_FIELDS(#{node.children.count});\n"

      node.children.each do |child|
        child_expr = child.children.first
        child_src, child_register = codegen(child_expr)
        src += child_src
        src += "#{fields_register}[#{node.type.canonical_field_order[child.term]}] = #{child_register};\n"
      end

      register = new_register
      src += "#{VALUE_TYPE} #{register} = MAKE_VALUE(#{@type_ids[node.type]}, #{fields_register});\n"

      [src, register]
    end

    def codegen_tuple(node)
      fields_register = new_register
      src = "#{VALUE_TYPE}* #{fields_register} = ALLOC_FIELDS(#{node.children.count});\n"

      node.children.each_with_index do |child, i|
        child_src, child_register = codegen(child)
        src += child_src
        src += "#{fields_register}[#{i}] = #{child_register};\n"
      end

      register = new_register
      src += "#{VALUE_TYPE} #{register} = MAKE_VALUE(#{@type_ids[node.type]}, #{fields_register});\n"

      [src, register]
    end

    def codegen_if(node)
      conditional, then_block, else_block = node.children

      src, conditional_result = codegen(conditional)

      if_result = new_register
      src += "#{VALUE_TYPE} #{if_result};\n"

      node.meta[:bindings].each do |variable|
        key = :"#{node.meta[:scope_id]}_#{variable}"

        if node.meta[:nil_bindings].include?(variable)
          nil_register = new_register
          src += "#{VALUE_TYPE} #{nil_register} = NIL_VALUE;\n"
          @bound_registers[key] = nil_register
        elsif !@bound_registers.key?(key)
          fresh_register = new_register
          src += "#{VALUE_TYPE} #{fresh_register};\n"
          @bound_registers[key] = fresh_register
        end
      end

      then_src, then_result = codegen(then_block)
      else_src, else_result = codegen(else_block)

      src += "if (VALUE2BOOL(#{conditional_result})) {\n"
      src += indent(then_src);
      src += indent("#{if_result} = #{then_result};\n")
      src += "} else {\n"
      src += indent(else_src);
      src += indent("#{if_result} = #{else_result};\n")
      src += "}\n"

      [src, if_result]
    end

    def codegen_require(node)
      # TODO: don't require the same file twice?
      _, require_ast = node.children
      codegen(require_ast)
    end

    def codegen_module(node)
      _, module_ast = node.children
      codegen(module_ast)
    end

    def codegen_block(node)
      srcs = node.children.map { |c| codegen(c) }
      src = srcs.map(&:first).join
      [src, srcs.last.last]
    end

    # Not worrying about lambdas at the moment.
    # If there are children, it's a fn call.
    # If not, it's a variable reference.
    def codegen_expr(node)
      if node.term.is_a?(Symbol) && ('A'..'Z').include?(node.term[0]) # Type constructor
        raise "Expected constructor to have children" if node.children.nil?

        if node.children.empty?
          register = new_register
          src = "#{VALUE_TYPE} #{register} = MAKE_VALUE(#{@type_ids[node.type]}, NULL);\n"
        else
          fields_register = new_register
          src = "#{VALUE_TYPE}* #{fields_register} = ALLOC_FIELDS(1);\n"

          arg_src, arg_register = codegen(node.children.first)
          src += arg_src
          src += "#{fields_register}[0] = #{arg_register};\n"

          register = new_register
          byebug if @type_ids[node.type].nil?
          src += "#{VALUE_TYPE} #{register} = MAKE_VALUE(#{@type_ids[node.type]}, #{fields_register});\n"

          [src, register]
        end
      elsif node.children.nil? # Variable
        if node.term.is_a?(Symbol)
          key = :"#{node.meta[:scope_id]}_#{node.term}"
          byebug unless @bound_registers.key?(key)
          raise "No register is bound to variable '#{node.term}'" unless @bound_registers.key?(key)

          ['', @bound_registers[key]]
        else
          convert_constant_expr(node)
        end
      else # Fn call
        byebug if node.term == :foo
        arg_srcs = node.children.map { |c| codegen(c) }
        src = arg_srcs.map(&:first).join
        arg_registers = arg_srcs.map(&:last)

        register = new_register
        src += "#{VALUE_TYPE} #{register};\n"

        first, *rest = node.children.map(&:type).map { |arg_type| deconstruct_type(arg_type) }
        type_combos = (first.nil? ? [[]] : first.product(*rest))

        fns = node.meta[:fns]

        if type_combos.count == 1
          fn = fns[type_combos.first].first

          arg_registers = type_combos.first.zip(fn.args.map(&:type)).zip(arg_registers).map do |(type, target_type), arg_register|
            inner_src, unwrapped_register = unwrap(type, target_type, arg_register)
            src += inner_src
            unwrapped_register
          end

          src += "#{register} = #{fn.mangled_name}(#{arg_registers.join(', ')});\n"
        else
          type_combos.each_with_index do |types, combo_i|
            fn = fns[types].first

            if combo_i == type_combos.size - 1
              src += "else "
            else
              conditions = types.each_with_index.map { |t, i| "TYPEOF(#{arg_registers[i]}) == #{@type_ids[t]}" }

              src += combo_i == 0 ? 'if ' : 'else if '
              src += "(#{conditions.join(' && ')}) "
            end

            src += "{\n"

            unwrapped_arg_registers = types.zip(fn.args.map(&:type)).zip(arg_registers).map do |(type, target_type), arg_register|
              inner_src, unwrapped_register = unwrap(type, target_type, arg_register)
              src += inner_src
              unwrapped_register
            end

            src += "#{register} = #{fn.mangled_name}(#{unwrapped_arg_registers.join(', ')});\n"

            src += "}\n"
          end
        end

        [src, register]
      end
    end

    def convert_constant_expr(node)
      src =
        case node.type.type_atom
        when :Nil then "NIL_VALUE"
        when :Bool then "BOOL2VALUE(#{node.term ? '1' : '0'})"
        when :Int then "INT2VALUE(#{node.term})"
        when :Float then "DOUBLE2VALUE(#{node.term})"
        when :String then "STR2VALUE(#{node.term.inspect})"
        else raise "Don't know how to convert #{node.inspect}"
        end

      reg = new_register
      ["#{VALUE_TYPE} #{reg} = #{src};\n", reg]
    end

    def deconstruct_type(type)
      case type
      when Types::Union
        type.types.map { |t| deconstruct_type(t) }.flatten
      else
        [type]
      end
    end

    def unwrap(type, target_type, register)
      if type.is_a?(Types::Nominal) && !type.underlying_type.nil? && target_type.subtype?(type.underlying_type, nil, true)
        unwrapped_register = new_register
        src = "#{VALUE_TYPE} #{unwrapped_register};\n"
        src += "#{unwrapped_register} = VALUE2FIELDS(#{register})[0];\n"

        if type.underlying_type.is_a?(Types::Union)
          type.underlying_type.types.each_with_index do |inner_type, i|
            if i == 0
              src += "if (TYPEOF(#{unwrapped_register}) == #{@type_ids[inner_type]}) {\n"
            elsif i == type.underlying_type.types.count - 1
              src += "else {\n"
            else
              src += "else if (TYPEOF(#{unwrapped_register}) == #{@type_ids[inner_type]}) {\n"
            end

            inner_src, inner_register = unwrap(inner_type, target_type, unwrapped_register)
            src += indent(inner_src)
            src += indent("#{unwrapped_register} = #{inner_register};\n") if unwrapped_register != inner_register

            src += "}\n"
          end

          [src, unwrapped_register]
        else
          inner_src, unwrapped_register = unwrap(type.underlying_type, target_type, unwrapped_register)
          [src + inner_src, unwrapped_register]
        end
      else
        ['', register]
      end
    end
  end
end
