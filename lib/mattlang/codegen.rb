require 'set'

module Mattlang
  class Codegen
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
      Nil: 0,
      Bool: 1,
      Int: 2,
      Float: 3,
      String: 4
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
    end

    def generate(ast)
      @type_ids = enumerate_types(ast).merge(BUILTIN_TYPES)
      mangle_fn_names(@global_scope)

      prototypes, definitions = generate_fns(@global_scope)
      @source += prototypes + "\n" + definitions

      main, _ = codegen(ast)

      @source += "int main() {\n#{indent(main)}}\n"
    end
    
    private

    def new_register
      name = "#{NAMESPACE_PREFIX}__t#{@register_index}"
      @register_index += 1
      name
    end

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

          body_src, return_var = codegen(fn.body)
          
          arg_srcs = fn.args.map { |arg| "#{VALUE_TYPE} #{arg.first}" }

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
        codegen_embed(node)
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
        raise
        #codegen_list(node)
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

    def codegen_access(node)
      raise
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

    # Not worrying about lambdas, tuples, or records at the moment.
    # If there are children, it's a fn call.
    # If not, it's a variable reference.
    def codegen_expr(node)
      if node.children.nil? # Variable
        if node.term.is_a?(Symbol)
          ['', node.term]
        else
          ['', convert_constant_expr(node)]
        end
      else # Fn call
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
          src += "#{register} = #{fn.mangled_name}(#{arg_registers.join(', ')});\n"
        else
          type_combos.each_with_index do |types, combo_i|
            fn = fns[types].first

            if combo_i == type_combos.size - 1
              src += 'else '
            else
              conditions = types.each_with_index.map { |t, i| "TYPEOF(#{arg_registers[i]}) == #{@type_ids[t]}" }

              src += combo_i == 0 ? 'if ' : 'else if '
              src += "(#{conditions.join(' && ')}) "
            end

            src += "{ #{register} = #{fn.mangled_name}(#{arg_registers.join(', ')}); }\n"
          end
        end

        [src, register]
      end
    end

    def convert_constant_expr(node)
      case node.type.type_atom
      when :Nil then "NIL_VALUE"
      when :Bool then "BOOL2VALUE(#{node.term ? '1' : '0'})"
      when :Int then "INT2VALUE(#{node.term})"
      when :Float then "DOUBLE2VALUE(#{node.term})"
      when :String then "STR2VALUE(#{node.term.inspect})"
      else raise "Don't know how to convert #{node.inspect}"
      end
    end

    def deconstruct_type(type)
      case type
      when Types::Union
        type.types.map { |t| deconstruct_type(t) }.flatten
      else
        [type]
      end
    end

    def indent(src)
      src.lines.map { |l| "    #{l}" }.join
    end
  end
end
