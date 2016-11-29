module Mattlang
  class Interpreter
    attr_reader :source, :semantic, :current_frame

    def self.debug(source)
      interpreter = new(source)
      last_value = interpreter.interpret
      puts "Binding: #{interpreter.current_frame.inspect}"
      puts " => #{last_value.inspect} : #{typeof(last_value)}"
      last_value
    end

    def initialize(source)
      kernel = File.read('src/kernel.matt')
      @source = kernel + "\n" + source
      @frames = []
    end

    def interpret
      ast = Parser.new(@source).parse
      @semantic = Semantic.new(ast)
      @semantic.analyze

      @functions = @semantic.global_scope.functions
      @current_frame = {}
      @last_value = execute(@semantic.ast)
    end

    private

    def self.typeof(value)
      case value
      when NilClass then :Nil
      when TrueClass, FalseClass then :Bool
      when Fixnum then :Int
      when Float then :Float
      when String then :String
      else raise "Unknown type for value '#{value.inspect}' with class #{value.class}"
      end
    end

    def push_frame
      @frames.push(@current_frame)
      @current_frame = {}
    end

    def pop_frame
      @current_frame = @frames.pop
    end

    def execute(node)
      case node.term
      when :__top__, :__block__
        execute_block(node)
      when :__if__
        execute_if(node)
      when :__embed__
        execute_embed(node)
      when :__fn__
        # do nothing
      when :__infix__
        # do nothing
      when :'='
        execute_assignment(node)
      else
        execute_expr(node)
      end
    end

    def execute_block(node)
      last_value = nil

      node.children.each do |child|
        last_value = execute(child)
      end

      last_value
    end

    def execute_if(node)
      conditional, then_block, else_block = node.children

      if execute(conditional) == true
        execute(then_block)
      else
        execute(else_block)
      end
    end

    def execute_embed(node)
      obj = Object.new
      @current_frame.each { |k, v| obj.instance_variable_set("@#{k}", v) }
      value = obj.instance_eval(node.children.first.term)

      case node.type
      when :Nil then nil
      when :Bool then value ? true : false
      when :Int then value.to_i
      when :Float then value.to_f
      when :String then value.to_s
      end
    end

    def execute_assignment(node)
      lhs, rhs = node.children

      value = execute(rhs)
      @current_frame[lhs.term] = value
      value
    end

    def execute_expr(node)
      if node.term.is_a?(Symbol)
        if !node.children.nil?
          args = node.children.map { |arg| execute(arg) }
          key = [node.term, args.map { |arg| self.class.typeof(arg) }]
          function = @functions[key]
          execute_function(function, args)
        elsif @current_frame.key?(node.term)
          @current_frame[node.term]
        elsif @functions.key?([node.term, []])
          function = @functions[[node.term, []]]
          execute_function(function, [])
        else
          nil
        end
      else
        node.term
      end
    end

    def execute_function(function, args)
      push_frame

      function.args.map(&:first).zip(args).each { |name, value| @current_frame[name] = value }
      value = execute(function.body)

      pop_frame

      value
    end
  end
end
