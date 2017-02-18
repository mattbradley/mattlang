module Mattlang
  class AST
    attr_accessor :term, :children, :meta
    attr_reader :type

    def initialize(term, children = nil, type: nil, meta: nil)
      @term = term
      @children = children
      @meta = meta

      self.type = type if !type.nil?
    end

    def type=(type)
      raise "type '#{type.inspect}' must be a subclass of `Types::Base`" unless type.is_a?(Types::Base)
      @type = type
    end

    def inspect(indent = 0)
      str = '  ' * indent + '('

      str += term.is_a?(AST) ? "\n#{term.inspect(indent + 1)}" : term.inspect

      str += " : #{[*type].join(' | ')}" if !type.nil?
      
      if !children.nil?
        if children.empty?
          str += ' []'
          str += " #{meta}" if !meta.nil?
          str += "\n" + '  ' * indent if term.is_a?(AST)
        else
          str += " #{meta}" if !meta.nil?
          str += "\n"

          children.each do |c|
            str += c.is_a?(AST) ? "#{c.inspect(indent + 1)}\n" : '  ' * (indent + 1) + "#{c.inspect}\n"
          end

          str += '  ' * indent
        end
      else
        str += " #{meta}" if !meta.nil?
      end

      str += ')'

      str
    end

    def dup
      AST.new(
        @term.is_a?(Symbol) ? @term : @term.dup,
        @children&.map(&:dup),
        type: @type,
        meta: @meta
      )
    end
  end
end
