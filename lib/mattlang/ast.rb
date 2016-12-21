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
      raise "`type` must be a subclass of `Types::Base`" unless type.is_a?(Types::Base)
      @type = type
    end

    def inspect(indent = 0)
      str = '  ' * indent + "(#{term.inspect}"
      str += " : #{[*type].join(' | ')}" if !type.nil?
      
      if !children.nil?
        if children.empty?
          str += ' []'
          str += " #{meta}" if !meta.nil?
        else
          str += " #{meta}" if !meta.nil?
          str += "\n"

          children.each do |c|
            str += c.is_a?(AST) ? "#{c.inspect(indent + 1)}\n" : '  ' * (indent + 1) + "#{c.inspect}\n"
          end

          str += '  ' * indent
        end
      end

      str += ')'

      str
    end
  end
end
