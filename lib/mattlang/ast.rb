module Mattlang
  class AST
    attr_accessor :term, :children, :meta

    def initialize(term, children = nil, meta: nil)
      @term = term
      @children = children
      @meta = meta
    end

    def inspect(indent = 0)
      str = '  ' * indent + "(#{term.inspect}"
      
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
    rescue => e
      puts @term
      puts @children
      exit
    end
  end
end
