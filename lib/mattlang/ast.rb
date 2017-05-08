module Mattlang
  class AST
    attr_accessor :term, :children, :meta
    attr_reader :type, :token

    def initialize(term, children = nil, type: nil, meta: nil, token: nil)
      @term = term
      @children = children
      @meta = meta
      @token = token

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

      cleaned_meta = meta.nil? ? {} : meta.reject { |k, v| k == :fns || v.is_a?(Semantic::Scope) || v.is_a?(Semantic::Protocol) }
      
      if !children.nil?
        if children.empty?
          str += ' []'
          str += " #{cleaned_meta}" if cleaned_meta.any?
          str += "\n" + '  ' * indent if term.is_a?(AST)
        else
          str += " #{cleaned_meta}" if cleaned_meta.any?
          str += "\n"

          children.each do |c|
            str += c.is_a?(AST) ? "#{c.inspect(indent + 1)}\n" : '  ' * (indent + 1) + "#{c.inspect}\n"
          end

          str += '  ' * indent
        end
      else
        str += " #{cleaned_meta}" if cleaned_meta.any?
      end

      str += ')'

      str
    end

    def dup
      AST.new(
        (@term.dup rescue @term),
        @children&.map { |c| c.dup rescue c },
        type: @type,
        meta: @meta
      )
    end
  end
end
