module Mattlang
  class Token
    EOF = :eof
    NEWLINE = :newline
    BOOL = :bool
    NIL = :nil
    KEYWORD_IF = :keyword_if
    KEYWORD_END = :keyword_end
    KEYWORD_ELSE = :keyword_else
    KEYWORD_ELSIF = :keyword_elsif
    KEYWORD_RETURN = :keyword_return
    KEYWORD_FN = :keyword_fn
    KEYWORD_INFIX = :keyword_infix
    LPAREN = :lparen
    LPAREN_ARG = :lparen_arg
    RPAREN = :rparen
    LBRACKET = :lbracket
    RBRACKET = :rbracket
    SEMICOLON = :semicolon
    COMMA = :comma
    IDENTIFIER = :identifier
    FLOAT = :float
    INT = :int
    STRING = :string
    OPERATOR = :operator

    attr_reader :type, :value, :meta, :line, :col

    def initialize(type, value = nil, line:, col:, meta: nil)
      @type = type
      @value = value
      @line = line
      @col = col
      @meta = meta
    end

    def to_short_s
      str =
        if value.nil?
          "(#{type}"
        else
          "(#{type}: #{value.inspect}"
        end

      str += ", #{meta}" unless meta.nil?
      str += ')'
      str
    end

    def to_s
      str = to_short_s
      str += "[#{line}:#{col}]" if line && col
      str
    end
  end
end
