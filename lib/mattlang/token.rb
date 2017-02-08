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
    KEYWORD_MODULE = :keyword_module
    KEYWORD_REQUIRE = :keyword_require
    KEYWORD_FN = :keyword_fn
    KEYWORD_INFIX = :keyword_infix
    LPAREN = :lparen
    LPAREN_ARG = :lparen_arg
    RPAREN = :rparen
    LBRACKET = :lbracket
    RBRACKET = :rbracket
    LBRACE = :lbrace
    RBRACE = :rbrace
    SEMICOLON = :semicolon
    COMMA = :comma
    IDENTIFIER = :identifier
    FLOAT = :float
    INT = :int
    STRING = :string
    EMBED = :embed
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
