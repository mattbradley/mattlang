module Mattlang
  class Token
    EOF = :eof
    NEWLINE = :newline
    BOOL = :bool
    NIL = :nil
    KEYWORD = :keyword
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

    attr_reader :type, :value, :line, :col

    def initialize(type, value = nil, line:, col:)
      @type = type
      @value = value
      @line = line
      @col = col
    end

    def to_short_s
      if value.nil?
        "(#{type})"
      else
        "(#{type}: #{value.inspect})"
      end
    end

    def to_s
      str = to_short_s
      str += " [#{line}:#{col}]" if line && col
      str
    end
  end
end
