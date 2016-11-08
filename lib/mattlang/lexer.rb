module Mattlang
  class Token
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

  class Lexer
    attr_reader :current_char

    LITERALS = {
      'nil' => :nil,
      'true' => [:bool, true],
      'false' => [:bool, false]
    }

    KEYWORDS = {
      'nil' => :nil,
      'true' => [:bool, true],
      'false' => [:bool, false],
      'fn' => :fn,
      'end' => :end,
      'if' => :if,
      'else' => :else,
      'return' => :return
    }

    PUNCTUATION_TYPES = {
      '(' => :lparen,
      ')' => :rparen,
      '[' => :lbracket,
      ']' => :rbracket,
      ';' => :semicolon,
      ',' => :comma
    }

    OPERATOR_CHARS = %w(@ . + - ! ^ ~ * / < > | & = : %)

    def self.tokenize(source)
      lexer = new(source)
      tokens = []
      loop do
        t = lexer.next_token
        tokens << t
        break if t.type == :eof
      end
      tokens
    end

    def self.annotate(source)
      tokens = tokenize(source)
      puts source.lines
        .map(&:strip)
        .reject(&:empty?)
        .zip(
          tokens
          .group_by(&:line)
          .values
          .map { |ts| ts.map(&:to_short_s).join(' ') }
        )
        .map { |s| s << "\n" }
    end

    def initialize(source)
      @source = source
      @pos = 0
      @line = 0
      @col = 0
      @current_char = @source[@pos]
      @buffered_tokens = []
    end

    def advance
      @pos += 1
      @col += 1

      if newline?(@current_char)
        @line += 1
        @col = 0
      end

      @current_char = @source[@pos]
    end

    def next_char
      @source[@pos + 1]
    end

    def next_token
      return @buffered_tokens.shift if @buffered_tokens.any?

      if current_char.nil?
        Token.new(:eof, line: @line, col: @col)
      elsif whitespace?(current_char)
        skip_whitespace
        next_token
      elsif newline?(current_char)
        token = Token.new(:newline, line: @line, col: @col)
        advance while newline?(current_char)
        token
      elsif identifier_start?(current_char)
        build_identifier
      elsif digit?(current_char)
        build_number
      elsif string_char?(current_char)
        build_string
      elsif (punctuation = PUNCTUATION_TYPES[current_char])
        token = Token.new(punctuation, line: @line, col: @col)
        advance
        token
      elsif operator_char?(current_char)
        build_operator
      else
        raise "Unknown token '#{current_char}'"
      end
    end

    private

    def build_identifier
      id = ''
      line = @line
      col = @col

      while identifier_char?(current_char)
        id += current_char
        advance
      end

      if (literal = LITERALS[id])
        Token.new(*literal, line: line, col: col)
      elsif (keyword = KEYWORDS[id])
        Token.new(:keyword, keyword, line: line, col: col)
      else
        if current_char == '('
          @buffered_tokens << Token.new(:lparen_arg, line: @line, col: @col)
          advance
        end

        Token.new(:identifier, id, line: line, col: col)
      end
    end

    def build_number
      line = @line
      col = @col

      num = number_part

      if current_char == '.' && digit?(next_char)
        num += current_char
        advance
        num += number_part
        Token.new(:float, num.to_f, line: line, col: col)
      else
        Token.new(:int, num.to_i, line: line, col: col)
      end
    end

    def number_part
      num = ''

      while digit?(current_char)
        num += current_char
        advance

        # Allow one underscore between digits for literals like `1_000_000`
        if current_char == '_'
          advance
          raise "Trailing `_` in number" unless digit?(current_char)
        end
      end

      num
    end

    def build_string
      str = ''
      line = @line
      col = @col

      advance

      until string_char?(current_char)
        str += current_char
        advance
      end

      advance
      Token.new(:string, str, line: line, col: col)
    end

    def build_operator
      op = ''
      line = @line
      col = @col

      while operator_char?(current_char) && op.size < 3
        op += current_char
        advance
      end

      Token.new(:operator, op, line: line, col: col)
    end

    def skip_whitespace
      advance while whitespace?(current_char)
    end

    def whitespace?(char)
      char =~ /[ \t\r\f\v]/
    end

    def newline?(char)
      char == "\n"
    end

    def identifier_start?(char)
      char =~ /[a-zA-Z_]/
    end

    def identifier_char?(char)
      char =~ /[a-zA-Z0-9_]/
    end

    def digit?(char)
      char =~ /[0-9]/
    end

    def string_char?(char)
      char == '"'
    end

    def operator_char?(char)
      OPERATOR_CHARS.include?(char)
    end
  end
end
