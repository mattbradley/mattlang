module Mattlang
  class Lexer
    attr_reader :current_char

    LITERALS = {
      'nil' => Token::NIL,
      'true' => [Token::BOOL, true],
      'false' => [Token::BOOL, false]
    }

    KEYWORDS = {
      'if' => :if,
      'end' => :end,
      'else' => :else,
      'elsif' => :elsif,
      'return' => :return,
      'fn' => :fn,
      'infix' => :infix
    }

    PUNCTUATION_TYPES = {
      '(' => Token::LPAREN,
      ')' => Token::RPAREN,
      '[' => Token::LBRACKET,
      ']' => Token::RBRACKET,
      '{' => Token::LBRACE,
      '}' => Token::RBRACE,
      ';' => Token::SEMICOLON,
      ',' => Token::COMMA
    }

    OPERATOR_CHARS = %w(@ . + - ! ^ ~ * / < > | & = : %)

    def self.tokenize(source)
      lexer = new(source)
      tokens = []
      loop do
        t = lexer.next_token
        tokens << t
        break if t.type == Token::EOF
      end
      tokens
    end

    def self.annotate(source)
      tokens = tokenize(source).group_by(&:line)

      puts source.lines
        .each_with_index
        .map { |line, i| "#{line}#{tokens[i]&.map(&:to_short_s)&.join(' ') || '--'}\n\n" }
    end

    def initialize(source)
      @source = source
      @pos = 0
      @line = 0
      @col = 0
      @current_char = @source[@pos]
      @token_buffer = []
      @previous_token = nil
      @previous_whitespace = false
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

    def prev_char
      @pos > 0 ? @source[@pos - 1] : nil
    end

    def next_char
      @source[@pos + 1]
    end

    def next_token
      return @token_buffer.shift if @token_buffer.any?

      token =
        if current_char.nil?
          Token.new(Token::EOF, line: @line, col: @col)
        elsif whitespace?(current_char)
          skip_whitespace
          @previous_whitespace = true
          next_token
        elsif comment_char?(current_char)
          skip_comment
          next_token
        elsif newline?(current_char)
          if @previous_token&.type == Token::NEWLINE
            advance
            next_token
          else
            Token.new(Token::NEWLINE, line: @line, col: @col)
          end
        elsif identifier_start?(current_char)
          build_identifier
        elsif digit?(current_char)
          build_number
        elsif string_char?(current_char)
          build_string
        elsif embed_char?(current_char)
          build_embed
        elsif (punctuation = PUNCTUATION_TYPES[current_char])
          token = Token.new(punctuation, line: @line, col: @col)
          advance

          if (token.type == Token::RPAREN || token.type == Token::RBRACE) && current_char == '('
            @token_buffer << Token.new(Token::LPAREN_ARG, line: @line, col: @col)
            advance
          end

          token
        elsif operator_char?(current_char)
          build_operator
        else
          raise "Unknown token '#{current_char}'"
        end

      @previous_whitespace = false
      @previous_token = token

      token
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
        Token.new(:"keyword_#{keyword}", line: line, col: col)
      else
        if current_char == '('
          @token_buffer << Token.new(Token::LPAREN_ARG, line: @line, col: @col)
          advance
        end

        Token.new(Token::IDENTIFIER, id, line: line, col: col)
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
        Token.new(Token::FLOAT, num.to_f, line: line, col: col)
      else
        Token.new(Token::INT, num.to_i, line: line, col: col)
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
          raise "Trailing '_' in number" unless digit?(current_char)
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
      Token.new(Token::STRING, str, line: line, col: col)
    end

    def build_embed
      embed = ''
      line = @line
      col = @col

      advance

      until embed_char?(current_char)
        embed += current_char
        advance
      end

      advance
      Token.new(Token::EMBED, embed, line: line, col: col)
    end

    def skip_comment
      advance until current_char == "\n" || current_char.nil?
    end

    def build_operator
      op = ''
      line = @line
      col = @col

      while operator_char?(current_char) && op.size < 3
        op += current_char
        advance
      end

      meta = {
        pre_space: @previous_whitespace,
        post_space: !!(newline?(current_char) || whitespace?(current_char))
      }

      Token.new(Token::OPERATOR, op, meta: meta, line: line, col: col)
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

    def embed_char?(char)
      char == '`'
    end

    def operator_char?(char)
      OPERATOR_CHARS.include?(char)
    end

    def comment_char?(char)
      char == '#'
    end
  end
end
