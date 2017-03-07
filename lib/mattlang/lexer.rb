module Mattlang
  class Lexer
    class Error < CompilerError
      def self.title; 'Lexical Error' end

      attr_accessor :location

      def initialize(message, location)
        @location = location
        super(message)
      end
    end

    attr_reader :current_char

    LITERALS = {
      'nil'   => Token::NIL,
      'true'  => [Token::BOOL, true],
      'false' => [Token::BOOL, false]
    }

    KEYWORDS = {
      'if'        => Token::KEYWORD_IF,
      'end'       => Token::KEYWORD_END,
      'else'      => Token::KEYWORD_ELSE,
      'elsif'     => Token::KEYWORD_ELSIF,
      'return'    => Token::KEYWORD_RETURN,
      'module'    => Token::KEYWORD_MODULE,
      'require'   => Token::KEYWORD_REQUIRE,
      'fn'        => Token::KEYWORD_FN,
      'infix'     => Token::KEYWORD_INFIX,
      'typealias' => Token::KEYWORD_TYPEALIAS,
      'case'      => Token::KEYWORD_CASE
    }

    RESERVED_OPERATORS = {
      '->' => Token::STAB,
      ':'  => Token::COLON
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

    def initialize(source, filename: nil)
      @source = source
      @filename = filename
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
      token =
        if @token_buffer.any?
          @token_buffer.shift
        elsif current_char.nil?
          Token.new(Token::EOF, location: current_location)
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
            t = Token.new(Token::NEWLINE, raw: current_char, location: current_location)
            advance
            t
          end
        elsif identifier_start?(current_char) || @previous_token&.type == Token::OPERATOR && @previous_token&.value == '.' && digit?(current_char)
          build_identifier
        elsif digit?(current_char)
          build_number
        elsif string_char?(current_char)
          build_string
        elsif embed_char?(current_char)
          build_embed
        elsif (punctuation = PUNCTUATION_TYPES[current_char])
          t = Token.new(punctuation, raw: current_char, location: current_location)
          advance

          if (t.type == Token::RPAREN || t.type == Token::RBRACE) && current_char == '('
            @token_buffer << Token.new(Token::LPAREN_ARG, raw: current_char, location: current_location)
            advance
          end

          t
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

    def current_location
      Location.new(source: @source, filename: @filename, line: @line, col: @col)
    end

    def build_identifier
      id = ''
      loc = current_location

      while identifier_char?(current_char)
        id += current_char
        advance
      end

      token = 
        if (literal = LITERALS[id])
          Token.new(*literal)
        elsif (keyword = KEYWORDS[id])
          Token.new(keyword)
        else
          if current_char == '('
            @token_buffer << Token.new(Token::LPAREN_ARG, raw: current_char, location: current_location)
            advance
          end

          Token.new(Token::IDENTIFIER, id)
        end

      token.raw = id
      token.location = loc
      token
    end

    def build_number
      loc = current_location

      num = number_part

      if current_char == '.' && digit?(next_char)
        num += current_char
        advance
        num += number_part

        Token.new(Token::FLOAT, num.gsub('_', '').to_f, raw: num, location: loc)
      else
        Token.new(Token::INT, num.gsub('_', '').to_i, raw: num, location: loc)
      end
    end

    def number_part
      num = ''

      while digit?(current_char)
        num += current_char
        advance

        # Allow one underscore between digits for literals like `1_000_000`
        if current_char == '_'
          num += current_char
          advance
          raise Error.new("An underscore in a number literal must be followed by a digit", current_location) unless digit?(current_char)
        end
      end

      num
    end

    def build_string
      str = ''
      loc = current_location
      raw = current_char

      advance

      until string_char?(current_char)
        str += current_char
        advance
      end

      raw += str + current_char
      advance
      Token.new(Token::STRING, str, raw: raw, location: loc)
    end

    def build_embed
      embed = ''
      loc = current_location
      raw = current_char

      advance

      until embed_char?(current_char)
        embed += current_char
        advance
      end

      raw += embed + current_char
      advance
      Token.new(Token::EMBED, embed, raw: raw, location: loc)
    end

    def skip_comment
      advance until current_char == "\n" || current_char.nil?
    end

    def build_operator
      op = ''
      loc = current_location

      while operator_char?(current_char) && op.size < 3
        op += current_char
        advance
      end

      if (reserved_op = RESERVED_OPERATORS[op])
        Token.new(reserved_op, raw: op, location: loc)
      else
        meta = {
          pre_space: @previous_whitespace,
          post_space: !!(newline?(current_char) || whitespace?(current_char))
        }

        Token.new(Token::OPERATOR, op, meta: meta, raw: op, location: loc)
      end
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
