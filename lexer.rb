class Token
  attr_reader :type, :value

  def initialize(type, value = nil)
    @type = type
    @value = value
  end

  def to_s
    value.nil? ? "(#{type})" : "(#{type}: #{value.inspect})"
  end
end

class Lexer
  attr_reader :current_char

  KEYWORDS = {
    'nil' => :nil,
    'true' => [:bool, true],
    'false' => [:bool, false],
    'fn' => :fn,
    'end' => :end
  }

  PUNCTUATION_TYPES = {
    '(' => :lparen,
    ')' => :rparen,
    '[' => :lbracket,
    ']' => :rbracket,
    ';' => :semicolon,
    ',' => :comma
  }

  OPERATOR_CHARS = %w(@ . + - ! ^ ~ * / < > | & = :)

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

  def initialize(source)
    @source = source
    @pos = 0
    @current_char = @source[@pos]
    @buffered_tokens = []
  end

  def advance
    @pos += 1
    @current_char = @source[@pos]
  end

  def next_token
    return @buffered_tokens.shift if @buffered_tokens.any?

    if current_char.nil?
      Token.new(:eof)
    elsif whitespace?(current_char)
      skip_whitespace
      next_token
    elsif newline?(current_char)
      advance while newline?(current_char)
      Token.new(:newline)
    elsif identifier_start?(current_char)
      identifier
    elsif digit?(current_char)
      number
    elsif (punctuation = PUNCTUATION_TYPES[current_char])
      curr = current_char
      advance
      Token.new(punctuation)
    elsif operator_char?(current_char)
      operator
    else
      raise "Unknown token '#{current_char}'"
    end
  end

  private

  def identifier
    id = ''

    while identifier_char?(current_char)
      id += current_char
      advance
    end

    if (keyword = KEYWORDS[id])
      Token.new(*keyword)
    else
      if current_char == '('
        advance
        @buffered_tokens << Token.new(:lparen_arg)
      end

      Token.new(:identifier, id)
    end
  end

  def number
    num = ''

    while digit?(current_char)
      num += current_char
      advance

      # Allow one underscore between digits for literals like `1_000_000`
      advance if current_char == '_'
    end

    if current_char == '.'
      num += current_char
      advance

      while digit?(current_char)
        num += current_char
        advance
        advance if current_char == '_'
      end

      Token.new(:float, num.to_f)
    else
      Token.new(:int, num.to_i)
    end
  end

  def operator
    op = ''

    while operator_char?(current_char) && op.size < 3
      op += current_char
      advance
    end

    Token.new(:operator, op)
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

  def operator_char?(char)
    OPERATOR_CHARS.include?(char)
  end
end
