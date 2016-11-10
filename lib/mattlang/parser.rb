module Mattlang
  class Parser
    attr_reader :current_token

    def initialize(source)
      puts source
      @lexer = Lexer.new(source)
      @current_token = @lexer.next_token
      @token_buffer = []
    end

    def parse
      consume_newlines

      expr_list
    end

    private

    def unary_operators
      ['-', '+', '!']
    end

    def consume(token_type = nil)
      raise "Unexpected #{current_token}; expected #{token_type}" if token_type && current_token.type != token_type

      @current_token = @token_buffer.any? ? @token_buffer.shift : @lexer.next_token
    end

    def consume_newlines
      consume while @current_token.type == Token::NEWLINE
    end

    def peek
      @token_buffer << @lexer.next_token if @token_buffer.empty?
      @token_buffer.first
    end

    def expr_list
      exprs = []

      loop do
        break if current_token.type == Token::EOF

        exprs << expr

        if current_token.type == Token::SEMICOLON || current_token.type == Token::NEWLINE
          consume
          consume_newlines
        else
          raise "Unexpected #{current_token}; expected a terminator"
        end
      end

      exprs
    end

    def expr
      atom = expr_atom

      if current_token.type == Token::OPERATOR
        op = current_token.value.to_sym
        consume(Token::OPERATOR)
        [op, [atom, expr]]
      else
        atom
      end
    end

    def expr_atom
      consume_newlines

      if current_token.type == Token::LPAREN
        consume(Token::LPAREN)
        consume_newlines

        if current_token.type == Token::RPAREN
          nil
        else
          ex = expr
          consume_newlines
          consume(Token::RPAREN)
          ex
        end
      elsif current_token.type == Token::OPERATOR
        if unary_operators.include?(current_token.value)
          unary_op = current_token.value.to_sym
          consume(Token::OPERATOR)
          [unary_op, [expr_atom]]
        else
          raise "Unexpected binary operator #{current_token}"
        end
      elsif [Token::FLOAT, Token::INT, Token::STRING, Token::NIL, Token::BOOL].include?(current_token.type)
        literal
      elsif current_token.type == Token::IDENTIFIER
        case peek.type
        when Token::IDENTIFIER, Token::LPAREN_ARG
          fn_call
        else
          identifier
        end
      else
        raise "Unknown token #{current_token}; expected expr atom"
      end
    end

    def literal
      value = current_token.value
      consume
      value
    end

    def identifier
      id = current_token.value.to_sym
      consume(Token::IDENTIFIER)
      id
    end

    def fn_call
      id = current_token.value.to_sym
      consume(Token::IDENTIFIER)

      if current_token.type == Token::LPAREN_ARG
        consume(Token::LPAREN_ARG)
        args = fn_args
        consume(Token::RPAREN)
      else
        args = fn_args
      end

      [id, args]
    end

    def fn_args
      args = []

      loop do
        args << expr

        if current_token.type == Token::COMMA
          consume(Token::COMMA)
        else
          break
        end
      end

      args
    end
  end
end
