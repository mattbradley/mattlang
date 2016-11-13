module Mattlang
  class Parser
    attr_reader :current_token

    def self.debug_ast(source)
      puts new(source).parse.inspect
    end

    def initialize(source)
      @lexer = Lexer.new(source)
      @current_token = @lexer.next_token
      @token_buffer = []
    end

    def parse
      consume_newline

      AST.new(:__block__, expr_list)
    end

    private

    def unary_operators
      ['-', '+', '!', '~']
    end

    def consume(token_type = nil)
      raise "Unexpected #{current_token}; expected #{token_type}" if token_type && token_type != current_token.type

      @current_token = @token_buffer.any? ? @token_buffer.shift : @lexer.next_token
    end

    def consume_newline
      consume if @current_token.type == Token::NEWLINE
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
          consume_newline
        elsif current_token.type != Token::EOF
          raise "Unexpected #{current_token}; expected a terminator"
        end
      end

      exprs
    end

    def expr
      return AST.new(nil) if current_token.type == Token::SEMICOLON

      atoms = [expr_atom]

      loop do
        if current_token.type == Token::NEWLINE && peek.type == Token::OPERATOR && !unary_operators.include?(peek.value)
          consume_newline
        elsif current_token.type != Token::OPERATOR
          break
        end

        atoms << AST.new(:__binary_op__, [current_token.value.to_sym])
        consume(Token::OPERATOR)
        atoms << expr_atom
      end

      if atoms.size == 1
        atoms.first
      else
        AST.new(:__expr__, atoms)
      end
    end

    def expr_atom
      consume_newline

      if current_token.type == Token::LPAREN
        consume(Token::LPAREN)
        consume_newline

        if current_token.type == Token::RPAREN
          consume(Token::RPAREN)
          AST.new(nil)
        else
          ex = expr
          consume_newline
          consume(Token::RPAREN)
          ex
        end
      elsif current_token.type == Token::OPERATOR
        if unary_operators.include?(current_token.value)
          unary_op = current_token.value.to_sym
          consume(Token::OPERATOR)
          AST.new(unary_op, [expr_atom])
        else
          raise "Unexpected binary operator #{current_token}"
        end
      elsif [Token::FLOAT, Token::INT, Token::STRING, Token::NIL, Token::BOOL].include?(current_token.type)
        literal
      elsif current_token.type == Token::IDENTIFIER
        if peek.type == Token::OPERATOR && unary_operators.include?(peek.value) && peek.meta && peek.meta[:pre_space] && !peek.meta[:post_space]
          fn_call(ambiguous_op: true)
        elsif [Token::LPAREN_ARG, Token::LPAREN, Token::IDENTIFIER, Token::FLOAT, Token::INT, Token::STRING, Token::NIL, Token::BOOL].include?(peek.type)
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
      AST.new(value)
    end

    def identifier
      id = current_token.value.to_sym
      consume(Token::IDENTIFIER)
      AST.new(id)
    end

    def fn_call(ambiguous_op: nil)
      id = current_token.value.to_sym
      consume(Token::IDENTIFIER)

      if current_token.type == Token::LPAREN_ARG
        consume(Token::LPAREN_ARG)
        args =
          if current_token.type == Token::RPAREN
            []
          else
            fn_args
          end

        consume(Token::RPAREN)
      else
        args = fn_args
      end

      AST.new(id, args, meta: !ambiguous_op.nil? ?  { ambiguous_op: true } : nil)
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
