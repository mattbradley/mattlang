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
      ast = top_expr_list

      consume(Token::EOF)

      ast
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

    def consume_terminator
      if current_token.type == Token::SEMICOLON || current_token.type == Token::NEWLINE
        consume
        consume_newline
      elsif current_token.type != Token::EOF
        raise "Unexpected #{current_token}; expected a terminator"
      end
    end

    def peek
      @token_buffer << @lexer.next_token if @token_buffer.empty?
      @token_buffer.first
    end

    def top_expr_list
      exprs = []

      loop do
        consume_newline
        break if current_token.type == Token::EOF

        exprs <<
          case current_token.type
          when Token::KEYWORD_FN then fn_def
          when Token::KEYWORD_INFIX then infix_def
          else expr
          end

        consume_terminator
      end

      AST.new(:__program__, exprs)
    end

    def expr_list
      exprs = []

      loop do
        consume_newline
        break if [Token::EOF, Token::KEYWORD_END, Token::KEYWORD_ELSE].include?(current_token.type)

        exprs << expr

        consume_terminator
      end

      if exprs.empty?
        AST.new(nil)
      else
        AST.new(:__block__, exprs)
      end
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

      if current_token.type == Token::KEYWORD_IF
        keyword_if
      elsif current_token.type == Token::LPAREN
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

    def keyword_if
      consume(Token::KEYWORD_IF)
      conditional = expr
      consume_terminator
      then_expr_list = expr_list

      else_expr_list =
        if current_token.type == Token::KEYWORD_ELSE
          consume(Token::KEYWORD_ELSE)
          expr_list
        else
          AST.new(nil)
        end

      consume(Token::KEYWORD_END)

      AST.new(:if, [conditional, then_expr_list, else_expr_list])
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

    def fn_def
      consume(Token::KEYWORD_FN)

      signature = fn_def_signature
      consume_terminator
      body = expr_list

      consume(Token::KEYWORD_END)

      AST.new(:__fn__, [signature, body])
    end

    def infix_def
      consume(Token::KEYWORD_INFIX)

      signature = infix_def_signature
      consume_terminator
      body = expr_list

      consume(Token::KEYWORD_END)

      AST.new(:__infix__, [signature, body])
    end

    def fn_def_signature
      id = current_token.value&.to_sym
      consume(Token::IDENTIFIER)

      if current_token.type == Token::LPAREN_ARG || current_token.type == Token::LPAREN
        consume
        consume_newline

        args =
          if current_token.type == Token::RPAREN
            []
          else
            fn_def_args
          end

        consume_newline
        consume(Token::RPAREN)
      else
        args = fn_def_args
      end

      consume_newline

      if current_token.type == Token::OPERATOR && current_token.value == '->'
        consume(Token::OPERATOR)
        return_type = current_token.value&.to_sym
        consume(Token::IDENTIFIER)
      else
        raise "Unexpected #{current_token}; expected -> followed by return type"
      end

      AST.new(id, [AST.new(:__args__, args), AST.new(return_type)])
    end

    def infix_def_signature
      associativity = :left
      precedence = 8

      if current_token.type == Token::IDENTIFIER
        associativity = current_token.value&.to_sym
        consume(Token::IDENTIFIER)

        raise "Unexpected associativity '#{associativity}'; infix associativity must be 'right' or 'left'" unless [:left, :right].include?(associativity)
      end

      if current_token.type == Token::INT
        precedence = current_token.value
        consume(Token::INT)

        raise "Unexpected precedence '#{precedence}'; infix precedence must be between 0 and 9" unless (0..9).include?(precedence)
      end

      op = current_token.value&.to_sym
      consume(Token::OPERATOR)

      if current_token.type == Token::LPAREN_ARG || current_token.type == Token::LPAREN
        consume
        consume_newline

        args =
          if current_token.type == Token::RPAREN
            []
          else
            fn_def_args
          end

        consume_newline
        consume(Token::RPAREN)
      else
        args = fn_def_args
      end

      consume_newline

      if current_token.type == Token::OPERATOR && current_token.value == '->'
        consume(Token::OPERATOR)
        return_type = current_token.value&.to_sym
        consume(Token::IDENTIFIER)
      else
        raise "Unexpected #{current_token}; expected -> followed by return type"
      end

      AST.new(op, [AST.new(:__args__, args), AST.new(return_type)], meta: { associativity: associativity, precedence: precedence })
    end

    def fn_def_args
      consume_newline
      return [] if current_token.type == Token::OPERATOR && current_token.value == '->'

      args = []

      loop do
        args << fn_def_arg

        if current_token.type == Token::COMMA
          consume(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      args
    end

    def fn_def_arg
      name = current_token.value&.to_sym
      consume(Token::IDENTIFIER)

      raise "Unexpected #{current_token}; expected ':' followed by type annotation" if current_token.type != Token::OPERATOR || current_token.value != ':'

      consume(Token::OPERATOR)
      type = current_token.value.to_sym
      consume(Token::IDENTIFIER)

      AST.new(name, [AST.new(type)])
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
