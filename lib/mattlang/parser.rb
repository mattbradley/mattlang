module Mattlang
  class Parser
    UNARY_OPERATORS = ['-', '+', '!', '~', '&']
    EXPR_LIST_ENDERS = [Token::EOF, Token::KEYWORD_END, Token::KEYWORD_ELSE, Token::KEYWORD_ELSIF, Token::RBRACE]
    LITERAL_TOKENS = {
      Token::FLOAT    => :Float,
      Token::INT      => :Int,
      Token::NIL      => :Nil,
      Token::BOOL     => :Bool,
      Token::STRING   => :String,
      Token::EMBED    => nil
    }

    attr_reader :current_token

    def self.debug(source)
      puts new(source).parse.inspect
    end

    def self.debug_type(source)
      new(source).parse_type
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

    def parse_type
      consume_newline
      type = type_annotation
      consume_newline
      consume(Token::EOF)
      type
    end

    private

    def consume(*token_types)
      if !token_types.empty? && !token_types.include?(current_token.type)
        if token_types.size == 1
          raise "Unexpected #{current_token}; expected #{token_types.first}"
        elsif token_types.size == 2
          raise "Unexpected #{current_token}; expected #{token_types.first} or #{token_types.last}"
        else
          raise "Unexpected #{current_token}; expected #{token_types[0..-2].join(', ')}, or #{token_types.last}"
        end
      end

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

    def top_expr_list(in_module: false)
      exprs = []
      consume_newline

      loop do
        break if current_token.type == Token::EOF || in_module && current_token.type == Token::KEYWORD_END

        exprs <<
          case current_token.type
          when Token::KEYWORD_MODULE then module_def
          when Token::KEYWORD_FN     then fn_def
          when Token::KEYWORD_INFIX  then infix_def
          else expr
          end

        break if current_token.type == Token::EOF || in_module && current_token.type == Token::KEYWORD_END
        consume_terminator
      end

      AST.new(:__top__, exprs)
    end

    def expr_list
      exprs = []
      consume_newline

      loop do
        break if EXPR_LIST_ENDERS.include?(current_token.type)

        exprs << expr

        break if EXPR_LIST_ENDERS.include?(current_token.type)
        consume_terminator
      end

      if exprs.empty?
        nil_ast
      elsif exprs.count == 1
        exprs.first
      else
        AST.new(:__block__, exprs)
      end
    end

    def expr
      return nil_ast if current_token.type == Token::SEMICOLON

      atoms = [expr_atom]

      loop do
        if current_token.type == Token::NEWLINE && peek.type == Token::OPERATOR && !UNARY_OPERATORS.include?(peek.value)
          consume_newline
        elsif current_token.type != Token::OPERATOR
          break
        end

        atoms << AST.new(current_token.value&.to_sym)
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
        consume(Token::KEYWORD_IF)
        keyword_if
      elsif current_token.type == Token::LPAREN
        consume(Token::LPAREN)
        consume_newline

        if current_token.type == Token::RPAREN
          consume(Token::RPAREN)
          nil_ast
        else
          ex = expr
          consume_newline
          consume(Token::RPAREN)

          if current_token.type == Token::LPAREN_ARG
            lambda_call(ex)
          else
            ex
          end
        end
      elsif current_token.type == Token::OPERATOR
        if UNARY_OPERATORS.include?(current_token.value)
          unary_op = current_token.value.to_sym
          consume(Token::OPERATOR)
          AST.new(unary_op, [expr_atom])
        else
          raise "Unexpected binary operator '#{current_token.value}'"
        end
      elsif current_token.type == Token::LBRACKET
        list_literal
      elsif current_token.type == Token::LBRACE
        lambda_literal
      elsif LITERAL_TOKENS.keys.include?(current_token.type)
        literal
      elsif current_token.type == Token::IDENTIFIER
        if peek.type == Token::OPERATOR && UNARY_OPERATORS.include?(peek.value) && peek.meta && peek.meta[:pre_space] && !peek.meta[:post_space]
          fn_call(ambiguous_op: true)
        elsif ([Token::LPAREN_ARG, Token::LPAREN, Token::LBRACKET, Token::LBRACE, Token::IDENTIFIER] + LITERAL_TOKENS.keys).include?(peek.type)
          fn_call
        else
          identifier
        end
      else
        raise "Unexpected token #{current_token}; expected expr atom"
      end
    end

    def keyword_if
      require_end = true

      conditional = expr
      consume_terminator
      then_expr_list = expr_list

      else_expr_list =
        if current_token.type == Token::KEYWORD_ELSE
          consume(Token::KEYWORD_ELSE)
          expr_list
        elsif current_token.type == Token::KEYWORD_ELSIF
          consume(Token::KEYWORD_ELSIF)
          require_end = false
          keyword_if
        else
          nil_ast
        end

      consume(Token::KEYWORD_END) if require_end

      AST.new(:__if__, [conditional, then_expr_list, else_expr_list])
    end

    def list_literal
      consume(Token::LBRACKET)

      list =
        if current_token.type == Token::RBRACKET
          []
        else
          list_elements
        end

      consume(Token::RBRACKET)

      AST.new(:__list__, list)
    end

    def list_elements
      elements = []

      loop do
        elements << expr
        consume_newline

        if current_token.type == Token::COMMA
          consume(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      elements
    end

    def lambda_literal
      consume(Token::LBRACE)
      consume_newline

      consume(Token::LPAREN)
      consume_newline

      args =
        if current_token.type == Token::RPAREN
          []
        else
          lambda_args
        end

      consume_newline
      consume(Token::RPAREN)
      consume_newline

      raise "Unexpected token '#{current_token}'; expect '->' followed by the body of a lambda" if current_token.type != Token::OPERATOR || current_token.value != '->'

      consume(Token::OPERATOR)
      consume_newline

      body = expr_list

      consume(Token::RBRACE)

      lambda_literal_ast = AST.new(:__lambda__, [AST.new(:__args__, args), body])

      if current_token.type == Token::LPAREN_ARG
        lambda_call(lambda_literal_ast)
      else
        lambda_literal_ast
      end
    end

    def lambda_args
      args = []

      loop do
        args << lambda_arg
        consume_newline

        if current_token.type == Token::COMMA
          consume(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      args
    end

    def lambda_arg
      name = current_token.value&.to_sym
      consume(Token::IDENTIFIER)
      consume_newline

      # TODO: Change this to make the type annotation optional once lambda type inference is implemented
      raise "Unexpected #{current_token}; expected ':' followed by type annotation" if current_token.type != Token::OPERATOR || current_token.value != ':'

      if current_token.type == Token::OPERATOR && current_token.value == ':'
        consume(Token::OPERATOR)
        consume_newline

        AST.new(name, type: type_annotation)
      else
        AST.new(name)
      end
    end

    def lambda_call(lambda_ast)
      consume(Token::LPAREN_ARG)
      args =
        if current_token.type == Token::RPAREN
          []
        else
          fn_args
        end

      consume(Token::RPAREN)

      args << lambda_literal if current_token.type == Token::LBRACE

      lambda_call_ast = AST.new(lambda_ast, args)

      if current_token.type == Token::LPAREN_ARG
        lambda_call(lambda_call_ast)
      else
        lambda_call_ast
      end
    end

    def literal
      type = current_token.type
      value = current_token.value
      consume

      case type
      when Token::EMBED
        consume_newline
        raise "Unexpected #{current_token}; expected ':' followed by type annotation after embed" if current_token.type != Token::OPERATOR || current_token.value != ':'

        consume(Token::OPERATOR)
        consume_newline
        embed_type = type_annotation

        AST.new(:__embed__, [AST.new(value)], type: embed_type)
      else
        AST.new(value, type: Types::Simple.new(LITERAL_TOKENS[type]))
      end
    end

    def identifier
      id = current_token.value&.to_sym
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

      AST.new(:__infix__, [AST.new(op), AST.new(associativity), AST.new(precedence)])
    end

    def module_def
      consume(Token::KEYWORD_MODULE)

      name = current_token.value&.to_sym
      consume(Token::IDENTIFIER)
      consume_terminator
      body = top_expr_list(in_module: true)

      consume(Token::KEYWORD_END)

      AST.new(:__module__, [AST.new(name), body])
    end

    def fn_def_signature
      id = current_token.value&.to_s.to_sym
      meta = current_token.type == Token::OPERATOR ? { operator: true } : { }
      consume(Token::IDENTIFIER, Token::OPERATOR)

      if current_token.type == Token::OPERATOR && current_token.value == '<'
        consume(Token::OPERATOR)

        meta[:type_params] = type_parameters(simple_only: true)

        if current_token.type == Token::OPERATOR && current_token.value == '>'
          consume(Token::OPERATOR)
        else
          raise "Unexpected #{current_token}; expected '>'"
        end
      end

      if current_token.type == Token::LPAREN_ARG || current_token.type == Token::LPAREN
        consume(Token::LPAREN_ARG, Token::LPAREN)
        consume_newline

        args =
          if current_token.type == Token::RPAREN
            []
          else
            fn_def_args(meta[:type_params])
          end

        consume_newline
        consume(Token::RPAREN)
      else
        raise "Unexpected #{current_token}; expected '(' followed by function arguments"
      end

      consume_newline

      if current_token.type == Token::OPERATOR && current_token.value == '->'
        consume(Token::OPERATOR)
        consume_newline
        return_type = type_annotation(meta[:type_params])
      else
        raise "Unexpected #{current_token}; expected '->' followed by return type"
      end

      AST.new(id, args, type: return_type, meta: meta.empty? ? nil : meta)
    end

    def fn_def_args(type_params = nil)
      args = []

      loop do
        args << fn_def_arg(type_params)
        consume_newline

        if current_token.type == Token::COMMA
          consume(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      args
    end

    def fn_def_arg(type_params)
      name = current_token.value&.to_sym
      consume(Token::IDENTIFIER)
      consume_newline

      raise "Unexpected #{current_token}; expected ':' followed by type annotation" if current_token.type != Token::OPERATOR || current_token.value != ':'

      consume(Token::OPERATOR)
      consume_newline

      AST.new(name, type: type_annotation(type_params))
    end

    def type_annotation(type_params = nil)
      lambda_type_args = [*type_union(type_params)]

      if current_token.type == Token::OPERATOR && current_token.value == '->'
        consume(Token::OPERATOR)
        consume_newline

        Types::Lambda.new(lambda_type_args, type_annotation(type_params))
      elsif lambda_type_args.size == 1
        lambda_type_args.first
      else
        raise "Unexpected #{current_token} in lambda type; expected '->'"
      end
    end

    def type_union(type_params = nil)
      types = []

      loop do
        next_atom = type_atom(type_params)

        if next_atom.is_a?(Array)
          raise "Invalid union type '#{Types::Union.new(types)} | (#{next_atom.join(', ')})'; surround a lambda type in parentheses to add it to a union" if !types.empty?
          return next_atom
        end

        types << next_atom

        if current_token.type == Token::OPERATOR && current_token.value == '|' || current_token.type == Token::NEWLINE && peek.type == Token::OPERATOR && peek.value == '|'
          consume_newline
          consume(Token::OPERATOR)
          consume_newline
        else
          break
        end
      end

      if types.size == 1
        types.first
      else
        Types.combine(types)
      end
    end

    def type_atom(type_params = nil)
      if current_token.type == Token::LPAREN
        consume(Token::LPAREN)
        consume_newline

        possible_lambda_type_args =
          if current_token.type == Token::RPAREN
            []
          else
            type_parameters(type_params: type_params)
          end

        consume(Token::RPAREN)

        if possible_lambda_type_args.size == 1
          possible_lambda_type_args.first
        else
          possible_lambda_type_args
        end
      else
        type = current_token.value&.to_sym
        consume(Token::IDENTIFIER)

        if current_token.type == Token::OPERATOR && current_token.value == '<' || current_token.type == Token::NEWLINE && peek.type == Token::OPERATOR && peek.value == '<'
          consume_newline
          consume(Token::OPERATOR)
          consume_newline

          type_params = type_parameters(type_params: type_params)

          if current_token.type != Token::OPERATOR || !current_token.value.start_with?('>')
            raise "Unexpected #{current_token}; expected '>'"
          elsif current_token.value != '>'
            @token_buffer << Token.new(Token::OPERATOR, current_token.value[1..-1], line: current_token.line, col: current_token.col + 1)
          end
          consume(Token::OPERATOR)

          Types::Generic.new(type, type_params)
        else
          Types::Simple.new(type, parameter_type: type_params&.include?(type))
        end
      end
    end

    def type_parameters(simple_only: false, type_params: nil)
      params = []

      loop do
        params <<
          if simple_only
            type = current_token.value&.to_sym
            consume(Token::IDENTIFIER)

            type
          else
            type_annotation(type_params)
          end

        consume_newline

        if current_token.type == Token::COMMA
          consume(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      params
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

        args << lambda_literal if current_token.type == Token::LBRACE

        fn_call_ast = AST.new(id, args, meta: !ambiguous_op.nil? ?  { ambiguous_op: true } : nil)

        if current_token.type == Token::LPAREN_ARG
          lambda_call(fn_call_ast)
        else
          fn_call_ast
        end
      else
        # Only allow a single lambda parameter in the non-paren fn call.
        # This disambiguates `[1, 2, 3] |> map { ... } |> reduce(0, { ... })`:
        #   * Right: `[1, 2, 3] |> map({ ... }) |> reduce(0, { ... })`
        #   * Wrong: `[1, 2, 3] |> map({ ... } |> reduce(0, { ... }))`
        #
        # This ensures that only the lambda is captured, and not any operators behind it (as would happen in expr).
        # Unfortunately, this makes expressions like `compose { ... }, { ... }` invalid without parens.
        #
        # This specifies that a call like `map { ... }` is parsed as `map() { ... }`, not including anything after
        # the lambda literal as an argument. Maybe in the future this can be made smarter so that expressions like
        # `compose { ... }, { ... } are parsed correctly instead of requiring parens.
        if current_token.type == Token::LBRACE
          AST.new(id, [lambda_literal], meta: !ambiguous_op.nil? ?  { ambiguous_op: true, no_paren: true } : { no_paren: true })
        else
          AST.new(id, fn_args, meta: !ambiguous_op.nil? ?  { ambiguous_op: true, no_paren: true } : { no_paren: true })
        end
      end
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

    def nil_ast
      AST.new(nil, type: Types::Simple.new(:Nil))
    end
  end
end
