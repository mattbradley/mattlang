module Mattlang
  class Parser
    class Error < StandardError; end
    class UnexpectedTokenError < Error
      attr_reader :token

      def initialize(token, message)
        @token = token
        super(message)
      end
    end

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
      ast = parse_top_expr_list

      consume(Token::EOF)

      ast
    end

    def parse_type
      consume_newline
      type = parse_type_annotation
      consume_newline
      consume(Token::EOF)
      type
    end

    private

    def freeze_lexer
      @frozen_lexer = @lexer.dup
      @frozen_current_token = @current_token.dup
      @frozen_token_buffer = @token_buffer.dup
    end

    def unfreeze_lexer
      raise "The lexer must be frozen before it can be unfrozen" if @frozen_lexer.nil?

      @lexer = @frozen_lexer
      @current_token = @frozen_current_token
      @token_buffer = @frozen_token_buffer

      @frozen_lexer = nil
      @frozen_current_token = nil
      @frozen_token_buffer = nil
    end

    def token_error(msg)
      UnexpectedTokenError.new(current_token, "Unexpected token '#{current_token}'; #{msg}")
    end

    def consume(*token_types)
      if !token_types.empty? && !token_types.include?(current_token.type)
        if token_types.size == 1
          raise token_error("expected #{token_types.first}")
        elsif token_types.size == 2
          raise token_error("expected #{token_types.first} or #{token_types.last}")
        else
          raise token_error("expected #{token_types[0..-2].join(', ')}, or #{token_types.last}")
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
        raise token_error("expected a terminator")
      end
    end

    def peek
      @token_buffer << @lexer.next_token if @token_buffer.empty?
      @token_buffer.first
    end

    def parse_top_expr_list(in_module: false)
      exprs = []
      consume_newline

      loop do
        break if current_token.type == Token::EOF || in_module && current_token.type == Token::KEYWORD_END

        exprs <<
          case current_token.type
          when Token::KEYWORD_MODULE  then parse_module_def
          when Token::KEYWORD_REQUIRE then in_module ? raise(Error.new("You can only require files at the top level")) : parse_require_directive
          when Token::KEYWORD_FN      then parse_fn_def
          when Token::KEYWORD_INFIX   then parse_infix_def
          else parse_expr
          end

        break if current_token.type == Token::EOF || in_module && current_token.type == Token::KEYWORD_END
        consume_terminator
      end

      AST.new(:__top__, exprs)
    end

    def parse_expr_list
      exprs = []
      consume_newline

      loop do
        break if EXPR_LIST_ENDERS.include?(current_token.type)

        exprs << parse_expr

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

    def parse_expr
      return nil_ast if current_token.type == Token::SEMICOLON

      atoms = [parse_expr_atom]

      loop do
        if current_token.type == Token::NEWLINE && peek.type == Token::OPERATOR && !UNARY_OPERATORS.include?(peek.value)
          consume_newline
        elsif current_token.type != Token::OPERATOR
          break
        end

        atoms << AST.new(current_token.value.to_sym) rescue nil
        consume(Token::OPERATOR)
        atoms << parse_expr_atom
      end

      if atoms.size == 1
        atoms.first
      else
        AST.new(:__expr__, atoms)
      end
    end

    def parse_expr_atom
      consume_newline

      if current_token.type == Token::KEYWORD_IF
        consume(Token::KEYWORD_IF)
        parse_if
      elsif current_token.type == Token::LPAREN
        consume(Token::LPAREN)
        consume_newline

        if current_token.type == Token::RPAREN
          consume(Token::RPAREN)
          nil_ast
        else
          tuple = parse_tuple_elements
          consume_newline
          consume(Token::RPAREN)

          ex =
            if tuple.size == 1
              tuple.first
            else
              AST.new(:__tuple__, tuple)
            end

          if current_token.type == Token::LPAREN_ARG
            parse_lambda_call(ex)
          else
            ex
          end
        end
      elsif current_token.type == Token::OPERATOR
        if UNARY_OPERATORS.include?(current_token.value)
          unary_op = current_token.value.to_sym
          consume(Token::OPERATOR)
          AST.new(unary_op, [parse_expr_atom])
        else
          raise token_error("expected expr atom")
        end
      elsif current_token.type == Token::LBRACKET
        parse_list_literal
      elsif current_token.type == Token::LBRACE
        parse_lambda_literal
      elsif LITERAL_TOKENS.keys.include?(current_token.type)
        parse_literal
      elsif current_token.type == Token::IDENTIFIER
        if peek.type == Token::OPERATOR && UNARY_OPERATORS.include?(peek.value) && peek.meta && peek.meta[:pre_space] && !peek.meta[:post_space]
          parse_fn_call(ambiguous_op: true)
        elsif ([Token::LPAREN_ARG, Token::LPAREN, Token::LBRACKET, Token::LBRACE, Token::IDENTIFIER] + LITERAL_TOKENS.keys).include?(peek.type)
          parse_fn_call
        else
          parse_identifier
        end
      else
        raise token_error("expected expr atom")
      end
    end

    def parse_if
      require_end = true

      conditional = parse_expr
      consume_terminator
      then_expr_list = parse_expr_list

      else_expr_list =
        if current_token.type == Token::KEYWORD_ELSE
          consume(Token::KEYWORD_ELSE)
          parse_expr_list
        elsif current_token.type == Token::KEYWORD_ELSIF
          consume(Token::KEYWORD_ELSIF)
          require_end = false
          parse_if
        else
          nil_ast
        end

      consume(Token::KEYWORD_END) if require_end

      AST.new(:__if__, [conditional, then_expr_list, else_expr_list])
    end

    def parse_list_literal
      consume(Token::LBRACKET)

      list =
        if current_token.type == Token::RBRACKET
          []
        else
          parse_list_elements
        end

      consume(Token::RBRACKET)

      AST.new(:__list__, list)
    end

    def parse_list_elements
      elements = []

      loop do
        elements << parse_expr
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

=begin
lambda_literal : LBRACE LPAREN (fn_def_args)? RPAREN '->' expr_list RBRACE
               | LBRACE lambda_ags '->' expr_list RBRACE
               | LBRACE expr_list RBRACE
               ;
=end
    def parse_lambda_literal
      consume(Token::LBRACE)
      consume_newline

      freeze_lexer

      lambda_args =
        if current_token.type == Token::LPAREN
          # Could be a typed lambda or a no-arg lambda with tuple start
          #   { (x: Int, y: Int) -> ... } or { (x, y) }
          #   { () -> ... } or { () }

          consume(Token::LPAREN)
          consume_newline

          no_arg_lambda = true

          if current_token.type == Token::RPAREN
            consume(Token::RPAREN)
            consume_newline

            no_arg_lambda = false if current_token.type == Token::STAB
          elsif current_token.type == Token::IDENTIFIER
            consume(Token::IDENTIFIER)
            consume_newline

            no_arg_lambda = false if current_token.type == Token::COLON
          end

          unfreeze_lexer

          if no_arg_lambda
            []
          else
            consume(Token::LPAREN)
            consume_newline

            args =
              if current_token.type == Token::RPAREN
                []
              else
                parse_fn_def_args
              end

            consume_newline
            consume(Token::RPAREN)
            consume_newline

            consume(Token::STAB)
            consume_newline

            args
          end
        else
          # Could be an untyped lambda or a no-arg lambda with identifier start
          #   { x -> ... } or { x * x }
          #   { x, y -> ... } or { x * x }

          no_arg_lambda = true

          if current_token.type == Token::IDENTIFIER
            consume(Token::IDENTIFIER)
            consume_newline

            no_arg_lambda = false if [Token::STAB, Token::COMMA].include?(current_token.type)
          end

          unfreeze_lexer

          if no_arg_lambda
            []
          else
            args = parse_untyped_lambda_args

            consume_newline
            consume(Token::STAB)
            consume_newline

            args
          end
        end

      body = parse_expr_list

      consume(Token::RBRACE)

      meta = lambda_args.size > 0 && lambda_args.first.type.nil? ? { untyped: true } : nil
      lambda_literal_ast = AST.new(:__lambda__, [AST.new(:__args__, lambda_args), body], meta: meta)

      if current_token.type == Token::LPAREN_ARG
        parse_lambda_call(lambda_literal_ast)
      else
        lambda_literal_ast
      end
    end

    def parse_untyped_lambda_args
      args = []

      loop do
        args << parse_untyped_lambda_arg
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

    def parse_untyped_lambda_arg
      name = current_token.value.to_sym rescue nil
      consume(Token::IDENTIFIER)

      AST.new(name)
    end

    def parse_lambda_call(lambda_ast)
      consume(Token::LPAREN_ARG)
      args =
        if current_token.type == Token::RPAREN
          []
        else
          parse_tuple_elements
        end

      consume(Token::RPAREN)

      args << parse_lambda_literal if current_token.type == Token::LBRACE

      lambda_call_ast = AST.new(lambda_ast, args)

      if current_token.type == Token::LPAREN_ARG
        parse_lambda_call(lambda_call_ast)
      else
        lambda_call_ast
      end
    end

    def parse_literal
      type = current_token.type
      value = current_token.value
      consume

      case type
      when Token::EMBED
        consume_newline
        consume(Token::COLON)
        consume_newline

        embed_type = parse_type_annotation

        AST.new(:__embed__, [AST.new(value)], type: embed_type)
      else
        AST.new(value, type: Types::Simple.new(LITERAL_TOKENS[type]))
      end
    end

    def parse_identifier
      id = current_token.value.to_sym rescue nil
      consume(Token::IDENTIFIER)

      raise Error.new("Unexpected identifier '#{id}'; identifiers cannot begin with double underscore") if id.to_s.start_with?('__')

      AST.new(id)
    end

    def parse_fn_def
      consume(Token::KEYWORD_FN)

      signature = parse_fn_def_signature
      consume_terminator
      body = parse_expr_list

      consume(Token::KEYWORD_END)

      AST.new(:__fn__, [signature, body])
    end

    def parse_infix_def
      consume(Token::KEYWORD_INFIX)

      associativity = :left
      precedence = 8

      if current_token.type == Token::IDENTIFIER
        associativity = current_token.value.to_sym rescue nil
        consume(Token::IDENTIFIER)

        raise Error.new("Unexpected associativity '#{associativity}'; infix associativity must be 'right' or 'left'") unless [:left, :right].include?(associativity)
      end

      if current_token.type == Token::INT
        precedence = current_token.value
        consume(Token::INT)

        raise Error.new("Unexpected precedence '#{precedence}'; infix precedence must be between 0 and 9") unless (0..9).include?(precedence)
      end

      op = current_token.value.to_sym rescue nil
      consume(Token::OPERATOR)

      AST.new(:__infix__, [AST.new(op), AST.new(associativity), AST.new(precedence)])
    end

    def parse_module_def
      consume(Token::KEYWORD_MODULE)
      consume_newline

      name = current_token.value.to_sym rescue nil
      consume(Token::IDENTIFIER)
      consume_terminator
      body = parse_top_expr_list(in_module: true)

      consume(Token::KEYWORD_END)

      AST.new(:__module__, [AST.new(name), body])
    end

    def parse_require_directive
      consume(Token::KEYWORD_REQUIRE)

      file = current_token.value
      consume(Token::STRING)

      AST.new(:__require__, [AST.new(file, type: Types::Simple.new(LITERAL_TOKENS[Token::STRING]))])
    end

    def parse_fn_def_signature
      consume_newline

      id = current_token.value.to_sym rescue nil
      meta = current_token.type == Token::OPERATOR ? { operator: true } : { }
      consume(Token::IDENTIFIER, Token::OPERATOR)

      if current_token.type == Token::OPERATOR && current_token.value == '<'
        consume(Token::OPERATOR)

        meta[:type_params] = parse_type_parameters(simple_only: true)

        if current_token.type == Token::OPERATOR && current_token.value == '>'
          consume(Token::OPERATOR)
        else
          raise token_error("expected '>'")
        end
      end

      if current_token.type == Token::LPAREN_ARG || current_token.type == Token::LPAREN
        consume(Token::LPAREN_ARG, Token::LPAREN)
        consume_newline

        args =
          if current_token.type == Token::RPAREN
            []
          else
            parse_fn_def_args(meta[:type_params])
          end

        consume_newline
        consume(Token::RPAREN)
      else
        raise token_error("expected '(' followed by function arguments")
      end

      consume_newline

      consume(Token::STAB)
      consume_newline
      return_type = parse_type_annotation(meta[:type_params])

      AST.new(id, args, type: return_type, meta: meta.empty? ? nil : meta)
    end

    def parse_fn_def_args(type_params = nil)
      args = []

      loop do
        args << parse_fn_def_arg(type_params)
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

    def parse_fn_def_arg(type_params)
      name = current_token.value.to_sym rescue nil
      consume(Token::IDENTIFIER)
      consume_newline

      consume(Token::COLON)
      consume_newline

      AST.new(name, type: parse_type_annotation(type_params))
    end

    def parse_type_annotation(type_params = nil)
      type = parse_type_union(type_params)

      if current_token.type == Token::STAB
        consume(Token::STAB)
        consume_newline

        if type.is_a?(Types::Tuple)
          type = type.types
        else
          type = [type]
        end

        Types::Lambda.new(type, parse_type_annotation(type_params))
      else
        type
      end
    end

    def parse_type_union(type_params = nil)
      types = []

      loop do
        types << parse_type_atom(type_params)

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

    def parse_type_atom(type_params = nil)
      if current_token.type == Token::LPAREN
        consume(Token::LPAREN)
        consume_newline

        possible_tuple =
          if current_token.type == Token::RPAREN
            []
          else
            parse_type_parameters(type_params: type_params)
          end

        consume(Token::RPAREN)

        if possible_tuple.size == 1 && !possible_tuple.first.is_a?(Types::Tuple)
          possible_tuple.first
        else
          Types::Tuple.new(possible_tuple)
        end
      else
        type = current_token.value.to_sym rescue nil
        consume(Token::IDENTIFIER)

        if current_token.type == Token::OPERATOR && current_token.value == '<' || current_token.type == Token::NEWLINE && peek.type == Token::OPERATOR && peek.value == '<'
          consume_newline
          consume(Token::OPERATOR)
          consume_newline

          type_params = parse_type_parameters(type_params: type_params)

          if current_token.type != Token::OPERATOR || !current_token.value.start_with?('>')
            raise token_error("expected '>'")
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

    def parse_type_parameters(simple_only: false, type_params: nil)
      params = []

      loop do
        params <<
          if simple_only
            type = current_token.value.to_sym rescue nil
            consume(Token::IDENTIFIER)

            type
          else
            parse_type_annotation(type_params)
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

    def parse_fn_call(ambiguous_op: nil)
      id = current_token.value.to_sym
      consume(Token::IDENTIFIER)

      if current_token.type == Token::LPAREN_ARG
        consume(Token::LPAREN_ARG)
        args =
          if current_token.type == Token::RPAREN
            []
          else
            parse_tuple_elements
          end

        consume(Token::RPAREN)

        args << parse_lambda_literal if current_token.type == Token::LBRACE

        fn_call_ast = AST.new(id, args, meta: !ambiguous_op.nil? ?  { ambiguous_op: true } : nil)

        if current_token.type == Token::LPAREN_ARG
          parse_lambda_call(fn_call_ast)
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
          AST.new(id, [parse_lambda_literal], meta: !ambiguous_op.nil? ?  { ambiguous_op: true, no_paren: true } : { no_paren: true })
        else
          AST.new(id, parse_tuple_elements, meta: !ambiguous_op.nil? ?  { ambiguous_op: true, no_paren: true } : { no_paren: true })
        end
      end
    end

    def parse_tuple_elements
      elements = []

      loop do
        elements << parse_expr

        if current_token.type == Token::COMMA
          consume(Token::COMMA)
        else
          break
        end
      end

      elements
    end

    def nil_ast
      AST.new(nil, type: Types::Simple.new(:Nil))
    end
  end
end
