module Mattlang
  class Parser
    class Error < CompilerError
      def self.title; 'Syntax Error' end
    end

    class UnexpectedTokenError < Error
      def self.title; 'Unexpected Token Error' end
    end

    UNARY_OPERATORS = ['-', '+', '!', '~', '&']
    EXPR_LIST_ENDERS = [Token::EOF, Token::KEYWORD_END, Token::KEYWORD_ELSE, Token::KEYWORD_ELSIF, Token::RBRACE, Token::STAB]
    LITERAL_TOKENS = {
      Token::FLOAT  => :Float,
      Token::INT    => :Int,
      Token::NIL    => :Nil,
      Token::BOOL   => :Bool,
      Token::STRING => :String,
      Token::EMBED  => nil
    }

    attr_reader :current_token

    def self.debug(source)
      puts new(source).parse.inspect
    end

    def self.debug_type(source)
      new(source).parse_type
    end

    def initialize(source, filename: nil)
      @lexer = Lexer.new(source, filename: filename)
      @current_token = @lexer.next_token
      @token_buffer = []
    end

    def parse
      consume_newline
      ast = parse_top_expr_list

      consume!(Token::EOF)

      ast
    end

    def parse_type
      consume_newline
      type = parse_type_annotation
      consume_newline
      consume!(Token::EOF)
      type
    end

    private

    def push_lexer
      @stacked_lexer = @lexer.dup
      @stacked_current_token = @current_token.dup
      @stacked_token_buffer = @token_buffer.dup
    end

    def pop_lexer
      raise "The lexer must be frozen before it can be unfrozen" if @stacked_lexer.nil?

      @lexer = @stacked_lexer
      @current_token = @stacked_current_token
      @token_buffer = @stacked_token_buffer

      @stacked_lexer = nil
      @stacked_current_token = nil
      @stacked_token_buffer = nil
    end

    def token_error(msg)
      UnexpectedTokenError.new("Unexpected token #{current_token}; #{msg}", current_token)
    end

    def consume
      @current_token = @token_buffer.any? ? @token_buffer.shift : @lexer.next_token
    end

    def consume!(*token_types)
      if !token_types.empty? && !token_types.include?(current_token.type)
        if token_types.size == 1
          raise token_error("expected #{token_types.first}")
        elsif token_types.size == 2
          raise token_error("expected #{token_types.first} or #{token_types.last}")
        else
          raise token_error("expected #{token_types[0..-2].join(', ')}, or #{token_types.last}")
        end
      end

      consume
    end

    def consume_newline
      consume if @current_token.type == Token::NEWLINE
    end

    def consume_terminator
      if current_token.type == Token::SEMICOLON || current_token.type == Token::NEWLINE
        consume
        consume_newline
      end
    end

    def consume_terminator!
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
          when Token::KEYWORD_MODULE    then parse_module_def
          when Token::KEYWORD_REQUIRE   then in_module ? raise(Error.new("You can only require files at the top level"), current_token) : parse_require_directive
          when Token::KEYWORD_FN        then parse_fn_def
          when Token::KEYWORD_INFIX     then parse_infix_def
          when Token::KEYWORD_TYPEALIAS then parse_typealias_def
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

        atoms << AST.new(current_token.value.to_sym, token: current_token) rescue nil
        consume!(Token::OPERATOR)
        atoms << parse_expr_atom
      end

      if atoms.size == 1
        atoms.first
      else
        AST.new(:__expr__, atoms, token: atoms.first.token)
      end
    end

    def parse_expr_atom
      consume_newline

      if current_token.type == Token::KEYWORD_IF
        parse_if
      elsif current_token.type == Token::KEYWORD_CASE
        parse_case
      elsif current_token.type == Token::LPAREN
        lparen_token = current_token
        consume!(Token::LPAREN)
        consume_newline

        if current_token.type == Token::RPAREN
          consume!(Token::RPAREN)
          nil_ast
        else
          tuple = parse_tuple_elements
          consume_newline
          consume!(Token::RPAREN)

          ex =
            if tuple.size == 1
              tuple.first
            else
              AST.new(:__tuple__, tuple, token: lparen_token)
            end

          if current_token.type == Token::LPAREN_ARG
            parse_lambda_call(ex)
          else
            ex
          end
        end
      elsif current_token.type == Token::OPERATOR
        if UNARY_OPERATORS.include?(current_token.value)
          op_token = current_token
          unary_op = current_token.value.to_sym rescue nil
          consume!(Token::OPERATOR)
          AST.new(unary_op, [parse_expr_atom], token: op_token)
        else
          raise token_error("expected expr atom")
        end
      elsif current_token.type == Token::LBRACKET
        parse_list_literal
      elsif current_token.type == Token::LBRACE
        parse_lbrace_expr
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

    def parse_if(elsif_token = nil)
      if elsif_token
        if_token = elsif_token
      else
        if_token = current_token
        consume!(Token::KEYWORD_IF)
      end

      require_end = true

      conditional = parse_expr
      consume_terminator!
      then_expr_list = parse_expr_list

      else_expr_list =
        if current_token.type == Token::KEYWORD_ELSE
          consume!(Token::KEYWORD_ELSE)
          parse_expr_list
        elsif current_token.type == Token::KEYWORD_ELSIF
          elsif_token = current_token
          consume!(Token::KEYWORD_ELSIF)
          require_end = false
          parse_if(elsif_token)
        else
          nil_ast
        end

      consume!(Token::KEYWORD_END) if require_end

      AST.new(:__if__, [conditional, then_expr_list, else_expr_list], token: if_token)
    end

    def parse_case
      case_token = current_token
      consume!(Token::KEYWORD_CASE)

      subject = parse_expr
      consume_terminator!

      patterns = []
      current_pattern = parse_expr

      consume_newline
      consume!(Token::STAB)

      loop do
        body_and_next_pattern = parse_expr_list

        if current_token.type == Token::KEYWORD_END
          patterns << AST.new(:__pattern__, [current_pattern, body_and_next_pattern])
          break
        elsif current_token.type == Token::STAB
          raise Error.new("Every clause in a case expression must have a body", token: case_token) if body_and_next_pattern.term != :__block__

          next_pattern = body_and_next_pattern.children.pop
          body_and_next_pattern = body_and_next_pattern.children.first if body_and_next_pattern.children.count == 1
          patterns << AST.new(:__pattern__, [current_pattern, body_and_next_pattern])
          current_pattern = next_pattern

          consume!(Token::STAB)
          consume_newline

          raise Error.new("Every clause in a case expression must have a body", token: case_token) if current_token.type == Token::KEYWORD_END
        else
          raise token_error('expected keyword_end or keyword_stab')
        end
      end

      consume!(Token::KEYWORD_END)

      AST.new(:__case__, [subject] + patterns, token: case_token)
    end

    def parse_list_literal
      list_token = current_token
      consume!(Token::LBRACKET)

      list =
        if current_token.type == Token::RBRACKET
          []
        else
          parse_list_elements
        end

      consume!(Token::RBRACKET)

      AST.new(:__list__, list, token: list_token)
    end

    def parse_list_elements
      elements = []

      loop do
        elements << parse_expr
        consume_newline

        if current_token.type == Token::COMMA
          consume!(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      elements
    end

    def parse_lambda_literal
      lambda_token = current_token
      consume!(Token::LBRACE)
      consume_newline

      push_lexer

      lambda_args =
        if current_token.type == Token::LPAREN
          # Could be a typed lambda or a no-arg lambda with tuple start
          #   { (x: Int, y: Int) -> ... } or { (x, y) }
          #   { () -> ... } or { () }

          consume!(Token::LPAREN)
          consume_newline

          no_arg_lambda = true

          if current_token.type == Token::RPAREN
            consume!(Token::RPAREN)
            consume_newline

            no_arg_lambda = false if current_token.type == Token::STAB
          elsif current_token.type == Token::IDENTIFIER
            consume!(Token::IDENTIFIER)
            consume_newline

            no_arg_lambda = false if current_token.type == Token::COLON
          end

          pop_lexer

          if no_arg_lambda
            []
          else
            consume!(Token::LPAREN)
            consume_newline

            args =
              if current_token.type == Token::RPAREN
                []
              else
                parse_fn_def_args
              end

            consume_newline
            consume!(Token::RPAREN)
            consume_newline

            consume!(Token::STAB)
            consume_newline

            args
          end
        else
          # Could be an untyped lambda or a no-arg lambda with identifier start
          #   { x -> ... } or { x * x }
          #   { x, y -> ... } or { x * x }

          no_arg_lambda = true

          if current_token.type == Token::IDENTIFIER
            consume!(Token::IDENTIFIER)
            consume_newline

            no_arg_lambda = false if [Token::STAB, Token::COMMA].include?(current_token.type)
          end

          pop_lexer

          if no_arg_lambda
            []
          else
            args = parse_untyped_lambda_args

            consume_newline
            consume!(Token::STAB)
            consume_newline

            args
          end
        end

      body = parse_expr_list

      consume!(Token::RBRACE)

      meta = lambda_args.size > 0 && lambda_args.first.type.nil? ? { untyped: true } : nil
      lambda_literal_ast = AST.new(:__lambda__, [AST.new(:__args__, lambda_args), body], meta: meta, token: lambda_token)

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
          consume!(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      args
    end

    def parse_untyped_lambda_arg
      arg_token = current_token
      name = current_token.value.to_sym rescue nil
      consume!(Token::IDENTIFIER)

      AST.new(name, token: arg_token)
    end

    def parse_lambda_call(lambda_ast)
      lambda_call_token = current_token
      consume!(Token::LPAREN_ARG)
      args =
        if current_token.type == Token::RPAREN
          []
        else
          parse_tuple_elements
        end

      consume!(Token::RPAREN)

      args << parse_lambda_literal if current_token.type == Token::LBRACE

      lambda_call_ast = AST.new(lambda_ast, args, token: lambda_call_token)

      if current_token.type == Token::LPAREN_ARG
        parse_lambda_call(lambda_call_ast)
      else
        lambda_call_ast
      end
    end

    def parse_literal
      literal_token = current_token
      consume

      case literal_token.type
      when Token::EMBED
        consume_newline
        consume!(Token::COLON)
        consume_newline

        embed_type = parse_type_annotation

        AST.new(:__embed__, [AST.new(literal_token.value)], type: embed_type, token: literal_token)
      else
        AST.new(literal_token.value, type: Types::Simple.new(LITERAL_TOKENS[literal_token.type]), token: literal_token)
      end
    end

    def parse_identifier
      id_token = current_token
      id = current_token.value.to_sym rescue nil
      consume!(Token::IDENTIFIER)

      raise Error.new("Unexpected identifier '#{id}'; identifiers cannot begin with double underscore", id_token) if id.to_s.start_with?('__')

      AST.new(id, token: id_token)
    end

    def parse_fn_def
      fn_token = current_token
      consume!(Token::KEYWORD_FN)

      signature = parse_fn_def_signature
      consume_terminator!
      body = parse_expr_list

      consume!(Token::KEYWORD_END)

      AST.new(:__fn__, [signature, body], token: fn_token)
    end

    def parse_infix_def
      infix_token = current_token
      consume!(Token::KEYWORD_INFIX)

      associativity = :left
      precedence = 8

      if current_token.type == Token::IDENTIFIER
        assoc_token = current_token
        associativity = current_token.value.to_sym rescue nil
        consume!(Token::IDENTIFIER)

        raise Error.new("Unexpected associativity '#{associativity}'; infix associativity must be 'right' or 'left'", assoc_token) unless [:left, :right].include?(associativity)
      end

      if current_token.type == Token::INT
        prec_token = current_token
        precedence = current_token.value
        consume!(Token::INT)

        raise Error.new("Unexpected precedence '#{precedence}'; infix precedence must be between 0 and 9", prec_token) unless (0..9).include?(precedence)
      end

      op_token = current_token
      op = current_token.value.to_sym rescue nil
      consume!(Token::OPERATOR)

      AST.new(:__infix__, [AST.new(op, token: op_token), AST.new(associativity), AST.new(precedence)], token: infix_token)
    end

    def parse_module_def
      module_token = current_token
      consume!(Token::KEYWORD_MODULE)
      consume_newline

      name_token = current_token
      name = current_token.value.to_sym rescue nil
      consume!(Token::IDENTIFIER)
      consume_terminator!
      body = parse_top_expr_list(in_module: true)

      consume!(Token::KEYWORD_END)

      AST.new(:__module__, [AST.new(name, token: name_token), body], token: module_token)
    end

    def parse_require_directive
      require_token = current_token
      consume!(Token::KEYWORD_REQUIRE)

      file_token = current_token
      file = current_token.value rescue nil
      consume!(Token::STRING)

      AST.new(:__require__, [AST.new(file, type: Types::Simple.new(LITERAL_TOKENS[Token::STRING]), token: file_token)], token: require_token)
    end

    def parse_fn_def_signature
      consume_newline

      id_token = current_token
      id = current_token.value.to_sym rescue nil
      meta = current_token.type == Token::OPERATOR ? { operator: true } : { }
      consume!(Token::IDENTIFIER, Token::OPERATOR)

      if current_token.type == Token::OPERATOR && current_token.value == '<'
        consume!(Token::OPERATOR)

        meta[:type_params] = parse_type_parameters(simple_only: true)

        if current_token.type == Token::OPERATOR && current_token.value == '>'
          consume!(Token::OPERATOR)
        else
          raise token_error("expected '>'")
        end
      end

      if current_token.type == Token::LPAREN_ARG || current_token.type == Token::LPAREN
        consume!(Token::LPAREN_ARG, Token::LPAREN)
        consume_newline

        args =
          if current_token.type == Token::RPAREN
            []
          else
            parse_fn_def_args(meta[:type_params])
          end

        consume_newline
        consume!(Token::RPAREN)
      else
        raise token_error("expected '(' followed by function arguments")
      end

      consume_newline

      consume!(Token::STAB)
      consume_newline
      return_type = parse_type_annotation(meta[:type_params])

      AST.new(id, args, type: return_type, meta: meta.empty? ? nil : meta, token: id_token)
    end

    def parse_fn_def_args(type_params = nil)
      args = []

      loop do
        args << parse_fn_def_arg(type_params)
        consume_newline

        if current_token.type == Token::COMMA
          consume!(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      args
    end

    def parse_fn_def_arg(type_params)
      name_token = current_token
      name = current_token.value.to_sym rescue nil
      consume!(Token::IDENTIFIER)
      consume_newline

      consume!(Token::COLON)
      consume_newline

      AST.new(name, type: parse_type_annotation(type_params), token: name_token)
    end

    def parse_type_annotation(type_params = nil)
      type = parse_type_union(type_params)

      if current_token.type == Token::STAB
        consume!(Token::STAB)
        consume_newline

        Types::Lambda.new(type.is_a?(Types::Tuple) ? type.types : [type], parse_type_annotation(type_params))
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
          consume!(Token::OPERATOR)
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
        consume!(Token::LPAREN)
        consume_newline

        possible_tuple =
          if current_token.type == Token::RPAREN
            []
          else
            parse_type_parameters(type_params: type_params)
          end

        consume!(Token::RPAREN)

        if possible_tuple.size == 1 && !possible_tuple.first.is_a?(Types::Tuple)
          possible_tuple.first
        else
          Types::Tuple.new(possible_tuple)
        end
      elsif current_token.type == Token::LBRACE
        consume!(Token::LBRACE)
        consume_newline

        record_type = parse_record_type_elements(type_params)

        consume!(Token::RBRACE)

        Types::Record.new(record_type)
      elsif current_token.type == Token::LBRACKET
        consume!(Token::LBRACKET)
        consume_newline

        list_type_parameter = parse_type_annotation(type_params)

        consume!(Token::RBRACKET)

        Types::Generic.new(:List, [list_type_parameter])
      else
        type_path = []

        loop do
          type_token = current_token
          type_path << current_token.value.to_sym rescue nil
          consume!(Token::IDENTIFIER)

          raise Error.new("The type or module '#{type}' must begin with an uppercase letter", type_token) unless ('A'..'Z').include?(type_path.last[0])

          if current_token.type == Token::OPERATOR && current_token.value == '.'
            consume!(Token::OPERATOR)
          else
            break
          end
        end

        type = type_path.pop

        if current_token.type == Token::OPERATOR && current_token.value == '<' || current_token.type == Token::NEWLINE && peek.type == Token::OPERATOR && peek.value == '<'
          consume_newline
          consume!(Token::OPERATOR)
          consume_newline

          type_params = parse_type_parameters(type_params: type_params)

          if current_token.type != Token::OPERATOR || !current_token.value.start_with?('>')
            raise token_error("expected '>'")
          elsif current_token.value != '>'
            op = current_token.value[1..-1]
            location = current_token.location.dup
            location.col += 1
            @token_buffer << Token.new(Token::OPERATOR, op, raw: op, location: location)
          end
          consume!(Token::OPERATOR)

          Types::Generic.new(type, type_params, module_path: type_path)
        else
          Types::Simple.new(type, parameter_type: type_params&.include?(type), module_path: type_path)
        end
      end
    end

    def parse_type_parameters(simple_only: false, type_params: nil)
      params = []

      loop do
        params <<
          if simple_only
            type = current_token.value.to_sym rescue nil
            consume!(Token::IDENTIFIER)

            type
          else
            parse_type_annotation(type_params)
          end

        consume_newline

        if current_token.type == Token::COMMA
          consume!(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      params
    end

    def parse_record_type_elements(type_params = nil)
      types = {}

      loop do
        field_token = current_token
        field = current_token.value.to_sym rescue nil
        consume!(Token::IDENTIFIER)
        consume_newline

        consume!(Token::COLON)
        consume_newline

        raise Error.new("The field '#{field}' cannot be used more than once in the same record type", field_token) if types.key?(field)

        types[field] = parse_type_annotation(type_params)

        consume_newline

        if current_token.type == Token::COMMA
          consume!(Token::COMMA)
          consume_newline
        else
          break
        end
      end

      types
    end

    def parse_fn_call(ambiguous_op: nil)
      id_token = current_token
      id = current_token.value.to_sym
      consume!(Token::IDENTIFIER)

      if current_token.type == Token::LPAREN_ARG
        consume!(Token::LPAREN_ARG)
        args =
          if current_token.type == Token::RPAREN
            []
          else
            parse_tuple_elements
          end

        consume!(Token::RPAREN)

        args << parse_lambda_literal if current_token.type == Token::LBRACE

        fn_call_ast = AST.new(id, args, meta: !ambiguous_op.nil? ?  { ambiguous_op: true } : nil, token: id_token)

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
          AST.new(id, [parse_lbrace_expr], meta: !ambiguous_op.nil? ?  { ambiguous_op: true, no_paren: true } : { no_paren: true }, token: id_token)
        else
          AST.new(id, parse_tuple_elements, meta: !ambiguous_op.nil? ?  { ambiguous_op: true, no_paren: true } : { no_paren: true }, token: id_token)
        end
      end
    end

    def parse_lbrace_expr
      # Look ahead to see if this is a record or a lambda
      push_lexer
      record_literal = false
      consume!(Token::LBRACE)
      consume_newline

      if current_token.type == Token::RBRACE
        record_literal = true
      elsif current_token.type == Token::IDENTIFIER
        consume!(Token::IDENTIFIER)
        consume_newline

        record_literal = true if current_token.type == Token::COLON
      end

      pop_lexer

      if record_literal
        parse_record_literal
      else
        parse_lambda_literal
      end
    end

    def parse_record_literal
      record_token = current_token
      consume!(Token::LBRACE)
      consume_newline

      if current_token.type == Token::RBRACE
        consume!(Token::RBRACE)
        nil_ast
      else
        elements = []

        loop do
          field_token = current_token
          field = current_token.value.to_sym rescue nil
          consume!(Token::IDENTIFIER)

          raise Error.new("Unexpected field '#{field}'; record fields cannot begin with an uppercase letter", field_token) if ('A'..'Z').include?(field[0])
          raise Error.new("The field '#{field}' cannot be used more than once in the same record", field_token) if elements.map(&:term).include?(field)

          consume_newline
          consume!(Token::COLON)
          consume_newline

          elements << AST.new(field, [parse_expr], token: field_token)

          consume_newline

          if current_token.type == Token::COMMA
            consume!(Token::COMMA)
          else
            break
          end
        end

        consume!(Token::RBRACE)
        AST.new(:__record__, elements, token: record_token)
      end
    end

    def parse_tuple_elements
      elements = []

      loop do
        elements << parse_expr
        consume_newline

        if current_token.type == Token::COMMA
          consume!(Token::COMMA)
        else
          break
        end
      end

      elements
    end

    def parse_typealias_def
      typealias_token = current_token
      consume!(Token::KEYWORD_TYPEALIAS)
      consume_newline

      id_token = current_token
      id = current_token.value.to_sym rescue nil
      consume!(Token::IDENTIFIER)
      consume_newline

      raise Error.new("The type alias '#{id}' must begin with an uppercase letter", id_token) unless ('A'..'Z').include?(id[0])

      meta = {}

      if current_token.type == Token::OPERATOR && current_token.value == '<'
        consume!(Token::OPERATOR)

        meta[:type_params] = parse_type_parameters(simple_only: true)

        if current_token.type == Token::OPERATOR && current_token.value == '>'
          consume!(Token::OPERATOR)
        else
          raise token_error("expected '>'")
        end
      end

      consume_newline

      if current_token.type == Token::OPERATOR && current_token.value == '='
        consume!(Token::OPERATOR)
        consume_newline
      else
        raise token_error("expected '='")
      end

      aliased_type = parse_type_annotation(meta[:type_params])

      AST.new(:__typealias__, [AST.new(id, type: aliased_type, meta: meta, token: id_token)], token: typealias_token)
    end

    def nil_ast
      AST.new(nil, type: Types::Simple.new(:Nil))
    end
  end
end
