require 'readline'
require 'term/ansicolor'
require 'mattlang/repl/history'

module Mattlang
  class REPL
    include Term::ANSIColor
    extend Term::ANSIColor

    PROMPT = bold { cyan { 'matt> ' } }
    DOT_PROMPT = bold { cyan { ' ...> ' } }
    UNDERLINE_ERROR_CHAR = '‾'

    def self.start
      new.start
    end

    def initialize
      @history = History.new
      @history.load
    end

    def start
      begin
        semantic = Semantic.new(Dir.pwd)
      rescue CompilerError => e
        print_error(e)
        exit
      end

      interpreter = Interpreter.new(semantic.global_scope)

      loop do
        begin
          ast = read
          break if ast.nil?

          ast = semantic.analyze(ast)
          result = interpreter.interpret(ast)

          value_str, type_str = result.repl_format(->(v) { bold { yellow { v } } }, ->(t) { magenta { t } })
          print bold { cyan { "   => " } }, value_str, ' : ', type_str, "\n"
        rescue CompilerError => e
          print_error(e)
        end
      end

      puts
    end

    private

    def read
      line = readline(PROMPT)
      return nil if line.nil?

      if line.empty?
        read
      else
        begin
          Parser.new(line).parse
        rescue Lexer::UnexpectedEOFError, Parser::UnexpectedTokenError => e
          if e.is_a?(Lexer::UnexpectedEOFError) || e.token.type == Token::EOF
            next_line = readline(DOT_PROMPT)
            return nil if next_line.nil?

            line += "\n" + next_line
            retry
          else
            raise e
          end
        end
      end
    rescue Interrupt
      puts
      read
    end

    def readline(prompt)
      line = Readline.readline(prompt)

      if line
        @history.push(line)
        line.strip
      end
    end

    def print_error(e)
      puts
      puts bold { on_red { "    #{e.class.title.upcase}    " } }
      puts
      puts "#{e.message}"

      location, token =
        case e
        when Lexer::Error then [e.location, nil]
        when Parser::Error then [e.token&.location, e.token]
        when Semantic::Error, Semantic::Scope::Error then [e.ast&.token&.location, e.ast&.token || e.token]
        end

      if location
        puts

        prefix =
          if location.filename
            line_num = location.line + 1
            puts bold { yellow { "--- #{location.filename}:#{line_num}" } }
            " #{line_num} "
          else
            ' > '
          end

        puts bold { on_red { prefix } } + ' ' + location.source.lines[location.line]&.chomp.to_s
        puts ' ' * (prefix.length + location.col + 1) + bold { red { UNDERLINE_ERROR_CHAR * (token&.raw&.size || 1) } }
      end
    end
  end
end
