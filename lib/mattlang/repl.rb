require 'readline'
require 'term/ansicolor'
require 'mattlang/repl/history'

module Mattlang
  class REPL
    include Term::ANSIColor
    extend Term::ANSIColor

    PROMPT = bold { cyan { 'matt> ' } }
    DOT_PROMPT = bold { cyan { ' ...> ' } }

    def self.start
      new.start
    end

    def initialize
      @history = History.new
      @history.load
    end

    def start
      semantic = Semantic.new(Dir.pwd)
      interpreter = Interpreter.new(semantic.global_scope)

      loop do
        begin
          ast = read
          break if ast.nil?

          ast = semantic.analyze(ast)
          result = interpreter.interpret(ast)

          print "   => ", bold { yellow { result.value.inspect } }, ' : ', magenta { result.type.to_s }, "\n"
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
        rescue Parser::UnexpectedTokenError => e
          if e.token.type == Token::EOF
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
      line = Readline.readline(prompt)&.strip

      if line
        @history.push(line)
        line
      end
    end

    def print_error(e)
      puts
      puts bold { on_red { "    #{e.class.title.upcase}    " } }
      puts
      puts "#{e.message}."

      location, token =
        case e
        when Lexer::Error then [e.location, nil]
        when Parser::Error then [e.token&.location, e.token]
        when Semantic::Error, Scope::Error then [e.ast&.token&.location, e.ast&.token || e.token]
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

        puts bold { yellow { on_red { prefix } } } + ' ' + location.source.lines[location.line]&.chomp.to_s
        puts ' ' * (prefix.length + location.col + 1) + bold { red { '~' * (token&.raw&.size || 1) } }
      end

      puts
    end
  end
end
