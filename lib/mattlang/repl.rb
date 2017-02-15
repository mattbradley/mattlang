require 'readline'
require 'term/ansicolor'

module Mattlang
  class REPL
    include Term::ANSIColor
    extend Term::ANSIColor

    PROMPT = bold { 'matt> ' }
    DOT_PROMPT = bold { ' ...> ' }

    def self.start
      new.start
    end

    def start
      semantic = Semantic.new(Dir.pwd)
      interpreter = Interpreter.new(semantic.global_scope)

      loop do
        ast = read
        break if ast.nil?

        ast = semantic.analyze(ast)
        result = interpreter.interpret(ast)

        print " => ", bold { yellow { result.value.inspect } }, ' : ', magenta { result.type.to_s }, "\n"
      end

      puts
    end

    private

    def read
      line = readline(PROMPT)
      return nil if line.nil?

      if line =~ /^\s*$/
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
            puts bold { red { e.message } }
            read
          end
        rescue Parser::Error => e
          puts bold { red { e.message } }
          read
        end
      end
    rescue Interrupt
      puts
      read
    end

    def readline(prompt)
      line = Readline.readline(prompt, true)

      if line
        Readline::HISTORY.pop if line =~ /^\s*$/ || Readline::HISTORY.size > 2 && line == Readline::HISTORY[Readline::HISTORY.size - 2]
        line
      end
    end
  end
end
