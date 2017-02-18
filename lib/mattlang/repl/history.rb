module Mattlang
  class REPL
    class History
      def initialize(filename = "~/.matt_history")
        @history_file_path = File.expand_path(filename)
        @history_file = File.open(@history_file_path, 'a', 0600)
        @history_file.sync = true
      end

      def load
        if File.exist?(@history_file_path)
          File.foreach(@history_file_path) do |line|
            Readline::HISTORY << line.chomp
          end
        end
      end

      def push(line)
        if !line.empty? && (Readline::HISTORY.size == 0 || line != Readline::HISTORY[Readline::HISTORY.size - 1])
          Readline::HISTORY << line
          @history_file.puts line
        end
      end
    end
  end
end
