module Mattlang
  # A code location in some source file.
  # A nil filename probably means the source came from the REPL.
  class Location
    attr_accessor :source, :filename, :line, :col

    def initialize(source: nil, filename: nil, line: nil, col: nil)
      @filename = filename
      @line = line
      @source = source
      @col = col
    end
  end
end
