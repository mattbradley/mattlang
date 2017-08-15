module Mattlang
  class Codegen
    module Utils
      def indent(src)
        src.lines.map { |l| "    #{l}" }.join
      end
    end
  end
end
