$MATT_LOAD_PATH = File.realpath("#{__dir__}/../src")

module Mattlang
  class CompilerError < StandardError
    def self.title; 'Compiler Error' end
  end
end

require 'mattlang/version'
require 'mattlang/location'
require 'mattlang/token'
require 'mattlang/types'
require 'mattlang/scope'
require 'mattlang/function'
require 'mattlang/variable'
require 'mattlang/lexer'
require 'mattlang/ast'
require 'mattlang/parser'
require 'mattlang/semantic'
require 'mattlang/interpreter'
require 'mattlang/repl'
