$MATT_LOAD_PATH = File.realpath("#{__dir__}/../src")

module Mattlang
  class CompilerError < StandardError
    def self.title; 'Compiler Error' end

    attr_accessor :token

    def initialize(message, token = nil)
      @token = token
      super(message)
    end
  end
end

require 'mattlang/version'
require 'mattlang/location'
require 'mattlang/token'
require 'mattlang/types'
require 'mattlang/scope'
require 'mattlang/function'
require 'mattlang/lexer'
require 'mattlang/ast'
require 'mattlang/parser'
require 'mattlang/semantic'
require 'mattlang/interpreter'
require 'mattlang/repl'
