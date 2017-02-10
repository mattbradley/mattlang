require 'mattlang/version'
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

$MATT_LOAD_PATH = File.realpath("#{__dir__}/../src")

module Mattlang
end
