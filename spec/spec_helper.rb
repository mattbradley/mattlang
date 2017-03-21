$LOAD_PATH.unshift File.expand_path('../../lib', __FILE__)
require 'byebug'
require 'rspec/its'
require 'mattlang'
require 'rspec/expectations'

RSpec::Matchers.define :eq_type do |expected|
  match do |actual|
    Mattlang::Parser.debug_type(expected) == actual
  end
end

RSpec::Matchers.define :eq_bindings do |expected|
  match do |actual|
    expected.map { |k, v| [k, Mattlang::Parser.debug_type(v)] }.to_h == actual
  end
end
