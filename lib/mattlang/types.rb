require 'mattlang/types/base'
require 'mattlang/types/simple'
require 'mattlang/types/union'
require 'mattlang/types/generic'
require 'mattlang/types/lambda'
require 'mattlang/types/tuple'
require 'mattlang/types/record'

module Mattlang
  module Types
    def self.combine(types)
      combined = types.reduce([]) do |memo, type|
        if type.is_a?(Union)
          memo += type.types
        else
          memo << type
        end
      end.uniq.reject(&:nothing?)

      if combined.size == 1
        combined.first
      else
        Union.new(combined)
      end
    end

    def self.nothing
      Types::Simple.new(:Nothing)
    end

    def self.anything
      Types::Simple.new(:Anything)
    end
  end
end
