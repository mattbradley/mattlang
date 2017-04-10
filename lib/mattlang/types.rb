require 'mattlang/types/base'
require 'mattlang/types/simple'
require 'mattlang/types/union'
require 'mattlang/types/intersection'
require 'mattlang/types/generic'
require 'mattlang/types/lambda'
require 'mattlang/types/tuple'
require 'mattlang/types/record'
require 'mattlang/types/nominal'

module Mattlang
  module Types
    def self.union(types)
      unioned = types.reduce([]) do |memo, type|
        if type.is_a?(Union)
          memo += type.types
        else
          memo << type
        end
      end.uniq.reject(&:nothing?)

      if unioned.size == 1
        unioned.first
      else
        Union.new(unioned)
      end
    end

    def self.intersect(types)
      intersected = types.reduce([]) do |memo, type|
        if type.is_a?(Intersection)
          memo += type.types
        else
          memo << type
        end
      end.uniq.reject(&:anything?)

      if intersected.size == 1
        intersected.first
      else
        Intersection.new(intersected)
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
