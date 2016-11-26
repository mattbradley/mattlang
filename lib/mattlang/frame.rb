module Mattlang
  class Frame
    attr_accessor :binding, :return_value

    def initialize(binding = {})
      @binding = binding
      @return_value = nil
    end

    def define(name, value)
      @binding[name] = value
    end
  end
end
