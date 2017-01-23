module Mattlang
  module Types
    # (Int, Int) -> Int
    class Lambda < Base
      attr_reader :args, :return_type
      
      def initialize(args, return_type)
        @args = args
        @return_type = return_type

        raise "All parameter types in a lambda type must inherit from Types::Base" if !@args.all? { |t| t.is_a?(Types::Base) }
        raise "The return type of a lambda type must inherit from Types::Base" if !@return_type.is_a?(Types::Base)
      end

      def to_s
        if args.size == 1
          "#{args.first} -> #{return_type}"
        else
          "(#{args.map(&:to_s).join(', ')}) -> #{return_type}"
        end
      end
    end
  end
end
