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

      # In a lambda type, args are contravariant and the return type is covariant
      def subtype?(other, type_bindings = nil, same_parameter_types = false)
        if other.is_a?(Lambda)
          other.args.size == args.size &&
            return_type.subtype?(other.return_type, type_bindings, same_parameter_types) &&
            other.args.zip(args).all? do |other_arg, arg|
              if arg.parameter_type?
                # If this type's arg is a parameter type, then reverse the subtype
                # check, so that other_arg's type can try to bind to the type parameter.
                arg.subtype?(other_arg, type_bindings, same_parameter_types)
              else
                other_arg.subtype?(arg, type_bindings, same_parameter_types)
              end
            end
        elsif other.is_a?(Union)
          other.types.all? { |t| self.subtype?(t, type_bindings, same_parameter_types) }
        elsif other.nothing?
          true
        else
          false
        end
      end

      def parameter_type?
        args.any?(&:parameter_type?) || return_type.parameter_type?
      end

      def replace_type_bindings(type_bindings)
        Lambda.new(args.map { |a| a.replace_type_bindings(type_bindings) }, return_type.replace_type_bindings(type_bindings))
      end

      def ==(other)
        other.is_a?(Lambda) && other.args == args && other.return_type == return_type
      end

      def to_s
        if args.size == 1
          arg = args.first

          if arg.is_a?(Lambda) || arg.is_a?(Tuple)
            "(#{arg}) -> #{return_type}"
          else
            "#{arg} -> #{return_type}"
          end
        else
          "(#{args.map(&:to_s).join(', ')}) -> #{return_type}"
        end
      end
    end
  end
end
