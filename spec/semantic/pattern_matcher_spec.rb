require 'spec_helper'
include Mattlang

describe Semantic::PatternMatcher do
  context 'type checking patterns' do
    let(:parsed_source) { parse(source) }
    let(:parsed_type) { Parser.debug_type(type) }

    context 'with succeeding type checks' do
      subject { Semantic::PatternMatcher.build_pattern(parsed_source, parsed_type) }

      context 'with a simple contant pattern' do
        let(:type) { 'Int' }
        let(:source) { '10' }

        its(:kind) { is_expected.to eq :literal }
        its(:args) { is_expected.to eq [10] }
        its('type.type_atom') { is_expected.to eq :Int }
      end

      context 'with a simple wildcard pattern' do
        let(:type) { 'Int' }
        let(:source) { 'x' }

        its(:kind) { is_expected.to eq :wildcard }
        its(:args) { is_expected.to be_empty }
        its('type.type_atom') { is_expected.to eq :Int }
        its(:bindings) { is_expected.to eq({ x: Parser.debug_type('Int') }) }
      end

      context 'with a tuple with constants' do
        let(:type) { '(Int, String)' }
        let(:source) { '(404, "not_found")' }

        its(:kind) { is_expected.to eq :tuple }
        its(:type) { is_expected.to eq Parser.debug_type('(Int, String)') }
        
        it 'has the correct args' do
          expect(subject.args[0].kind).to eq :literal
          expect(subject.args[0].type.type_atom).to eq :Int
          expect(subject.args[1].kind).to eq :literal
          expect(subject.args[1].type.type_atom).to eq :String
        end
      end

      context 'with a simple constant on a union type' do
        let(:type) { 'Int | Nil' }
        let(:source) { '10' }

        its(:kind) { is_expected.to eq :literal }
        its(:args) { is_expected.to eq [10] }
        its('type.type_atom') { is_expected.to eq :Int }
      end

      context 'with a tuple on a union type' do
        let(:type) { '(Int, Int) | (String, Bool)' }
        let(:source) { '(a, b)' }

        its(:kind) { is_expected.to eq :tuple }
        its(:type) { is_expected.to eq Parser.debug_type('(Int, Int) | (String, Bool)') }
        its(:bindings) { is_expected.to eq({ a: Parser.debug_type('Int | String'), b: Parser.debug_type('Int | Bool') }) }

        it 'has the correct args' do
          expect(subject.args[0].kind).to eq :wildcard
          expect(subject.args[0].type).to be_a Types::Union
          expect(subject.args[1].kind).to eq :wildcard
          expect(subject.args[1].type).to be_a Types::Union
        end
      end

      context 'with a (constant, wilcard) tuple that matches only one type in a union' do
        let(:type) { '(Int, Int) | (String, Bool)' }
        let(:source) { '(0, x) ' }

        its(:kind) { is_expected.to eq :tuple }
        its(:type) { is_expected.to eq Parser.debug_type('(Int, Int)') }
        its(:bindings) { is_expected.to eq({ x: Parser.debug_type('Int') }) }

        it 'has the correct args' do
          expect(subject.args[0].kind).to eq :literal
          expect(subject.args[0].type.type_atom).to eq :Int
          expect(subject.args[1].kind).to eq :wildcard
          expect(subject.args[1].type.type_atom).to eq :Int
        end
      end

      context 'with a record type that matches only one type in a union' do
        let(:type) { '{ code: Int, body: String | Nil } | { body: String, code: Int | Nil, status: String }' }
        let(:source) { '{ body: body, code: nil }' }

        its(:kind) { is_expected.to eq :record }
        its(:type) { is_expected.to eq Parser.debug_type('{ body: String, code: Nil, status: String }') }
        its(:bindings) { is_expected.to eq({ body: Parser.debug_type('String') }) }

        it 'has the correct args in field sorted order' do
          expect(subject.args[0].kind).to eq :wildcard
          expect(subject.args[0].type.type_atom).to eq :String
          expect(subject.args[1].kind).to eq :literal
          expect(subject.args[1].type.type_atom).to eq :Nil
        end
      end

      context 'with a nested constructed pattern' do
        let(:type) { '{ body: String, code: (Int, String) }' }
        let(:source) { '{ body: body, code: (404, s) }' }

        its(:kind) { is_expected.to eq :record }
        its(:type) { is_expected.to eq Parser.debug_type('{ body: String, code: (Int, String) }') }
        its(:bindings) { is_expected.to eq({ body: Parser.debug_type('String'), s: Parser.debug_type('String') }) }

        it 'has the correct nested type' do
          expect(subject.args[1].kind).to eq :tuple
          expect(subject.args[1].type).to eq Parser.debug_type('(Int, String)')
        end
      end

      context 'with a record with an implicit wildcard' do
        let(:type) { '{ body: String, code: Int }' }
        let(:source) { '{ code: c }' }

        its(:kind) { is_expected.to eq :record }
        its(:type) { is_expected.to eq Parser.debug_type('{ body: String, code: Int }') }
        its(:bindings) { is_expected.to eq({ c: Parser.debug_type('Int') }) }

        it 'has the correct nested type' do
          expect(subject.args.count).to eq 1
          expect(subject.args[0].kind).to eq :wildcard
          expect(subject.args[0].type).to eq Parser.debug_type('Int')
        end
      end
    end

    context 'with failing type checks' do
      subject { -> { Semantic::PatternMatcher.build_pattern(parsed_source, parsed_type) } }

      context 'with a simple contant pattern' do
        let(:type) { 'Int' }
        let(:source) { 'nil' }

        it { is_expected.to raise_error(Semantic::PatternMatcher::NoMatchError) }
      end

      context 'with a tuple pattern' do
        let(:type) { '(Int, String)' }
        let(:source) { '(i, (x, y))' }

        it { is_expected.to raise_error(Semantic::PatternMatcher::NoMatchError) }
      end

      context 'with a record pattern' do
        let(:type) { '{ body: String | Nil, code: Int } | { body: String, code: String }' }
        let(:source) { '{ body: nil, code: "not_found" }' }

        it { is_expected.to raise_error(Semantic::PatternMatcher::NoMatchError) }
      end

      context 'with duplicate variables' do
        let(:type) { '(String, (Int, String))' }
        let(:source) { '(s, (0, s))' }

        it { is_expected.to raise_error(Semantic::PatternMatcher::InvalidPatternError) }
      end

      context 'with invalid expression' do
        let(:type) { 'Int' }
        let(:source) { 'foo(x)' }

        it { is_expected.to raise_error(Semantic::PatternMatcher::InvalidPatternError) }
      end
    end
  end

  context 'checking usefulness' do
    let(:parsed_type) { Parser.debug_type(type) }
    let(:matrix) { patterns.map { |p| [Semantic::PatternMatcher.build_pattern(parse(p), parsed_type)] } }
    let(:vector) { [Semantic::PatternMatcher.build_pattern(parse(candidate), parsed_type)] }
    let(:debugger) { false }
    subject { Semantic::PatternMatcher.useful?(matrix, vector, [parsed_type], debugger: debugger) }

    context 'with a wildcard following by a wildcard' do
      let(:patterns) { ['x'] }
      let(:candidate) { '_' }
      let(:type) { 'Int' }

      it { is_expected.to eq false }
    end

    context 'with a wildcard following a literal' do
      let(:patterns) { ['0'] }
      let(:candidate) { '_' }
      let(:type) { 'Int' }

      it { is_expected.to eq true }
    end

    context 'with a wildcard following a tuple' do
      let(:patterns) { ['(0, 0)'] }
      let(:candidate) { 't' }
      let(:type) { '(Int, Int)' }

      it { is_expected.to eq true }
    end

    context 'with a wildcard following a wildcarded tuple' do
      let(:patterns) { ['(a, b)'] }
      let(:candidate) { '_' }
      let(:type) { '(Int, Int)' }

      it { is_expected.to eq false }
    end

    context 'with a literal tuple following a wildcard tuple' do
      let(:patterns) { ['(a, b)'] }
      let(:candidate) { '(0, _)' }
      let(:type) { '(Int, Int)' }

      it { is_expected.to eq false }
    end

    context 'with a tuple following a wildcard' do
      let(:patterns) { ['_'] }
      let(:candidate) { '(a, b)' }
      let(:type) { '(Int, Int) | (String, Bool)' }

      it { is_expected.to eq false }
    end

    context 'with a half-literal tuple following a half-literal tuple of a different type' do
      let(:patterns) { ['(i, 0.0)'] }
      let(:candidate) { '("hi", b)' }
      let(:type) { '(Int, Float) | (String, Bool)' }

      it { is_expected.to eq true }
    end

    context 'with a 2-tuple following a 3-tuple' do
      let(:patterns) { ['(a, b, c)'] }
      let(:candidate) { '(a, b)' }
      let(:type) { '(Int, Int) | (Int, Int, Int)' }

      it { is_expected.to eq true }
    end

    context 'with complex literal specialization in a tuple' do
      let(:patterns) { [
        '(0, b, c)',
        '(a, 0, c)',
        '(a, b, 0)'
      ] }
      let(:candidate) { '(0, nil, c)' } # Any matching values would have already matched the first pattern above
      let(:type) { '(Int, Int | Nil, Int)' }

      it { is_expected.to eq false }
    end

    context 'with a wildcard record after a literal record' do
      let(:patterns) { ['{ a: a, b: 0 }'] }
      let(:candidate) { '{ a: a, b: b }' }
      let(:type) { '{ a: Int, b: Int }' }

      it { is_expected.to eq true }
    end

    context 'with a literal record after a wildcard record' do
      let(:patterns) { ['{ a: a, b: b }'] }
      let(:candidate) { '{ a: a, b: 0 }' }
      let(:type) { '{ a: Int, b: Int }' }

      it { is_expected.to eq false }
    end

    context 'with more generic record following a wildcard record' do
      let(:patterns) { ['{ a: a, b: b }'] }
      let(:candidate) { '{ a: a }' }
      let(:type) { '{ a: Int, b: Int }' }

      it { is_expected.to eq false }
    end

    context 'with a more specific record following a wildcard record' do
      let(:patterns) { ['{ a: a }'] }
      let(:candidate) { '{ a: a, b: b }' }
      let(:type) { '{ a: Int, b: Int } | { a: Int }' }

      it { is_expected.to eq false }
    end

    context 'with a union of differently-sized record types' do
      let(:patterns) { ['{ a: a, b: b }'] }
      let(:candidate) { '{ a: a }' }
      let(:type) { '{ a: Int } | { a: Int, b: Int }' }

      it { is_expected.to eq true }
    end

    context 'with a record following a tuple' do
      let(:patterns) { ['{ a: _, b: _, c: _ }'] }
      let(:candidate) { '(a, b, c)' }
      let(:type) { '{ a: Int, b: Int, c: Int } | (Int, Int, Int)' }

      it { is_expected.to eq true }
    end

    context 'with a diagonal literal matrix' do
      let(:patterns) { [
        '(0, 0, _, _, _, _, _, _)',
        '(_, _, 0, 0, _, _, _, _)',
        '(_, _, _, _, 0, 0, _, _)',
        '(_, _, _, _, _, _, 0, 0)'
      ] }
      let(:candidate) { '(0, nil, 0, nil, 0, nil, 0, nil)' }
      let(:type) { '(Int | Nil, Int | Nil, Int | Nil, Int | Nil, Int | Nil, Int | Nil, Int | Nil, Int | Nil)' }

      it { is_expected.to eq true }
    end

    context 'with a literal nil "constructor" and a literal in a tuple' do
      let(:patterns) { [ '(nil, 1)' ] }
      let(:candidate) { '(nil, 0)' }
      let(:type) { '(Nil, Int)' }

      it { is_expected.to eq true }
    end

    context 'with a literal nil "constructor" and a duplicate literal in a tuple' do
      let(:patterns) { [ '(nil, 1)' ] }
      let(:candidate) { '(nil, 1)' }
      let(:type) { '(Nil, Int)' }

      it { is_expected.to eq false }
    end

    context 'with a nil wildcard and a literal in a tuple' do
      let(:patterns) { [ '(nil, 1)' ] }
      let(:candidate) { '(x, 0)' }
      let(:type) { '(Nil, Int)' }

      it { is_expected.to eq true }
    end

    context 'with a literal bool "constructor" and a literal in a tuple' do
      let(:patterns) { [ '(true, 0)' ] }
      let(:candidate) { '(false, 0)' }
      let(:type) { '(Bool, Int)' }

      it { is_expected.to eq true }
    end

    context 'with a complete literal bool "constructor" and a duplicate literal in a tuple' do
      let(:patterns) { [
        '(true, 0)',
        '(false, 0)'
      ] }
      let(:candidate) { '(x, 0)' }
      let(:type) { '(Bool, Int)' }

      it { is_expected.to eq false }
    end

    context 'with before and after literals with a complete literal bool "constructor"' do
      let(:patterns) { [
        '(0, true, 0)',
        '(0, false, 0)'
      ] }
      let(:candidate) { '(0, x, 0)' }
      let(:type) { '(Int, Bool, Int)' }

      it { is_expected.to eq false }
    end
  end

  context 'checking exhaustiveness' do
    let(:parsed_type) { Parser.debug_type(type) }
    let(:matrix) { patterns.map { |p| [Semantic::PatternMatcher.build_pattern(parse(p), parsed_type)] } }
    subject { Semantic::PatternMatcher.exhaustive?(matrix, [parsed_type]) }

    context 'with a simple literal' do
      let(:patterns) { [ '0' ] }
      let(:type) { 'Int' }

      its(:count) { is_expected.to eq 1 }
      its('first.kind') { is_expected.to eq :complement }
      its('first.args.first.kind') { is_expected.to eq :literal }
      its('first.args.first.args.first') { is_expected.to eq 0 }
    end

    context 'with an exhaustive literal' do
      let(:patterns) { [
        '0',
        '1',
        '2',
        '_'
      ] }
      let(:type) { 'Int' }

      it { is_expected.to be_nil }
    end

    context 'with an non-exhaustive tuple' do
      let(:patterns) { [
        '(0, b)',
        '(a, 0)',
      ] }
      let(:type) { '(Int, Int)' }

      it { is_expected.to_not be_empty }
    end

    context 'with an exhaustive tuple' do
      let(:patterns) { [
        '(0, b)',
        '(_, _)'
      ] }
      let(:type) { '(Int, Int)' }

      it { is_expected.to be_nil }
    end

    context 'with an exhaustive tuple with a wildcard' do
      let(:patterns) { [
        '(0, b)',
        '_'
      ] }
      let(:type) { '(Int, Int)' }

      it { is_expected.to be_nil }
    end

    context 'with a non-exhaustive Int | tuple | record type' do
      let(:patterns) { [ '0' ] }
      let(:type) { 'Int | (Int, Int) | { x: Int, y: Int }' }

      it 'is non-exhaustive and has the correct missing patterns' do
        missing_patterns = subject()
        expect(missing_patterns).to_not be_empty

        expect(missing_patterns.map(&:kind).sort).to eq [:complement, :record, :tuple]
      end
    end

    context 'with a partially-exhaustive Int | tuple | record type' do
      let(:patterns) { [
        '0',
        '(_, _)'
      ] }
      let(:type) { 'Int | (Int, Int) | { x: Int, y: Int }' }

      it 'is non-exhaustive and has the correct missing patterns' do
        missing_patterns = subject()
        expect(missing_patterns).to_not be_empty

        expect(missing_patterns.map(&:kind).sort).to eq [:complement, :record]
      end
    end

    context 'with a union of overlapping record types' do
      let(:patterns) { [
        '{ a: a, b: b }'
      ] }
      let(:type) { '{ a: String } | { a: String, b: Int }' }

      it { is_expected.to_not be_empty }
    end

    context 'with an exhaustive union of overlapping record types' do
      let(:patterns) { [
        '{ a: a, b: b }',
        '{ a: a }'
      ] }
      let(:type) { '{ a: String } | { a: String, b: Int }' }

      it { is_expected.to be_nil }
    end

    context 'with a single-pattern exhaustive union of overlapping record types' do
      let(:patterns) { [
        '{ a: a }'
      ] }
      let(:type) { '{ a: String } | { a: String, b: Int }' }

      it { is_expected.to be_nil }
    end

    context 'with a record | Nil inside of a tuple' do
      let(:patterns) { [
        '({ a: a }, 0)',
        '(x, 0)'
      ] }
      let(:type) { '({ a: Int } | Nil, Int)' }

      it { is_expected.to_not be_empty }
    end

    context 'with true and false literals' do
      let(:patterns) { [
        'true',
        'false'
      ] }
      let(:type) { 'Bool' }

      it { is_expected.to be_nil }
    end

    context 'with single nil literal' do
      let(:patterns) { [ 'nil' ] }
      let(:type) { 'Nil' }

      it { is_expected.to be_nil }
    end

    context 'with missing literals in Bool | Nil union' do
      let(:patterns) { [ 'true' ] }
      let(:type) { 'Bool | Nil' }

      it 'is non-exhaustive and has the correct missing patterns' do
        missing_patterns = subject()
        expect(missing_patterns).to_not be_empty

        expect(missing_patterns.map(&:kind).sort).to eq [:literal, :literal]
        expect(missing_patterns.map(&:args).map(&:first).sort_by(&:inspect)).to eq [false, nil]
      end
    end
  end

  context 'checking variable binding type elimination' do
    let(:parsed_type) { Parser.debug_type(type) }
    let(:pattern_nodes) { patterns.map { |p| parse(p) } }
    subject { Semantic::PatternMatcher.generate_case_patterns(pattern_nodes, parsed_type).map(&:bindings).reduce(&:merge) }

    context 'with tuple | Nil type' do
      let(:patterns) { [
        '(_, _)',
        'a'
      ] }
      let(:type) { '(Int, Int) | Nil' }

      its([:a]) { is_expected.to eq Parser.debug_type('Nil') }
    end

    context 'with literal tuple | Nil type' do
      let(:patterns) { [
        '(0, 0)',
        'a'
      ] }
      let(:type) { '(Int, Int) | Nil' }

      its([:a]) { is_expected.to eq Parser.debug_type('(Int, Int) | Nil') }
    end

    context 'with inner pattern type elimination' do
      let(:patterns) { [
        '(nil, 0)',
        '(a, 0)',
        '(nil, _)',
        'b'
      ] }
      let(:type) { '(Int | Nil, Int)' }

      its([:a]) { is_expected.to eq Parser.debug_type('Int') }
      its([:b]) { is_expected.to eq Parser.debug_type('(Int, Int)') }
    end

    context 'with wilcards in nested records and tuples' do
      let(:patterns) { [
        '{ a: (0, { b: w }) }',
        '{ a: (1, x) }',
        '{ a: (0, y) }',
        '{ a: (_, nil) }',
        '{ a: z }'
      ] }
      let(:type) { '{ a: (Int, { b: String } | Nil) }' }

      its([:w]) { is_expected.to eq Parser.debug_type('String') }
      its([:x]) { is_expected.to eq Parser.debug_type('{ b: String } | Nil') }
      its([:y]) { is_expected.to eq Parser.debug_type('Nil') }
      its([:z]) { is_expected.to eq Parser.debug_type('(Int, { b: String })') }
    end
  end
end

def parse(source)
  Parser.new(source).parse.children.first
end
