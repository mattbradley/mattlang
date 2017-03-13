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
      ]}
      let(:candidate) { '(0, nil, 0, nil, 0, nil, 0, nil)' }
      let(:type) { '(Int | Nil, Int | Nil, Int | Nil, Int | Nil, Int | Nil, Int | Nil, Int | Nil, Int | Nil)' }

      it { is_expected.to eq true }
    end
  end

  context 'checking exhaustiveness' do
    let(:parsed_type) { Parser.debug_type(type) }
    let(:matrix) { patterns.map { |p| [Semantic::PatternMatcher.build_pattern(parse(p), parsed_type)] } }
    let(:debugger) { false }
    subject { Semantic::PatternMatcher.exhaustive?(matrix, [parsed_type], debugger: debugger) }

    context 'with a simple literal' do
      let(:patterns) { ['0'] }
      let(:type) { 'Int' }

      it { is_expected.to_not be_empty }
    end
  end
end

def parse(source)
  Parser.new(source).parse.children.first
end
