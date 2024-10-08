// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/set
import gleam/string
import gleeunit/should
import nibble/lexer.{type Matcher, type Token, Span, Token}

// TYPES -----------------------------------------------------------------------

type TokenT {
  Str(String)
  Int(Int)
  Num(Float)
  Kwd(String)
  Var(String)
}

// INTEGER TESTS ---------------------------------------------------------------

pub fn single_digit_integer_test() {
  use run <- should("lex a sigle digit integer")

  let input = "1"
  let expected = [Token(Span(1, 1, 1, 2), "1", Int(1))]

  run(input, expected, [lexer.int(Int)])
}

pub fn single_negative_digit_integer_test() {
  use run <- should("lex a single digit negative integer")

  let input = "-1"
  let expected = [Token(Span(1, 1, 1, 3), "-1", Int(-1))]

  run(input, expected, [lexer.int(Int)])
}

pub fn multi_digit_integer_test() {
  use run <- should("lex a multi digit integer")

  let input = "123"
  let expected = [Token(Span(1, 1, 1, 4), "123", Int(123))]

  run(input, expected, [lexer.int(Int)])
}

pub fn multi_digit_negative_integer_test() {
  use run <- should("lex a multi digit negative integer")

  let input = "-123"
  let expected = [Token(Span(1, 1, 1, 5), "-123", Int(-123))]

  run(input, expected, [lexer.int(Int)])
}

// FLOAT TESTS -----------------------------------------------------------------

pub fn single_digit_float_test() {
  use run <- should("lex a single digit float")

  let input = "1.0"
  let expected = [Token(Span(1, 1, 1, 4), "1.0", Num(1.0))]

  run(input, expected, [lexer.float(Num)])
}

pub fn single_nagive_digit_float_test() {
  use run <- should("lex a single digit negative float")

  let input = "-1.0"
  let expected = [Token(Span(1, 1, 1, 5), "-1.0", Num(-1.0))]

  run(input, expected, [lexer.float(Num)])
}

pub fn multi_digit_float_test() {
  use run <- should("lex a multi digit float")

  let input = "123.456"
  let expected = [Token(Span(1, 1, 1, 8), "123.456", Num(123.456))]

  run(input, expected, [lexer.float(Num)])
}

pub fn multi_digit_negative_float_test() {
  use run <- should("lex a multi digit negative float")

  let input = "-123.456"
  let expected = [Token(Span(1, 1, 1, 9), "-123.456", Num(-123.456))]

  run(input, expected, [lexer.float(Num)])
}

// STRING TESTS ----------------------------------------------------------------

pub fn empty_string_test() {
  use run <- should("lex an empty string")

  let input = "''"
  let expected = [Token(Span(1, 1, 1, 3), "''", Str(""))]

  run(input, expected, [lexer.string("'", Str)])
}

pub fn single_char_string_test() {
  use run <- should("lex a single char string")

  let input = "'a'"
  let expected = [Token(Span(1, 1, 1, 4), "'a'", Str("a"))]

  run(input, expected, [lexer.string("'", Str)])
}

pub fn multi_char_string_test() {
  use run <- should("lex a multi char string")

  let input = "'abc'"
  let expected = [Token(Span(1, 1, 1, 6), "'abc'", Str("abc"))]

  run(input, expected, [lexer.string("'", Str)])
}

pub fn escaped_string_test() {
  use run <- should("lex an escaped string")

  let input = "'\\''"
  let expected = [Token(Span(1, 1, 1, 5), "'\\''", Str("\\'"))]

  run(input, expected, [lexer.string("'", Str)])
}

pub fn multiline_string_test() {
  use run <- should("lex a multi-line string")

  let input = string.join(["'this is a", "multi-line string'"], "\n")
  let expected = [
    Token(Span(1, 1, 2, 19), input, Str("this is a\nmulti-line string")),
  ]

  run(input, expected, [lexer.string("'", Str)])
}

// KEYWORD TESTS ---------------------------------------------------------------

pub fn keyword_test() {
  use run <- should("lex a keyword")

  let input = "in"
  let expected = [Token(Span(1, 1, 1, 3), "in", Kwd("in"))]

  run(input, expected, [lexer.keyword("in", "\\s", Kwd("in"))])
}

pub fn keyword_breaker_test() {
  use run <- should("lex a keyword in an identifier")

  let input = "as assert"
  let expected = [
    Token(Span(1, 1, 1, 3), "as", Kwd("as")),
    Token(Span(1, 4, 1, 10), "assert", Var("assert")),
  ]

  run(input, expected, [
    lexer.keyword("as", "\\s", Kwd("as")),
    lexer.keyword("assert", "\\s", Var("assert")),
    lexer.token(" ", Nil)
      |> lexer.ignore(),
  ])
}

// VARIABLE TESTS --------------------------------------------------------------

pub fn single_char_variable_test() {
  use run <- should("lex a single char variable")

  let input = "a"
  let expected = [Token(Span(1, 1, 1, 2), "a", Var("a"))]

  run(input, expected, [lexer.variable(set.new(), Var)])
}

pub fn multi_char_variable_test() {
  use run <- should("lex a multi char variable")

  let input = "abc"
  let expected = [Token(Span(1, 1, 1, 4), "abc", Var("abc"))]

  run(input, expected, [lexer.variable(set.new(), Var)])
}

pub fn multi_char_variable_with_numbers_and_underscores_test() {
  use run <- should("lex a multi char variable with numbers and underscores")

  let input = "abc_123"
  let expected = [Token(Span(1, 1, 1, 8), "abc_123", Var("abc_123"))]

  run(input, expected, [lexer.variable(set.new(), Var)])
}

pub fn variable_containing_keyword_test() {
  use run <- should("lex a variable containing a keyword")

  let input = "insert"
  let expected = [Token(Span(1, 1, 1, 7), "insert", Var("insert"))]

  run(input, expected, [
    lexer.keyword("in", "\\s", Kwd("in")),
    lexer.variable(set.from_list(["int"]), Var),
  ])
}

// UTILS -----------------------------------------------------------------------

fn should(
  description: String,
  run: fn(fn(String, List(Token(a)), List(Matcher(a, Nil))) -> Nil) -> Nil,
) -> Nil {
  use input, expected, matchers <- run

  io.print("should " <> description)

  lexer.simple(matchers)
  |> lexer.run(input, _)
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}
