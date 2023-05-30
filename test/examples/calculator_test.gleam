// IMPORTS ---------------------------------------------------------------------

import gleam/function
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleeunit/should
import nibble.{do, do_in, return}
import nibble/pratt
import nibble/lexer

// TYPES -----------------------------------------------------------------------

type TokenT {
  Add
  Sub
  Mul
  Div
  Num(Float)
  LParen
  RParen
}

type Context {
  InSubExpr
}

type DeadEnd =
  nibble.DeadEnd(TokenT, Context)

// TESTS -----------------------------------------------------------------------

pub fn add_test() {
  use test <- should("add two numbers")
  let input = "1 + 2"
  let expected = 3.0

  test(input, expected)
}

pub fn multi_add_test() {
  use test <- should("add multiple numbers")
  let input = "1 + 2 + 3"
  let expected = 6.0

  test(input, expected)
}

pub fn sub_test() {
  use test <- should("subtract two numbers")
  let input = "3 - 2"
  let expected = 1.0

  test(input, expected)
}

pub fn multi_sub_test() {
  use test <- should("subtract multiple numbers")
  let input = "3 - 2 - 1"
  let expected = 0.0

  test(input, expected)
}

pub fn mul_test() {
  use test <- should("multiply two numbers")
  let input = "2 * 3"
  let expected = 6.0

  test(input, expected)
}

pub fn multi_mul_test() {
  use test <- should("multiply multiple numbers")
  let input = "2 * 3 * 4"
  let expected = 24.0

  test(input, expected)
}

pub fn precedence_test() {
  use test <- should("evaluate operators according to precedence")
  let input = "2 * 3 + 4"
  let expected = 10.0

  test(input, expected)
}

pub fn parens_test() {
  use test <- should("evaluate parens first")
  let input = "2 * (3 + 4)"
  let expected = 14.0

  test(input, expected)
}

pub fn complex_test() {
  use test <- should("evaluate complex expressions")
  let input = "2 * (3 + 4) / 2 - 1"
  let expected = 6.0

  test(input, expected)
}

pub fn mismatched_parens_test() {
  use test <- should_error("on mismatched parens")
  let input = "2 * (3 + 4"
  let expected = [
    nibble.DeadEnd(
      lexer.Span(1, 10, 1, 11),
      nibble.EndOfInput,
      [nibble.Located(lexer.Span(1, 5, 1, 6), InSubExpr)],
    ),
  ]

  test(input, expected)
}

// UTILS -----------------------------------------------------------------------

fn should(description: String, run: fn(fn(String, Float) -> Nil) -> Nil) -> Nil {
  use input, expected <- run

  io.print("should " <> description)

  lexer.run(input, lexer())
  |> should.be_ok
  |> nibble.run(parser())
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}

fn should_error(
  description: String,
  run: fn(fn(String, List(DeadEnd)) -> Nil) -> Nil,
) -> Nil {
  use input, expected <- run

  io.print("should error " <> description)

  lexer.run(input, lexer())
  |> should.be_ok
  |> nibble.run(parser())
  |> should.equal(Error(expected))

  io.println(" ✅")
}

fn lexer() {
  lexer.simple([
    // Grouping
    lexer.token("(", LParen),
    lexer.token(")", RParen),
    // Operators
    lexer.token("+", Add),
    lexer.token("-", Sub),
    lexer.token("*", Mul),
    lexer.token("/", Div),
    // Numbers
    lexer.number(int.to_float, function.identity)
    |> lexer.map(Num),
    // Whitespace
    lexer.whitespace(Nil)
    |> lexer.ignore(),
  ])
}

fn parser() {
  let add = fn(x, y) { x +. y }
  let sub = fn(x, y) { x -. y }
  let mul = fn(x, y) { x *. y }
  let div = fn(x, y) { x /. y }

  pratt.expression(
    one_of: [parens_parser, number_parser],
    and_then: [
      pratt.infix_left(14, nibble.token(Mul), mul),
      pratt.infix_left(14, nibble.token(Div), div),
      pratt.infix_left(13, nibble.token(Add), add),
      pratt.infix_left(13, nibble.token(Sub), sub),
    ],
    // Whitespace is already ignored by the lexer so there's no tokens we need to
    // explicitly ignore here.
    dropping: return(Nil),
  )
}

fn number_parser(_) {
  use tok <- nibble.take_map("a number token")

  case tok {
    Num(n) -> Some(n)
    _ -> None
  }
}

fn parens_parser(_) {
  use _ <- do(nibble.token(LParen))
  use n <- do_in(InSubExpr, nibble.lazy(parser))
  use _ <- do(nibble.token(RParen))

  return(n)
}
