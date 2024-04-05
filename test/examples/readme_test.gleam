// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/option.{None, Some}
import gleeunit/should
import nibble.{do, return}
import nibble/lexer

// TYPES -----------------------------------------------------------------------

type Point {
  Point(x: Int, y: Int)
}

type T {
  Num(Int)
  LParen
  RParen
  Comma
}

// TESTS -----------------------------------------------------------------------

pub fn readme_test() {
  use run <- should("parse the example in README.md")
  let input = "(1, 2)"
  let expected = Point(1, 2)

  run(input, expected)
}

// UTILS -----------------------------------------------------------------------

fn should(description: String, run: fn(fn(String, Point) -> Nil) -> Nil) -> Nil {
  use input, expected <- run

  io.print("should " <> description)

  lexer.run(input, lexer())
  |> should.be_ok
  |> nibble.run(parser())
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}

fn lexer() {
  lexer.simple([
    lexer.int(Num),
    lexer.token("(", LParen),
    lexer.token(")", RParen),
    lexer.token(",", Comma),
    // Skip over whitespace, we don't care about it!
    lexer.whitespace(Nil)
    |> lexer.ignore,
  ])
}

fn parser() {
  use _ <- do(nibble.token(LParen))
  use x <- do(int_parser())
  use _ <- do(nibble.token(Comma))
  use y <- do(int_parser())
  use _ <- do(nibble.token(RParen))

  return(Point(x, y))
}

fn int_parser() {
  use tok <- nibble.take_map("a `Num` token")

  case tok {
    Num(n) -> Some(n)
    _ -> None
  }
}
