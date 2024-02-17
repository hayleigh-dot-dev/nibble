// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleeunit/should
import nibble.{type Parser}
import nibble/lexer.{type Lexer, type Span, Span}

// TYPES -----------------------------------------------------------------------

type TokenT {
  LParenT
  RParenT
  NumT(Int)
}

type Expr {
  SExpr(body: List(Expr), location: Span)
  Num(value: Int, location: Span)
}

// TESTS -----------------------------------------------------------------------

pub fn basic_test() {
  use run <- should("parse a basic s-expresssion")
  let input = "(1 2)"
  let expected =
    SExpr(
      [Num(1, Span(1, 2, 1, 3)), Num(2, Span(1, 4, 1, 5))],
      Span(1, 1, 1, 6),
    )

  run(input, expected)
}

pub fn nested_test() {
  use run <- should("parse nested s-expressions")
  let input = "(1 ((2 3) 4) (5) 6)"
  let expected =
    SExpr(
      [
        Num(1, Span(1, 2, 1, 3)),
        SExpr(
          [
            SExpr(
              [Num(2, Span(1, 6, 1, 7)), Num(3, Span(1, 8, 1, 9))],
              Span(1, 5, 1, 10),
            ),
            Num(4, Span(1, 11, 1, 12)),
          ],
          Span(1, 4, 1, 13),
        ),
        SExpr([Num(5, Span(1, 15, 1, 16))], Span(1, 14, 1, 17)),
        Num(6, Span(1, 18, 1, 19)),
      ],
      Span(1, 1, 1, 20),
    )

  run(input, expected)
}

pub fn locations_test() {
  use run <- should("parse locations over multiple lines")
  let input =
    "(
      1

  2 3)
    "
  let expected =
    SExpr(
      [
        Num(1, Span(2, 7, 2, 8)),
        Num(2, Span(4, 3, 4, 4)),
        Num(3, Span(4, 5, 4, 6)),
      ],
      Span(1, 1, 4, 7),
    )

  run(input, expected)
}

// UTILS -----------------------------------------------------------------------

fn should(description: String, run: fn(fn(String, Expr) -> Nil) -> Nil) -> Nil {
  use input, expected <- run

  io.print("should " <> description)

  lexer.run(input, lexer())
  |> should.be_ok
  |> nibble.run(parser())
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}

fn lexer() -> Lexer(TokenT, Nil) {
  lexer.simple([
    lexer.token("(", LParenT),
    lexer.token(")", RParenT),
    lexer.int(NumT),
    lexer.whitespace(Nil)
      |> lexer.ignore,
  ])
}

fn parser() -> Parser(Expr, TokenT, Nil) {
  nibble.one_of([number_parser(), sexpr_parser()])
}

fn number_parser() -> Parser(Expr, TokenT, Nil) {
  use t <- nibble.do(nibble.any())
  // Get the position of the current token
  use pos <- nibble.do(nibble.span())

  case t {
    NumT(n) -> nibble.return(Num(n, pos))
    _ -> nibble.fail("expected a number literal")
  }
}

fn sexpr_parser() -> Parser(Expr, TokenT, Nil) {
  use _ <- nibble.do(nibble.token(LParenT))
  // Position of the left parenthesis
  use start <- nibble.do(nibble.span())
  use body <- nibble.do(nibble.many1(parser()))
  use _ <- nibble.do(nibble.token(RParenT))
  // Position of the right parenthesis
  use end <- nibble.do(nibble.span())

  // Create a combined location from the start and end spans
  //
  // (  1  2  3  )
  // ~ start     ~ end
  // ~~~~~~~~~~~~~ combined
  let combined = merge_spans(start, end)

  nibble.return(SExpr(body, combined))
}

fn merge_spans(span1: Span, span2: Span) -> Span {
  Span(span1.row_start, span1.col_start, span2.row_end, span2.col_end)
}
