// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/list
import gleeunit/should
import nibble.{Parser, do, return}
import nibble/lexer.{Token}

// STRESS TESTS ----------------------------------------------------------------

pub fn do_tco_test() {
  use test <- should("Not stack overflow with many chained `do` calls")
  let input = []
  let expected = 0
  let parser =
    list.repeat(return, 1_000_000)
    |> list.fold(return(0), do)

  test(input, expected, parser)
}

// UTILS -----------------------------------------------------------------------

fn should(
  description: String,
  run: fn(fn(List(Token(tok)), a, Parser(a, tok, ctx)) -> Nil) -> Nil,
) -> Nil {
  use input, expected, parser <- run

  io.print("should " <> description)

  nibble.run(input, parser)
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}
