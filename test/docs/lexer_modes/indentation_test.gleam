// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/order.{Eq, Gt, Lt}
import gleam/regex
import gleam/set
import gleam/string
import gleam/io
import gleeunit/should
import nibble/lexer.{
  type Matcher, type Token, Drop, Keep, NoMatch, Skip, Span, Token,
}

// TYPES -----------------------------------------------------------------------

type TokenT {
  Var(String)
  Str(String)
  Num(Int)

  // Keywords
  Def
  For
  In
  Print

  // Indentation
  Indent(Int)
  Dedent(Int)
}

// TESTS -----------------------------------------------------------------------

pub fn indentation_test() {
  use test <- should("parse indent and dedent tokens")
  let input =
    "def wibble arr
  for x in arr
    print x

  print \"done!\"

def wobble
  wibble numbers
"
  let expected = [
    Token(Span(1, 1, 1, 4), "def", Var("def")),
    Token(Span(1, 5, 1, 11), "wibble", Var("wibble")),
    Token(Span(1, 12, 1, 15), "arr", Var("arr")),
    Token(Span(1, 15, 2, 3), "\n  ", Indent(2)),
    Token(Span(2, 3, 2, 6), "for", Var("for")),
    Token(Span(2, 7, 2, 8), "x", Var("x")),
    Token(Span(2, 9, 2, 11), "in", Var("in")),
    Token(Span(2, 12, 2, 15), "arr", Var("arr")),
    Token(Span(2, 15, 3, 5), "\n    ", Indent(4)),
    Token(Span(3, 5, 3, 10), "print", Var("print")),
    Token(Span(3, 11, 3, 12), "x", Var("x")),
    Token(Span(4, 1, 5, 3), "\n  ", Dedent(2)),
    Token(Span(5, 3, 5, 8), "print", Var("print")),
    Token(Span(5, 9, 5, 16), "\"done!\"", Str("done!")),
    Token(Span(6, 1, 7, 1), "\n", Dedent(0)),
    Token(Span(7, 1, 7, 4), "def", Var("def")),
    Token(Span(7, 5, 7, 11), "wobble", Var("wobble")),
    Token(Span(7, 11, 8, 3), "\n  ", Indent(2)),
    Token(Span(8, 3, 8, 9), "wibble", Var("wibble")),
    Token(Span(8, 10, 8, 17), "numbers", Var("numbers")),
    Token(Span(8, 17, 9, 1), "\n", Dedent(0)),
  ]

  test(input, expected)
}

// UTILS -----------------------------------------------------------------------

fn should(
  description: String,
  run: fn(fn(String, List(Token(TokenT))) -> Nil) -> Nil,
) -> Nil {
  use input, expected <- run

  io.print("should " <> description)

  lexer.advanced(lexer)
  |> lexer.run_advanced(input, 0, _)
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}

fn lexer(_) -> List(Matcher(TokenT, Int)) {
  let assert Ok(is_indent) = regex.from_string("^\\n[ \\t]*")
  let indentation = {
    use current_indent, lexeme, lookahead <- lexer.custom

    case regex.check(is_indent, lexeme), lookahead {
      False, _ -> NoMatch
      True, " " | True, "\t" -> Skip
      True, "\n" -> Drop(current_indent)
      True, _ -> {
        let spaces = string.length(lexeme) - 1

        case int.compare(spaces, current_indent) {
          Lt -> Keep(Dedent(spaces), spaces)
          Eq if spaces == 0 -> Drop(0)
          Eq -> Keep(Indent(spaces), spaces)
          Gt -> Keep(Indent(spaces), spaces)
        }
      }
    }
  }

  [
    lexer.variable(set.new(), Var),
    lexer.string("\"", Str),
    lexer.int(Num),
    // Keywords
    lexer.keyword("def", "[\\W\\D]", Def),
    lexer.keyword("for", "[\\W\\D]", For),
    lexer.keyword("in", "[\\W\\D]", In),
    lexer.keyword("print", "[\\W\\D]", Print),
    // Our custom indentation lexer
    indentation,
    // Ignore all other whitespace
    lexer.whitespace(Nil)
    |> lexer.ignore,
  ]
}
