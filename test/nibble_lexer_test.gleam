import gleeunit/should
import nibble/lexer.{NoMatchFound, Span, Token}
import gleam/set

type Tok {
  I(Int)
  F(Float)
  Id(String)
}

pub fn int_lexer_test() {
  let input = "123"
  let expected = Ok([Token(Span(1, 1, 1, 4), "123", I(123))])

  lexer.simple([lexer.int(I)])
  |> lexer.run(input, _)
  |> should.equal(expected)

  let input = "000"
  let expected = Ok([Token(Span(1, 1, 1, 4), "000", I(0))])

  lexer.simple([lexer.int(I)])
  |> lexer.run(input, _)
  |> should.equal(expected)
}

pub fn float_lexer_test() {
  let input = "123"
  let expected = Error(NoMatchFound(1, 1, "123"))

  lexer.simple([lexer.float(F)])
  |> lexer.run(input, _)
  |> should.equal(expected)

  let input = "123.456"
  let expected = Ok([Token(Span(1, 1, 1, 8), "123.456", F(123.456))])

  lexer.simple([lexer.float(F)])
  |> lexer.run(input, _)
  |> should.equal(expected)

  let input = "000.000"
  let expected = Ok([Token(Span(1, 1, 1, 8), "000.000", F(0.0))])

  lexer.simple([lexer.float(F)])
  |> lexer.run(input, _)
  |> should.equal(expected)
}

pub fn identifier_lexer_test() {
  let reserved = set.from_list(["let", "if", "else"])

  let input = "let"
  let expected = Error(NoMatchFound(1, 1, "let"))

  lexer.simple([lexer.identifier("[a-z]", "[a-zA-Z0-9_]", reserved, Id)])
  |> lexer.run(input, _)
  |> should.equal(expected)

  let input = "letter"
  let expected = Ok([Token(Span(1, 1, 1, 7), "letter", Id("letter"))])

  lexer.simple([lexer.identifier("[a-z]", "[a-zA-Z0-9_]", reserved, Id)])
  |> lexer.run(input, _)
  |> should.equal(expected)
}
