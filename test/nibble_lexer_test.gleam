// import gleeunit/should
// import nibble/lexer.{NoMatchFound, Span, Token}
// import gleam/set

// type Tok {
//   I(Int)
//   F(Float)
//   S(String)
//   Id(String)
//   If
//   Then
//   Else
// }

// pub fn int_lexer_test() {
//   let input = "123"
//   let expected = Ok([Token(Span(1, 1, 1, 4), "123", I(123))])

//   lexer.simple([lexer.int(I)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)

//   let input = "000"
//   let expected = Ok([Token(Span(1, 1, 1, 4), "000", I(0))])

//   lexer.simple([lexer.int(I)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)
// }

// pub fn float_lexer_test() {
//   let input = "123"
//   let expected = Error(NoMatchFound(1, 1, "123"))

//   lexer.simple([lexer.float(F)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)

//   let input = "123.456"
//   let expected = Ok([Token(Span(1, 1, 1, 8), "123.456", F(123.456))])

//   lexer.simple([lexer.float(F)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)

//   let input = "000.000"
//   let expected = Ok([Token(Span(1, 1, 1, 8), "000.000", F(0.0))])

//   lexer.simple([lexer.float(F)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)
// }

// pub fn string_lexer_test() {
//   let input = "'hello'"
//   let expected = Ok([Token(Span(1, 1, 1, 8), "'hello'", S("hello"))])

//   lexer.simple([lexer.string("'", S)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)

//   let input = "'And Hayleigh said, \\'wow, this is a great string\\''"
//   let expected =
//     Ok([
//       Token(
//         Span(1, 1, 1, 53),
//         "'And Hayleigh said, \\'wow, this is a great string\\''",
//         S("And Hayleigh said, \\'wow, this is a great string\\'"),
//       ),
//     ])

//   lexer.simple([lexer.string("'", S)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)
// }

// pub fn identifier_lexer_test() {
//   let reserved = set.from_list(["let", "if", "else"])

//   let input = "let"
//   let expected = Error(NoMatchFound(1, 1, "let"))

//   lexer.simple([lexer.identifier("[a-z]", "[a-zA-Z0-9_]", reserved, Id)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)

//   let input = "letter"
//   let expected = Ok([Token(Span(1, 1, 1, 7), "letter", Id("letter"))])

//   lexer.simple([lexer.identifier("[a-z]", "[a-zA-Z0-9_]", reserved, Id)])
//   |> lexer.run(input, _)
//   |> should.equal(expected)
// }

// pub fn full_test() {
//   let reserved = set.from_list(["if", "then", "else"])

//   let input = "if wibble then w0BBL3 else 123.456"
//   let expected =
//     Ok([
//       Token(Span(1, 1, 1, 3), "if", If),
//       Token(Span(1, 4, 1, 10), "wibble", Id("wibble")),
//       Token(Span(1, 11, 1, 15), "then", Then),
//       Token(Span(1, 16, 1, 22), "w0BBL3", Id("w0BBL3")),
//       Token(Span(1, 23, 1, 27), "else", Else),
//       Token(Span(1, 28, 1, 35), "123.456", F(123.456)),
//     ])

//   lexer.simple([
//     lexer.ignore(lexer.whitespace(Nil)),
//     lexer.token("if", If),
//     lexer.token("then", Then),
//     lexer.token("else", Else),
//     lexer.identifier("[a-z]", "[a-zA-Z0-9_]", reserved, Id),
//     lexer.float(F),
//   ])
//   |> lexer.run(input, _)
//   |> should.equal(expected)
// }
