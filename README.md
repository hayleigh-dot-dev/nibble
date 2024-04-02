# nibble

A lexer and parser combinator library inspired by [`elm/parser`](https://github.com/elm/parser).

[![Package Version](https://img.shields.io/hexpm/v/nibble)](https://hex.pm/packages/nibble)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/nibble/)

✨ This project is written in **pure Gleam** so you can use it anywhere Gleam
runs: Erlang, Elixir, Node, Deno, and the browser!

## Quick start

If you just want to get a feel for what nibble can do, check out the example
below.

```gleam
import gleam/option.{Some, None}
import nibble.{do, return}
import nibble/lexer

type Point {
  Point(x: Int, y: Int)
}

type Token {
  Num(Int)
  LParen
  RParen
  Comma
}

pub fn main() {
  // Your lexer knows how to take an input string and
  // turn it into a flat list of tokens. You define the
  // type of token you want to use, but nibble will wrap
  // that up in its own `Token` type that includes the
  // source span and original lexeme for each token.
  let lexer = lexer.simple([
    lexer.int(Num),
    lexer.token("(", LParen),
    lexer.token(")", RParen),
    lexer.token(",", Comma),
    // Skip over whitespace, we don't care about it!
    lexer.whitespace(Nil)
    |> lexer.ignore
  ])

  // Your parser(s!) know how to transform a list of
  // tokens into whatever you want. You have the full
  // power of Gleam here, so you can go wild!
  let parser = {
    use _ <- do(nibble.token(LParen))
    use x <- do(int_parser())
    use _ <- do(nibble.token(Comma))
    use y <- do(int_parser())
    use _ <- do(nibble.token(RParen))

    return(Point(x, y))
  }

  let int_parser = {
    // Use `take_map` to only consume certain kinds of tokens and transform the
    // result.
    use tok <- nibble.take_map

    case tok {
      Num(n) -> Some(n)
      _ -> None
    }
  }

  let assert Ok(tokens) = lexer.run("(1, 2)", lexer)
  let assert Ok(point) = nibble.run(tokens, parser)

  point.x //=> 1
  point.y //=> 2
}
```

## Installation

If available on Hex this package can be added to your Gleam project:

```sh
gleam add nibble
```

and its documentation can be found at <https://hexdocs.pm/nibble>.
