// IMPORTS ---------------------------------------------------------------------

import gleam/function
import gleam/int
import gleam/float
import gleam/set
import gleam/io
import gleam/option.{None, Some}
import gleam/map.{Map}
import gleeunit/should
import nibble.{Break, Continue, do, return}
import nibble/lexer

// TYPES -----------------------------------------------------------------------

type Env =
  Map(String, String)

type TokenT {
  Key(String)
  Str(String)
  Equals
  NewLine
}

// TESTS -----------------------------------------------------------------------

pub fn empty_env_test() {
  use test <- should("parse an empty env")
  let input = ""
  let expected = map.new()

  test(input, expected)
}

pub fn single_env_test() {
  use test <- should("parse a single k/v pair")
  let input = "FOO=bar"
  let expected = map.from_list([#("FOO", "bar")])

  test(input, expected)
}

pub fn single_env_string_test() {
  use test <- should("parse a single k/v pair with a string value")
  let input = "FOO='bar'"
  let expected = map.from_list([#("FOO", "bar")])

  test(input, expected)
}

pub fn single_env_number_test() {
  use test <- should("parse a single k/v pair with a number value")
  let input = "FOO=123"
  let expected = map.from_list([#("FOO", "123")])

  test(input, expected)
}

pub fn single_env_float_test() {
  use test <- should("parse a single k/v pair with a float value")
  let input = "FOO=123.456"
  let expected = map.from_list([#("FOO", "123.456")])

  test(input, expected)
}

pub fn multi_env_test() {
  use test <- should("parse multiple k/v pairs")
  let input =
    "
    FOO=bar
    BAZ=qux
    "
  let expected = map.from_list([#("FOO", "bar"), #("BAZ", "qux")])

  test(input, expected)
}

// UTILS -----------------------------------------------------------------------

fn should(description: String, run: fn(fn(String, Env) -> Nil) -> Nil) -> Nil {
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
    lexer.token("=", Equals),
    lexer.token("\n", NewLine),
    // Strings values can use either double quotes or single quotes
    lexer.string("\"", Str),
    lexer.string("'", Str),
    // Keys can be any non-whitespace character
    lexer.identifier("[^\\s=#]", "[^\\s=]", set.new(), Key),
    // We'll allow number literals and just convert them to string values
    lexer.number(
      function.compose(int.to_string, Str),
      function.compose(float.to_string, Str),
    ),
    // Drop comments and whitespace
    lexer.comment("#", function.constant(Nil))
    |> lexer.ignore,
    lexer.spaces(Nil)
    |> lexer.ignore,
  ])
}

fn parser() {
  use env <- nibble.loop(map.new())

  nibble.one_of([
    key_value_parser(env)
    |> nibble.map(Continue),
    // The `key_value_parser` already consumes one new line. This parser makes
    // sure that if k/v pairs are separated by _multiple_ newlines that we still
    // consume them all.
    //
    // We use `many1` here because we need to consume at least _one_ token to 
    // prevent an infinite loop.
    //
    nibble.many1(nibble.token(NewLine))
    |> nibble.replace(Continue(env)),
    nibble.eof()
    |> nibble.replace(Break(env)),
  ])
}

fn key_value_parser(env) {
  use k <- do(key_parser())
  use _ <- do(nibble.token(Equals))
  use v <- do(val_parser())
  use _ <- do(nibble.one_of([nibble.token(NewLine), nibble.eof()]))

  return(map.insert(env, k, v))
}

fn key_parser() {
  use tok <- nibble.take_map("an env key")

  case tok {
    Key(k) -> Some(k)
    _ -> None
  }
}

fn val_parser() {
  use tok <- nibble.take_map("an env value")

  case tok {
    Str(v) -> Some(v)
    // We can treat a single unquoted word as a value
    Key(v) -> Some(v)
    _ -> None
  }
}
