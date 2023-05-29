// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int
import gleam/list
import gleam/regex
import gleam/result
import gleam/set.{Set}
import gleam/string

// TYPES -----------------------------------------------------------------------

///
///
pub opaque type Matcher(a, mode) {
  Matcher(run: fn(mode, String, String) -> Match(a, mode))
}

pub type Match(a, mode) {
  Keep(a, mode)
  Skip
  Drop(mode)
  NoMatch
}

///
///
pub type Token(a) {
  Token(span: Span, lexeme: String, value: a)
}

///
///
pub type Span {
  Span(row_start: Int, col_start: Int, row_end: Int, col_end: Int)
}

///
///
pub type Error {
  NoMatchFound(row: Int, col: Int, lexeme: String)
}

///
///
pub opaque type Lexer(a, mode) {
  Lexer(matchers: fn(mode) -> List(Matcher(a, mode)))
}

type State(a) {
  State(
    source: List(String),
    tokens: List(Token(a)),
    current: #(Int, Int, String),
    row: Int,
    col: Int,
  )
}

// LEXER CONSTRUCTORS ----------------------------------------------------------

///
///
pub fn simple(matchers: List(Matcher(a, Nil))) -> Lexer(a, Nil) {
  Lexer(fn(_) { matchers })
}

///
///
pub fn custom(matchers: fn(mode) -> List(Matcher(a, mode))) -> Lexer(a, mode) {
  Lexer(fn(mode) { matchers(mode) })
}

// MATCHER CONSTRUCTORS --------------------------------------------------------

///
///
pub fn keep(f: fn(String, String) -> Result(a, Nil)) -> Matcher(a, mode) {
  use mode, lexeme, lookahead <- Matcher

  f(lexeme, lookahead)
  |> result.map(Keep(_, mode))
  |> result.unwrap(NoMatch)
}

///
///
pub fn drop(f: fn(String, String) -> Bool) -> Matcher(a, mode) {
  use mode, lexeme, lookahead <- Matcher

  case f(lexeme, lookahead) {
    True -> Drop(mode)
    False -> NoMatch
  }
}

///
///
pub fn skip(matcher: Matcher(a, mode)) -> Matcher(b, mode) {
  use mode, lexeme, lookahead <- Matcher

  case matcher.run(mode, lexeme, lookahead) {
    Keep(_, _) -> Skip
    Skip -> Skip
    Drop(_) -> Skip
    NoMatch -> NoMatch
  }
}

///
///
pub fn map(matcher: Matcher(a, mode), f: fn(a) -> b) -> Matcher(b, mode) {
  use mode, lexeme, lookahead <- Matcher

  case matcher.run(mode, lexeme, lookahead) {
    Keep(value, mode) -> Keep(f(value), mode)
    Skip -> Skip
    Drop(mode) -> Drop(mode)
    NoMatch -> NoMatch
  }
}

///
///
pub fn custom_matcher(
  f: fn(mode, String, String) -> Match(a, mode),
) -> Matcher(a, mode) {
  Matcher(f)
}

///
///
pub fn transition(
  matcher: Matcher(a, mode),
  f: fn(mode) -> mode,
) -> Matcher(a, mode) {
  use mode, lexeme, lookahead <- Matcher

  case matcher.run(mode, lexeme, lookahead) {
    Keep(value, mode) -> Keep(value, f(mode))
    Skip -> Skip
    Drop(mode) -> Drop(f(mode))
    NoMatch -> NoMatch
  }
}

///
///
pub fn ignore(matcher: Matcher(a, mode)) -> Matcher(b, mode) {
  use mode, lexeme, lookahead <- Matcher

  case matcher.run(mode, lexeme, lookahead) {
    Keep(_, mode) -> Drop(mode)
    Skip -> Skip
    Drop(mode) -> Drop(mode)
    NoMatch -> NoMatch
  }
}

// COMMON MATCHERS -------------------------------------------------------------

///
///
pub fn token(str: String, value: a) -> Matcher(a, mode) {
  use mode, lexeme, _ <- Matcher

  case lexeme == str {
    True -> Keep(value, mode)
    False -> NoMatch
  }
}

///
///
pub fn symbol(str: String, breaker: String, value: a) -> Matcher(a, mode) {
  let assert Ok(break) = regex.from_string(breaker)

  use mode, lexeme, lookahead <- Matcher

  case lexeme == str && { lookahead == "" || regex.check(break, lookahead) } {
    True -> Keep(value, mode)
    False -> NoMatch
  }
}

///
///
pub fn keyword(str: String, breaker: String, value: a) -> Matcher(a, mode) {
  let assert Ok(break) = regex.from_string(breaker)

  use mode, lexeme, lookahead <- Matcher

  case lexeme == str && { lookahead == "" || regex.check(break, lookahead) } {
    True -> Keep(value, mode)
    False -> NoMatch
  }
}

///
///
pub fn int(to_value: fn(Int) -> a) -> Matcher(a, mode) {
  int_with_separator("", to_value)
}

///
///
pub fn int_with_separator(
  separator: String,
  to_value: fn(Int) -> a,
) -> Matcher(a, mode) {
  let assert Ok(digit) = regex.from_string("[0-9" <> separator <> "]")
  let assert Ok(integer) = regex.from_string("^-*[0-9" <> separator <> "]+$")

  use mode, lexeme, lookahead <- Matcher

  case !regex.check(digit, lookahead) && regex.check(integer, lexeme) {
    False -> NoMatch
    True -> {
      let assert Ok(num) =
        lexeme
        |> string.replace(separator, "")
        |> int.parse
      Keep(to_value(num), mode)
    }
  }
}

///
///
pub fn float(to_value: fn(Float) -> a) -> Matcher(a, mode) {
  float_with_separator("", to_value)
}

///
///
pub fn float_with_separator(
  separator: String,
  to_value: fn(Float) -> a,
) -> Matcher(a, mode) {
  let assert Ok(digit) = regex.from_string("[0-9" <> separator <> "]")
  let assert Ok(integer) = regex.from_string("^-*[0-9" <> separator <> "]+$")
  let assert Ok(number) =
    regex.from_string(
      "^-*[0-9" <> separator <> "]+\\.[0-9" <> separator <> "]+$",
    )

  use mode, lexeme, lookahead <- Matcher
  let is_int = !regex.check(digit, lookahead) && regex.check(integer, lexeme)
  let is_float = !regex.check(digit, lookahead) && regex.check(number, lexeme)

  case lexeme {
    "." if is_int -> NoMatch

    _ if is_float -> {
      let assert Ok(num) =
        lexeme
        |> string.replace(separator, "")
        |> float.parse
      Keep(to_value(num), mode)
    }

    _ -> NoMatch
  }
}

pub fn number(
  from_int: fn(Int) -> a,
  from_float: fn(Float) -> a,
) -> Matcher(a, mode) {
  number_with_separator("", from_int, from_float)
}

pub fn number_with_separator(
  separator: String,
  from_int: fn(Int) -> a,
  from_float: fn(Float) -> a,
) -> Matcher(a, mode) {
  let assert Ok(digit) = regex.from_string("[0-9" <> separator <> "]")
  let assert Ok(integer) = regex.from_string("^-*[0-9" <> separator <> "]+$")
  let assert Ok(number) =
    regex.from_string(
      "^-*[0-9" <> separator <> "]+\\.[0-9" <> separator <> "]+$",
    )

  use mode, lexeme, lookahead <- Matcher
  let is_int = !regex.check(digit, lookahead) && regex.check(integer, lexeme)
  let is_float = !regex.check(digit, lookahead) && regex.check(number, lexeme)

  case lexeme, lookahead {
    ".", _ if is_int -> NoMatch
    _, "." if is_int -> NoMatch

    _, _ if is_int -> {
      let assert Ok(num) =
        lexeme
        |> string.replace(separator, "")
        |> int.parse
      Keep(from_int(num), mode)
    }

    _, _ if is_float -> {
      let assert Ok(num) =
        lexeme
        |> string.replace(separator, "")
        |> float.parse
      Keep(from_float(num), mode)
    }

    _, _ -> NoMatch
  }
}

///
///
pub fn string(char: String, to_value: fn(String) -> a) -> Matcher(a, mode) {
  let assert Ok(is_string) =
    regex.from_string(
      "^" <> char <> "([^" <> char <> "\\\\]|\\\\[\\s\\S])*" <> char <> "$",
    )
  use mode, lexeme, _ <- Matcher

  case regex.check(is_string, lexeme) {
    True ->
      lexeme
      |> string.drop_left(1)
      |> string.drop_right(1)
      |> to_value
      |> Keep(mode)
    False -> NoMatch
  }
}

///
///
pub fn identifier(
  start: String,
  inner: String,
  reserved: Set(String),
  to_value: fn(String) -> a,
) -> Matcher(a, mode) {
  let assert Ok(ident) = regex.from_string("^" <> start <> inner <> "*$")
  let assert Ok(inner) = regex.from_string(inner)

  use mode, lexeme, lookahead <- Matcher

  case regex.check(inner, lookahead), regex.check(ident, lexeme) {
    True, True -> Skip
    False, True ->
      case set.contains(reserved, lexeme) {
        True -> NoMatch
        False -> Keep(to_value(lexeme), mode)
      }
    _, _ -> NoMatch
  }
}

///
///
pub fn try_identifier(
  start: String,
  inner: String,
  reserved: Set(String),
  to_value: fn(String) -> a,
) -> Result(Matcher(a, mode), regex.CompileError) {
  use ident <- result.then(regex.from_string("^" <> start <> inner <> "*$"))
  use inner <- result.map(regex.from_string(inner))

  use mode, lexeme, lookahead <- Matcher

  case regex.check(inner, lookahead), regex.check(ident, lexeme) {
    True, True -> Skip
    False, True ->
      case set.contains(reserved, lexeme) {
        True -> NoMatch
        False -> Keep(to_value(lexeme), mode)
      }
    _, _ -> NoMatch
  }
}

///
///
pub fn variable(
  reserved: Set(String),
  to_value: fn(String) -> a,
) -> Matcher(a, mode) {
  identifier("[a-z]", "[a-zA-Z0-9_]", reserved, to_value)
}

///
///
pub fn whitespace(token: a) -> Matcher(a, mode) {
  let assert Ok(whitespace) = regex.from_string("^\\s+$")

  use mode, lexeme, _ <- Matcher

  case regex.check(whitespace, lexeme) {
    True -> Keep(token, mode)
    False -> NoMatch
  }
}

// RUNNING A LEXER -------------------------------------------------------------

///
///
pub fn run(
  source: String,
  lexer: Lexer(a, Nil),
) -> Result(List(Token(a)), Error) {
  string.to_graphemes(source)
  |> State([], #(1, 1, ""), 1, 1)
  |> do_run(lexer, Nil, _)
}

///
///
pub fn run_custom(
  source: String,
  mode: mode,
  lexer: Lexer(a, mode),
) -> Result(List(Token(a)), Error) {
  do_run(lexer, mode, State(string.to_graphemes(source), [], #(1, 1, ""), 1, 1))
}

fn do_run(
  lexer: Lexer(a, mode),
  mode: mode,
  state: State(a),
) -> Result(List(Token(a)), Error) {
  let matchers = lexer.matchers(mode)

  case state.source, state.current {
    // If we're at the end of the source and there's no lexeme left to match, 
    // we're done!
    //
    // We have to remember to reverse the list of tokens because we've been building
    // it backwards using `[token, ..state.tokens]`. This is much quicker than
    // trying to prepend to the list as we go.
    [], #(_, _, "") -> Ok(list.reverse(state.tokens))

    // If we're at the end of the source but there's still a lexeme left to match,
    // we'll run the final `do_match` and return the result. If we get a `NoMatch`
    // at this point something went wrong.
    [], #(start_row, start_col, lexeme) ->
      case do_match(mode, lexeme, "", matchers) {
        NoMatch -> Error(NoMatchFound(start_row, start_col, lexeme))
        Skip -> Error(NoMatchFound(start_row, start_col, lexeme))
        Drop(_) -> Ok(list.reverse(state.tokens))
        Keep(value, _) -> {
          let span = Span(start_row, start_col, state.row, state.col)
          let token = Token(span, lexeme, value)

          Ok(list.reverse([token, ..state.tokens]))
        }
      }

    // When lexing we include a one-grapheme lookahead to help us with things like
    // matching identifiers or other mode-aware tokens. This just takes the 
    // skip grapheme from the source (we call it `lookahead` here) and calls the
    // `do_match` function with it and some other bits.
    [lookahead, ..rest], #(start_row, start_col, lexeme) -> {
      let row = next_row(state.row, lookahead)
      let col = next_col(state.col, lookahead)

      case do_match(mode, lexeme, lookahead, matchers) {
        Keep(value, mode) -> {
          let span = Span(start_row, start_col, state.row, state.col)
          let token = Token(span, lexeme, value)

          do_run(
            lexer,
            mode,
            State(
              source: rest,
              tokens: [token, ..state.tokens],
              current: #(state.row, state.col, lookahead),
              row: row,
              col: col,
            ),
          )
        }

        // A skip says that a matcher has matched the lexeme but still wants to
        // consume more input. This is mostly useful for things like identifiers
        // where the current lexeme is in the set of reserved words but we can
        // see the lookahead and know that it's not a reserved word.
        Skip ->
          do_run(
            lexer,
            mode,
            State(
              source: rest,
              tokens: state.tokens,
              current: #(start_row, start_col, lexeme <> lookahead),
              row: row,
              col: col,
            ),
          )

        // A drop says that we've matched the lexeme but we don't want to emit a
        // token. This is mostly useful for things like comments or whitespace that
        // users may not want to appear in the final token stream but do want to
        // handle in the lexer.
        Drop(mode) ->
          do_run(
            lexer,
            mode,
            State(
              source: rest,
              tokens: state.tokens,
              current: #(state.row, state.col, lookahead),
              row: row,
              col: col,
            ),
          )

        NoMatch ->
          do_run(
            lexer,
            mode,
            State(
              source: rest,
              tokens: state.tokens,
              current: #(start_row, start_col, lexeme <> lookahead),
              row: row,
              col: col,
            ),
          )
      }
    }
  }
}

fn do_match(
  mode: mode,
  str: String,
  lookahead: String,
  matchers: List(Matcher(a, mode)),
) -> Match(a, mode) {
  use _, matcher <- list.fold_until(matchers, NoMatch)

  case matcher.run(mode, str, lookahead) {
    Keep(_, _) as match -> list.Stop(match)
    Skip -> list.Stop(Skip)
    Drop(_) as match -> list.Stop(match)
    NoMatch -> list.Continue(NoMatch)
  }
}

// UTILS -----------------------------------------------------------------------

fn next_col(col: Int, str: String) -> Int {
  case str {
    "\n" -> 1
    _ -> col + 1
  }
}

fn next_row(row: Int, str: String) -> Int {
  case str {
    "\n" -> row + 1
    _ -> row
  }
}
