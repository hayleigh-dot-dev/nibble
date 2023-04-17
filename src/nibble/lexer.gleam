// IMPORTS ---------------------------------------------------------------------
import gleam/list
import gleam/option.{None, Option, Some}
import gleam/regex
import gleam/set.{Set}
import gleam/string
import gleam/int
import gleam/float
import gleam/io

// TYPES -----------------------------------------------------------------------

///
///
pub type Span {
  Span(row_start: Int, col_start: Int, row_end: Int, col_end: Int)
}

///
///
pub type Token(a) {
  Token(span: Span, lexeme: String, value: a)
}

///
///
pub opaque type Matcher(a, ctx) {
  Matcher(run: fn(ctx, String, String) -> Match(a, ctx))
}

pub type Match(a, ctx) {
  Keep(a, ctx)
  Drop(ctx)
  NoMatch
}

///
///
pub type Error {
  NoMatchFound(row: Int, col: Int, lexeme: String)
}

///
///
pub opaque type Lexer(a, ctx) {
  Lexer(matchers: fn(ctx) -> List(Matcher(a, ctx)))
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
pub fn custom(matchers: fn(ctx) -> List(Matcher(a, ctx))) -> Lexer(a, ctx) {
  Lexer(fn(ctx) { matchers(ctx) })
}

// MATCHER CONSTRUCTORS --------------------------------------------------------

///
///
pub fn simple_matcher(
  f: fn(String, String) -> Result(Option(a), Nil),
) -> Matcher(a, ctx) {
  use ctx, lexeme, lookahead <- Matcher

  case f(lexeme, lookahead) {
    Ok(Some(value)) -> Keep(value, ctx)
    Ok(None) -> Drop(ctx)
    Error(_) -> NoMatch
  }
}

///
///
pub fn custom_matcher(
  f: fn(ctx, String, String) -> Result(#(Option(a), ctx), Nil),
) -> Matcher(a, ctx) {
  use ctx, lexeme, lookahead <- Matcher

  case f(ctx, lexeme, lookahead) {
    Ok(#(Some(value), ctx)) -> Keep(value, ctx)
    Ok(#(None, ctx)) -> Drop(ctx)
    Error(_) -> NoMatch
  }
}

///
///
pub fn token(str: String, value: a) -> Matcher(a, ctx) {
  use ctx, lexeme, _ <- Matcher

  case lexeme == str {
    True -> Keep(value, ctx)
    False -> NoMatch
  }
}

///
///
pub fn int(to_value: fn(Int) -> a) -> Matcher(a, ctx) {
  let assert Ok(digit) = regex.from_string("[0-9]")
  let assert Ok(integer) = regex.from_string("^[0-9]+$")

  use ctx, lexeme, lookahead <- Matcher

  case !regex.check(digit, lookahead) && regex.check(integer, lexeme) {
    False -> NoMatch
    True -> {
      let assert Ok(num) = int.parse(lexeme)
      Keep(to_value(num), ctx)
    }
  }
}

///
///
pub fn float(to_value: fn(Float) -> a) -> Matcher(a, ctx) {
  let assert Ok(digit) = regex.from_string("[0-9]")
  let assert Ok(integer) = regex.from_string("^[0-9]+$")
  let assert Ok(number) = regex.from_string("^[0-9]+\\.[0-9]+$")

  use ctx, lexeme, lookahead <- Matcher
  let is_int = !regex.check(digit, lookahead) && regex.check(integer, lexeme)
  let is_float = !regex.check(digit, lookahead) && regex.check(number, lexeme)

  case lexeme {
    "." if is_int -> NoMatch

    _ if is_float -> {
      let assert Ok(num) = float.parse(lexeme)
      Keep(to_value(num), ctx)
    }

    _ -> NoMatch
  }
}

///
///
pub fn identifier(
  start: String,
  inner: String,
  reserved: Set(String),
  to_value: fn(String) -> a,
) -> Matcher(a, ctx) {
  let assert Ok(ident) = regex.from_string("^" <> start <> inner <> "+$")
  let assert Ok(inner) = regex.from_string(inner)

  use ctx, lexeme, lookahead <- Matcher

  case !regex.check(inner, lookahead) && regex.check(ident, lexeme) {
    False -> NoMatch
    True ->
      case set.contains(reserved, lexeme) {
        True -> NoMatch
        False -> Keep(to_value(lexeme), ctx)
      }
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
pub fn run_with_ctx(
  source: String,
  ctx: ctx,
  lexer: Lexer(a, ctx),
) -> Result(List(Token(a)), Error) {
  do_run(lexer, ctx, State(string.to_graphemes(source), [], #(1, 1, ""), 1, 1))
}

fn do_run(
  lexer: Lexer(a, ctx),
  context: ctx,
  state: State(a),
) -> Result(List(Token(a)), Error) {
  let matchers = lexer.matchers(context)

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
      case do_match(context, lexeme, "", matchers) {
        NoMatch -> Error(NoMatchFound(start_row, start_col, lexeme))
        Drop(_) -> Ok(list.reverse(state.tokens))
        Keep(value, _) -> {
          let span = Span(start_row, start_col, state.row, state.col)
          let token = Token(span, lexeme, value)

          Ok(list.reverse([token, ..state.tokens]))
        }
      }

    // When lexing we include a one-grapheme lookahead to help us with things like
    // matching identifiers or other context-aware tokens. This just takes the 
    // next grapheme from the source (we call it `lookahead` here) and calls the
    // `do_match` function with it and some other bits.
    [lookahead, ..rest], #(start_row, start_col, lexeme) -> {
      let row = next_row(state.row, lexeme)
      let col = next_col(state.col, lexeme)

      case do_match(context, lexeme, lookahead, matchers) {
        Keep(value, ctx) -> {
          let span = Span(start_row, start_col, row, col)
          let token = Token(span, lexeme, value)

          do_run(
            lexer,
            ctx,
            State(
              source: rest,
              tokens: [token, ..state.tokens],
              current: #(row, col, lookahead),
              row: row,
              col: col,
            ),
          )
        }

        // A drop says that we've matched the lexeme but we don't want to emit a
        // token. This is mostly useful for things like comments or whitespace that
        // users may not want to appear in the final token stream but do want to
        // handle in the lexer.
        Drop(ctx) ->
          do_run(
            lexer,
            ctx,
            State(
              source: rest,
              tokens: state.tokens,
              current: #(row, col, lookahead),
              row: row,
              col: col,
            ),
          )

        NoMatch ->
          do_run(
            lexer,
            context,
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
  ctx: ctx,
  str: String,
  lookahead: String,
  matchers: List(Matcher(a, ctx)),
) -> Match(a, ctx) {
  use _, matcher <- list.fold_until(matchers, NoMatch)

  case matcher.run(ctx, str, lookahead) {
    Keep(_, _) as match -> list.Stop(match)
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
