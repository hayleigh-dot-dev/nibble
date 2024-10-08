// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import glearray.{type Array}
import nibble/lexer.{type Span, type Token, Span, Token}

// TYPES -----------------------------------------------------------------------

/// The `Parser` type has three parameters, let's take a look at each of them:
///
/// ```
/// Parser(a, tok, ctx)
/// // (1) ^
/// // (2)    ^^^
/// // (3)         ^^^
/// ```
///
/// 1) `a` is the type of value that the parser knows how to produce. If you were
///   writing a parser for a programming language, this might be your expression
///   type.
///
/// 2) `tok` is the type of tokens that the parser knows how to consume. You can
///   take a look at the [`Token`](./nibble/lexer#Token) type for a bit more info,
///   but note that it's not necessary for the token stream to come from nibble's
///   lexer.
///
/// 3) `ctx` is used to make error reporting nicer. You can place a parser into a
///   custom context. When the parser runs the context gets pushed into a stack.
///   If the parser fails you can see the context stack in the error message,
///   which can make error reporting and debugging much easier!
///
pub opaque type Parser(a, tok, ctx) {
  Parser(fn(State(tok, ctx)) -> Step(a, tok, ctx))
}

type Step(a, tok, ctx) {
  Cont(CanBacktrack, a, State(tok, ctx))
  Fail(CanBacktrack, Bag(tok, ctx))
}

type State(tok, ctx) {
  State(
    // ❓ You might wonder why we're wanting an `Array` at all when we could just
    // use a `List` and backtrack to a previous state when we need to. By tracking
    // the index and indexing into the dict/array directly we save ever having to
    // allocate something new, which is a big deal for performance!
    src: Array(Token(tok)),
    idx: Int,
    pos: Span,
    ctx: List(#(Span, ctx)),
  )
}

type CanBacktrack {
  CanBacktrack(Bool)
}

// RUNNING PARSERS -------------------------------------------------------------

/// Parsers don't do anything until they're run! The `run` function takes a
/// [`Parser`](#Parser) and a list of [`Token`](./nibble/lexer#Token)s and
/// runs it; returning either the parsed value or a list of [`DeadEnds`](#DeadEnd)
/// where the parser failed.
///
pub fn run(
  src: List(Token(tok)),
  parser: Parser(a, tok, ctx),
) -> Result(a, List(DeadEnd(tok, ctx))) {
  let init =
    State(src: glearray.from_list(src), idx: 0, pos: Span(1, 1, 1, 1), ctx: [])

  case runwrap(init, parser) {
    Cont(_, a, _) -> Ok(a)
    Fail(_, bag) -> Error(to_deadends(bag, []))
  }
}

fn runwrap(
  state: State(tok, ctx),
  parser: Parser(a, tok, ctx),
) -> Step(a, tok, ctx) {
  let Parser(parse) = parser
  parse(state)
}

fn next(state: State(tok, ctx)) -> #(Option(tok), State(tok, ctx)) {
  case glearray.get(state.src, state.idx) {
    Error(_) -> #(option.None, state)
    Ok(Token(span, _, tok)) -> #(
      option.Some(tok),
      State(..state, idx: state.idx + 1, pos: span),
    )
  }
}

// CONSTRUCTORS ----------------------------------------------------------------

/// The simplest kind of parser. [`return`](#return) consumes no tokens and always
/// produces the given value. Sometimes called [`succeed`](#succeed) instead.
///
/// This function might seem useless at first, but it is very useful when used in
/// combination with [`do`](#do) or [`then`](#then).
///
/// ```gleam
/// import nibble.{do, return}
///
/// fn unit8_parser() {
///   use int <- do(int_parser())
///
///   case int >= 0, int <= 255 {
///     True, True ->
///       return(int)
///
///     False, _ ->
///       throw("Expected an int >= 0")
///
///     _, False ->
///       throw("Expected an int <= 255")
///  }
/// }
/// ```
///
/// 💡 [`return`](#return`) and [`succeed`](#succeed) are names for the same thing.
/// We suggesting using `return` unqualified when using `do` and Gleam's `use`
/// syntax, and `nibble.succeed` in a pipeline with `nibble.then`.
pub fn return(value: a) -> Parser(a, tok, ctx) {
  use state <- Parser

  Cont(CanBacktrack(False), value, state)
}

/// The simplest kind of parser. [`succeed`](#succeed) consumes no tokens and always
/// produces the given value. Sometimes called [`return`](#return) instead.
///
/// This function might seem useless at first, but it is very useful when used in
/// combination with [`do`](#do) or [`then`](#then).
///
/// ```gleam
/// import nibble
///
/// fn unit8_parser() {
///   int_parser()
///   |> nibble.then(fn(int) {
///     case int >= 0, int <= 255 {
///       True, True -> succeed(int)
///       False, _ -> fail("Expected an int >= 0")
///       _, False -> fail("Expected an int <= 255")
///     }
///   })
/// }
/// ```
///
/// 💡 [`succeed`](#succeed) and [`return`](#return) are names for the same thing.
/// We suggest using `succeed` in a pipeline with `nibble.then`, and `return`
/// unqalified when using `do` with Gleam's `use` syntax.
///
pub fn succeed(value: a) -> Parser(a, tok, ctx) {
  return(value)
}

/// The opposite of [`return`](#return), this parser always fails with the given
/// message. Sometimes called [`fail`](#fail) instead.
///
pub fn throw(message: String) -> Parser(a, tok, ctx) {
  use state <- Parser
  let error = Custom(message)
  let bag = bag_from_state(state, error)

  Fail(CanBacktrack(False), bag)
}

/// Create a parser that consumes no tokens and always fails with the given
/// error message.
///
pub fn fail(message: String) -> Parser(a, tok, ctx) {
  throw(message)
}

/// Defer the creation of a parser until it is needed. This is often most useful
/// when creating a parser that is recursive and is *not* a function.
///
pub fn lazy(parser: fn() -> Parser(a, tok, ctx)) -> Parser(a, tok, ctx) {
  use state <- Parser

  runwrap(state, parser())
}

// BACKTRACKING ----------------------------------------------------------------

/// By default, parsers will not backtrack if they fail after consuming at least
/// one token. Passing a parser to `backtrackable` will change this behaviour and
/// allows us to jump back to the state of the parser before it consumed any input
/// and try another one.
///
/// This is most useful when you want to quickly try a few different parsers using
/// [`one_of`](#one_of).
///
/// 🚨 Backtracing parsers can drastically reduce performance, so you should avoid
/// them where possible. A common reason folks reach for backtracking is when they
/// want to try multiple branches that start with the same token or same sequence
/// of tokens.
///
/// To avoid backtracking in these cases, you can create an intermediate parser
/// that consumes the common tokens _and then_ use [`one_of`](#one_of) to try
/// the different branches.
///
pub fn backtrackable(parser: Parser(a, tok, ctx)) -> Parser(a, tok, ctx) {
  use state <- Parser

  case runwrap(state, parser) {
    Cont(_, a, state) -> Cont(CanBacktrack(False), a, state)
    Fail(_, bag) -> Fail(CanBacktrack(False), bag)
  }
}

fn should_commit(a: CanBacktrack, or b: CanBacktrack) -> CanBacktrack {
  let CanBacktrack(a) = a
  let CanBacktrack(b) = b

  CanBacktrack(a || b)
}

// MANIPULATING PARSERS --------------------------------------------------------

///
///
pub fn do(
  parser: Parser(a, tok, ctx),
  f: fn(a) -> Parser(b, tok, ctx),
) -> Parser(b, tok, ctx) {
  use state <- Parser

  case runwrap(state, parser) {
    Cont(to_a, a, state) ->
      case runwrap(state, f(a)) {
        Cont(to_b, b, state) -> Cont(should_commit(to_a, or: to_b), b, state)
        Fail(to_b, bag) -> Fail(should_commit(to_a, or: to_b), bag)
      }
    Fail(can_backtrack, bag) -> Fail(can_backtrack, bag)
  }
}

///
///
pub fn do_in(
  context: ctx,
  parser: Parser(a, tok, ctx),
  f: fn(a) -> Parser(b, tok, ctx),
) -> Parser(b, tok, ctx) {
  do(parser, f)
  |> in(context)
}

///
///
pub fn then(
  parser: Parser(a, tok, ctx),
  f: fn(a) -> Parser(b, tok, ctx),
) -> Parser(b, tok, ctx) {
  do(parser, f)
}

///
///
pub fn map(parser: Parser(a, tok, ctx), f: fn(a) -> b) -> Parser(b, tok, ctx) {
  use a <- do(parser)

  return(f(a))
}

///
///
pub fn replace(parser: Parser(a, tok, ctx), with b: b) -> Parser(b, tok, ctx) {
  map(parser, fn(_) { b })
}

// PARSER STATE ----------------------------------------------------------------

/// A parser that returns the current token position.
///
pub fn span() -> Parser(Span, tok, ctx) {
  use state <- Parser

  Cont(CanBacktrack(False), state.pos, state)
}

// SIMPLE PARSERS --------------------------------------------------------------

/// Returns the next token in the input stream. Fails if there are no more
/// tokens.
pub fn any() -> Parser(tok, tok, ctx) {
  take_if("a single token", fn(_) { True })
}

/// Returns nil if the token `tok` is the next token in the input stream. Fails
/// if the next token is not `tok` or if the input stream is empty.
pub fn token(tok: tok) -> Parser(Nil, tok, ctx) {
  use state <- Parser

  case next(state) {
    #(option.Some(t), state) if tok == t -> Cont(CanBacktrack(True), Nil, state)
    #(option.Some(t), state) ->
      Fail(
        CanBacktrack(False),
        bag_from_state(state, Expected(string.inspect(tok), t)),
      )
    #(option.None, state) ->
      Fail(CanBacktrack(False), bag_from_state(state, EndOfInput))
  }
}

/// Succeeeds if the input stream is empty, fails otherwise. This is useful to
/// verify that you've consumed all the tokens in the input stream.
///
pub fn eof() -> Parser(Nil, tok, ctx) {
  use state <- Parser

  case next(state) {
    #(option.Some(tok), state) ->
      Fail(CanBacktrack(False), bag_from_state(state, Unexpected(tok)))
    #(option.None, _) -> Cont(CanBacktrack(False), Nil, state)
  }
}

// BRANCHING AND LOOPING -------------------------------------------------------

/// Try the given parsers in order until one succeeds. If all fail, the parser
/// fails.
pub fn one_of(parsers: List(Parser(a, tok, ctx))) -> Parser(a, tok, ctx) {
  use state <- Parser
  let init = Fail(CanBacktrack(False), Empty)

  use result, next <- list.fold_until(parsers, init)

  case result {
    Cont(_, _, _) -> list.Stop(result)
    Fail(CanBacktrack(True), _) -> list.Stop(result)
    Fail(_, bag) ->
      runwrap(state, next)
      |> add_bag_to_step(bag)
      |> list.Continue
  }
}

///
/// Consumes a sequence of tokens using the given parser, separated by the
/// given `separator` parser. Returns a list of the parsed values, ignoring
/// the results of the `separator` parser.
///
pub fn sequence(
  parser: Parser(a, tok, ctx),
  separator sep: Parser(x, tok, ctx),
) -> Parser(List(a), tok, ctx) {
  one_of([
    parser
      |> then(more(_, parser, sep)),
    return([]),
  ])
}

///
/// Returns consecutive applications of the given parser. If you are parsing
/// values with a separator, use [`sequence`](#sequence) instead.
///
/// 💡 This parser can succeed without consuming any input. You can end up with
/// an infinite loop if you're not careful. Use [`many1`](#many1) if you want
/// to guarantee you take at least one token.
///
pub fn many(parser: Parser(a, tok, ctx)) -> Parser(List(a), tok, ctx) {
  sequence(parser, return(Nil))
}

///
/// This is the same as [`many1`](#many1), but is guaranteed to return at least
/// one value.
///
pub fn many1(parser: Parser(a, tok, ctx)) -> Parser(List(a), tok, ctx) {
  use x <- do(parser)
  use xs <- do(many(parser))

  return([x, ..xs])
}

fn more(
  x: a,
  parser: Parser(a, tok, ctx),
  separator: Parser(x, tok, ctx),
) -> Parser(List(a), tok, ctx) {
  use xs <- loop([x])
  // `break` is lazy so we don't reverse `xs` every iteration if we don't need
  // to.
  let break = fn() { return(Break(list.reverse(xs))) }
  let continue = {
    use _ <- do(separator)
    use x <- do(parser)

    return(Continue([x, ..xs]))
  }

  one_of([continue, lazy(break)])
}

///
///
pub type Loop(a, state) {
  Continue(state)
  Break(a)
}

///
///
pub fn loop(
  init: state,
  step: fn(state) -> Parser(Loop(a, state), tok, ctx),
) -> Parser(a, tok, ctx) {
  use state <- Parser

  loop_help(step, CanBacktrack(False), init, state)
}

fn loop_help(f, commit, loop_state, state) {
  case runwrap(state, f(loop_state)) {
    Cont(can_backtrack, Continue(next_loop_state), next_state) ->
      loop_help(
        f,
        should_commit(commit, can_backtrack),
        next_loop_state,
        next_state,
      )
    Cont(can_backtrack, Break(result), next_state) ->
      Cont(should_commit(commit, can_backtrack), result, next_state)
    Fail(can_backtrack, bag) -> Fail(should_commit(commit, can_backtrack), bag)
  }
}

// PREDICATES ------------------------------------------------------------------

///
/// Fails if the given condition is false, otherwise returns `Nil`.
///
pub fn guard(cond: Bool, expecting: String) -> Parser(Nil, tok, ctx) {
  case cond {
    True -> return(Nil)
    False -> fail(expecting)
  }
}

///
/// Takes the next token off the stream if it satisfies the given predicate.
///
pub fn take_if(
  expecting: String,
  predicate: fn(tok) -> Bool,
) -> Parser(tok, tok, ctx) {
  use state <- Parser
  let #(tok, next_state) = next(state)

  case tok, option.map(tok, predicate) {
    Some(tok), Some(True) -> Cont(CanBacktrack(False), tok, next_state)
    Some(tok), Some(False) ->
      Fail(
        CanBacktrack(False),
        bag_from_state(next_state, Expected(expecting, got: tok)),
      )
    _, _ -> Fail(CanBacktrack(False), bag_from_state(next_state, EndOfInput))
  }
}

///
/// Take tokens from the stream while the given predicate is satisfied.
///
/// 💡 This parser can succeed without consuming any input (if the predicate
/// immediately fails). You can end up with an infinite loop if you're not
/// careful. Use [`take_while1`](#take_while1) if you want to guarantee you
/// take at least one token.
///
pub fn take_while(predicate: fn(tok) -> Bool) -> Parser(List(tok), tok, ctx) {
  use state <- Parser
  let #(tok, next_state) = next(state)

  case tok, option.map(tok, predicate) {
    Some(tok), Some(True) ->
      runwrap(next_state, {
        use toks <- do(take_while(predicate))
        return([tok, ..toks])
      })
    Some(_), Some(False) -> Cont(CanBacktrack(False), [], state)
    _, _ -> Cont(CanBacktrack(False), [], state)
  }
}

///
/// Take tokens from the stream while the given predicate is satisfied.
///
/// 💡 If this parser succeeds, the list produced is guaranteed to be non-empty.
/// Feel free to `let assert` the result!
///
pub fn take_while1(
  expecting: String,
  predicate: fn(tok) -> Bool,
) -> Parser(List(tok), tok, ctx) {
  use x <- do(take_if(expecting, predicate))
  use xs <- do(take_while(predicate))

  return([x, ..xs])
}

///
/// Take token from the stream until the given predicate is satisfied.
///
/// 💡 This parser can succeed without consuming any input (if the predicate
/// immediately succeeds). You can end up with an infinite loop if you're not
/// careful. Use [`take_until1`](#take_until1) if you want to guarantee you
/// take at least one token.
///
pub fn take_until(predicate: fn(tok) -> Bool) -> Parser(List(tok), tok, ctx) {
  take_while(fn(tok) { bool.negate(predicate(tok)) })
}

///
/// Take token from the stream until the given predicate is satisfied.
///
/// 💡 If this parser succeeds, the list produced is guaranteed to be non-empty.
/// Feel free to `let assert` the result!
///
pub fn take_until1(
  expecting: String,
  predicate: fn(tok) -> Bool,
) -> Parser(List(tok), tok, ctx) {
  take_while1(expecting, fn(tok) { bool.negate(predicate(tok)) })
}

///
/// Apply the parser up to `count` times, returning a list of the results.
///
/// 💡 This parser can succeed without consuming any input (if the parser
/// fails immediately) and return an empty list. You can end up with an
/// infinite loop if you're not careful.
///
pub fn take_up_to(
  parser: Parser(a, tok, ctx),
  count: Int,
) -> Parser(List(a), tok, ctx) {
  case count {
    0 -> return([])
    _ ->
      {
        use x <- do(parser)
        use xs <- do(take_up_to(parser, count - 1))

        return([x, ..xs])
      }
      |> or([])
  }
}

///
/// Apply the parser a minimum of `count` times, returning a list of the results.
///
pub fn take_at_least(
  parser: Parser(a, tok, ctx),
  count: Int,
) -> Parser(List(a), tok, ctx) {
  case count {
    0 -> many(parser)
    _ -> {
      use x <- do(parser)
      use xs <- do(take_at_least(parser, count - 1))

      return([x, ..xs])
    }
  }
}

///
/// Take `count` consecutive tokens from the stream using the given parser.
///
pub fn take_exactly(
  parser: Parser(a, tok, ctx),
  count: Int,
) -> Parser(List(a), tok, ctx) {
  case count {
    0 -> return([])
    _ -> {
      use x <- do(parser)
      use xs <- do(take_exactly(parser, count - 1))

      return([x, ..xs])
    }
  }
}

///
/// Try the given parser, but if it fails return the given default value instead
/// of failing.
///
pub fn or(parser: Parser(a, tok, ctx), default: a) -> Parser(a, tok, ctx) {
  one_of([parser, return(default)])
}

///
/// Try the given parser, but if it fails return
/// [`None`](#https://hexdocs.pm/gleam_stdlib/gleam/option.html#Option) instead
/// of failing.
///
pub fn optional(parser: Parser(a, tok, ctx)) -> Parser(Option(a), tok, ctx) {
  one_of([map(parser, Some), return(None)])
}

///
/// Take the next token and attempt to transform it with the given function. This
/// is useful when creating reusable primtive parsers for your own tokens such as
/// `take_identifier` or `take_number`.
///
pub fn take_map(
  expecting: String,
  f: fn(tok) -> Option(a),
) -> Parser(a, tok, ctx) {
  use state <- Parser
  let #(tok, next_state) = next(state)

  case tok, option.then(tok, f) {
    None, _ -> Fail(CanBacktrack(False), bag_from_state(next_state, EndOfInput))
    Some(tok), None ->
      Fail(
        CanBacktrack(False),
        bag_from_state(next_state, Expected(expecting, got: tok)),
      )
    _, Some(a) -> Cont(CanBacktrack(False), a, next_state)
  }
}

///
/// Applies a function to consecutive tokens while the given function returns
/// `Some`.
///
/// 💡 This parser can succeed without consuming any input (if the predicate
/// immediately succeeds). You can end up with an infinite loop if you're not
/// careful. Use [`take_map_while1`](#take_map_while1) if you want to guarantee you
/// take at least one token.
///
pub fn take_map_while(f: fn(tok) -> Option(a)) -> Parser(List(a), tok, ctx) {
  use state <- Parser
  let #(tok, next_state) = next(state)

  case tok, option.then(tok, f) {
    None, _ -> Cont(CanBacktrack(True), [], state)
    Some(_), None -> Cont(CanBacktrack(True), [], state)
    _, Some(x) ->
      runwrap(
        next_state,
        take_map_while(f)
          |> map(list.prepend(_, x)),
      )
  }
}

/// Applies a function to consecutive tokens while the given function returns
/// `Some`.
///
/// 💡 If this parser succeeds, the list produced is guaranteed to be non-empty.
/// Feel free to `let assert` the result!
///
pub fn take_map_while1(
  expecting: String,
  f: fn(tok) -> Option(a),
) -> Parser(List(a), tok, ctx) {
  use x <- do(take_map(expecting, f))
  use xs <- do(take_map_while(f))

  return([x, ..xs])
}

// ERRORS ----------------------------------------------------------------------

///
///
///
///
pub type Error(tok) {
  BadParser(String)
  Custom(String)
  EndOfInput
  Expected(String, got: tok)
  Unexpected(tok)
}

///
/// A dead end represents a the point where a parser that had committed down a
/// path failed. It contains the position of the failure, the [`Error`](#Error)
/// describing the failure, and the context stack for any parsers that had run.
///
pub type DeadEnd(tok, ctx) {
  DeadEnd(pos: Span, problem: Error(tok), context: List(#(Span, ctx)))
}

type Bag(tok, ctx) {
  Empty
  Cons(Bag(tok, ctx), DeadEnd(tok, ctx))
  Append(Bag(tok, ctx), Bag(tok, ctx))
}

fn bag_from_state(state: State(tok, ctx), problem: Error(tok)) -> Bag(tok, ctx) {
  Cons(Empty, DeadEnd(state.pos, problem, state.ctx))
}

fn to_deadends(
  bag: Bag(tok, ctx),
  acc: List(DeadEnd(tok, ctx)),
) -> List(DeadEnd(tok, ctx)) {
  case bag {
    Empty -> acc
    Cons(Empty, deadend) -> [deadend, ..acc]
    Cons(bag, deadend) -> to_deadends(bag, [deadend, ..acc])
    Append(left, right) -> to_deadends(left, to_deadends(right, acc))
  }
}

fn add_bag_to_step(
  step: Step(a, tok, ctx),
  left: Bag(tok, ctx),
) -> Step(a, tok, ctx) {
  case step {
    Cont(can_backtrack, a, state) -> Cont(can_backtrack, a, state)
    Fail(can_backtrack, right) -> Fail(can_backtrack, Append(left, right))
  }
}

// CONTEXT ---------------------------------------------------------------------

///
///
pub fn in(parser: Parser(a, tok, ctx), context: ctx) -> Parser(a, tok, ctx) {
  use state <- Parser

  case runwrap(push_context(state, context), parser) {
    Cont(can_backtrack, a, state) -> Cont(can_backtrack, a, pop_context(state))
    Fail(can_backtrack, bag) -> Fail(can_backtrack, bag)
  }
}

fn push_context(state: State(tok, ctx), context: ctx) -> State(tok, ctx) {
  State(..state, ctx: [#(state.pos, context), ..state.ctx])
}

fn pop_context(state: State(tok, ctx)) -> State(tok, ctx) {
  case state.ctx {
    [] -> state
    [_, ..context] -> State(..state, ctx: context)
  }
}

///
/// Run the given parser and then inspect it's state.
///
pub fn inspect(
  parser: Parser(a, tok, ctx),
  message: String,
) -> Parser(a, tok, ctx) {
  use state <- Parser
  io.println(message <> ": ")

  runwrap(state, parser)
  |> io.debug
}
