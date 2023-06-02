////

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/function
import gleam/io
import gleam/string
import gleam/list
import gleam/map.{Map}
import gleam/option.{None, Option, Some}
import nibble/lexer.{Span, Token}

// TYPES -----------------------------------------------------------------------

/// The `Parser` type has three paramteres, let's take a look at each of them:
///
/// ```
/// Parser(a, tok, ctx)
/// //     ^            (1)
/// //        ^^^       (2)
/// //             ^^^  (3)
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
    // The Gleam stdlib doesn't seem to have an `Array` type, so we'll just
    // use a `Map` instead. We only need something for indexed access, to it's
    // not a huge deal.
    //
    // TODO: Louis says making an `Array` backed by tuples in Erlang will
    // be way better for performance. In JavaScript we could just use normal
    // arrays - someone should look into this. 
    //
    // ❓ You might wonder why we're wanting an `Array` at all when we could just
    // use a `List` and backtrack to a previous state when we need to. By tracking
    // the index and indexing into the map/array directly we save ever having to
    // allocate something new, which is a big deal for performance!
    src: Map(Int, Token(tok)),
    idx: Int,
    pos: Span,
    ctx: List(Located(ctx)),
  )
}

///
///
pub type Located(ctx) {
  Located(pos: Span, context: ctx)
}

type CanBacktrack {
  CanBacktrack(Bool)
}

// RUNNING PARSERS -------------------------------------------------------------

///
///
pub fn run(
  src: List(Token(tok)),
  parser: Parser(a, tok, ctx),
) -> Result(a, List(DeadEnd(tok, ctx))) {
  let src =
    list.index_fold(
      src,
      map.new(),
      fn(map, tok, idx) { map.insert(map, idx, tok) },
    )
  let init = State(src, 0, Span(1, 1, 1, 1), [])

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
  case map.get(state.src, state.idx) {
    Error(_) -> #(option.None, state)
    Ok(Token(span, _, tok)) -> #(
      option.Some(tok),
      State(..state, idx: state.idx + 1, pos: span),
    )
  }
}

// CONSTRUCTORS ----------------------------------------------------------------

///
///
pub fn return(a: a) -> Parser(a, tok, ctx) {
  Parser(fn(state) { Cont(CanBacktrack(False), a, state) })
}

///
///
pub fn fail(message: String) -> Parser(a, tok, ctx) {
  Parser(fn(state) {
    Fail(CanBacktrack(False), bag_from_state(state, Custom(message)))
  })
}

///
///
pub fn lazy(parser: fn() -> Parser(a, tok, ctx)) -> Parser(a, tok, ctx) {
  Parser(fn(state) { runwrap(state, parser()) })
}

// BACKTRACKING ----------------------------------------------------------------

///
///
pub fn backtrackable(parser: Parser(a, tok, ctx)) -> Parser(a, tok, ctx) {
  Parser(fn(state) {
    case runwrap(state, parser) {
      Cont(_, a, state) -> Cont(CanBacktrack(False), a, state)
      Fail(_, bag) -> Fail(CanBacktrack(False), bag)
    }
  })
}

///
///
pub fn commit(to a: a) -> Parser(a, tok, ctx) {
  Parser(fn(state) { Cont(CanBacktrack(True), a, state) })
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
  Parser(fn(state) {
    case runwrap(state, parser) {
      Cont(to_a, a, state) ->
        case runwrap(state, f(a)) {
          Cont(to_b, b, state) -> Cont(should_commit(to_a, or: to_b), b, state)
          Fail(to_b, bag) -> Fail(should_commit(to_a, or: to_b), bag)
        }

      Fail(can_backtrack, bag) -> Fail(can_backtrack, bag)
    }
  })
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

// SIMPLE PARSERS --------------------------------------------------------------

///
///
pub fn any() -> Parser(tok, tok, ctx) {
  take_if("a single grapheme", function.constant(True))
}

///
///
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

///
///
pub fn eof() -> Parser(Nil, tok, ctx) {
  Parser(fn(state) {
    case next(state) {
      #(option.Some(tok), _) ->
        Fail(CanBacktrack(False), bag_from_state(state, Unexpected(tok)))

      #(option.None, _) -> Cont(CanBacktrack(False), Nil, state)
    }
  })
}

// BRANCHING AND LOOPING -------------------------------------------------------

///
///
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
///
pub fn list(
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
///
pub fn many(parser: Parser(a, tok, ctx)) -> Parser(List(a), tok, ctx) {
  list(parser, return(Nil))
}

///
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

  one_of([
    {
      use _ <- do(separator)
      use x <- do(parser)

      return(Continue([x, ..xs]))
    },
    return(Break(list.reverse(xs))),
  ])
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
  Parser(fn(state) { loop_help(step, CanBacktrack(False), init, state) })
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
        bag_from_state(state, Expected(expecting, got: tok)),
      )
    None, _ -> Fail(CanBacktrack(False), bag_from_state(state, EndOfInput))
  }
}

///
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
      runwrap(
        next_state,
        {
          use toks <- do(take_while(predicate))
          return([tok, ..toks])
        },
      )
    Some(_), Some(False) -> Cont(CanBacktrack(False), [], state)
    None, _ -> Cont(CanBacktrack(False), [], state)
  }
}

///
///
/// 💡 If this parser succeeds, the list produced is guaranteed to be non-empty.
/// `let assert` away!
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
///
pub fn take_until(predicate: fn(tok) -> Bool) -> Parser(List(tok), tok, ctx) {
  take_while(function.compose(predicate, bool.negate))
}

///
///
pub fn take_until1(
  expecting: String,
  predicate: fn(tok) -> Bool,
) -> Parser(List(tok), tok, ctx) {
  take_while1(expecting, function.compose(predicate, bool.negate))
}

///
///
pub fn or(parser: Parser(a, tok, ctx), default: a) -> Parser(a, tok, ctx) {
  one_of([parser, return(default)])
}

///
///
pub fn optional(parser: Parser(a, tok, ctx)) -> Parser(Option(a), tok, ctx) {
  one_of([map(parser, Some), return(None)])
}

///
///
pub fn take_map(
  expecting: String,
  f: fn(tok) -> Option(a),
) -> Parser(a, tok, ctx) {
  use state <- Parser
  let #(tok, next_state) = next(state)

  case tok, option.then(tok, f) {
    None, _ -> Fail(CanBacktrack(False), bag_from_state(state, EndOfInput))
    Some(tok), None ->
      Fail(
        CanBacktrack(False),
        bag_from_state(state, Expected(expecting, got: tok)),
      )
    _, Some(a) -> Cont(CanBacktrack(False), a, next_state)
  }
}

///
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

/// A dead end represents a the point where a parser that had committed down a
/// path failed. It contains the position of the failure, the [`Error`](#Error)
/// describing the failure, and the context stack for any parsers that had run.
///
pub type DeadEnd(tok, ctx) {
  DeadEnd(pos: Span, problem: Error(tok), context: List(Located(ctx)))
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
  Parser(fn(state) {
    case runwrap(push_context(state, context), parser) {
      Cont(can_backtrack, a, state) ->
        Cont(can_backtrack, a, pop_context(state))

      Fail(can_backtrack, bag) -> Fail(can_backtrack, bag)
    }
  })
}

fn push_context(state: State(tok, ctx), context: ctx) -> State(tok, ctx) {
  let located = Located(state.pos, context)
  State(..state, ctx: [located, ..state.ctx])
}

fn pop_context(state: State(tok, ctx)) -> State(tok, ctx) {
  case state.ctx {
    [] -> state
    [_, ..context] -> State(..state, ctx: context)
  }
}

/// Run the given parser and then inspect it's state. 
pub fn inspect(
  parser: Parser(a, tok, ctx),
  message: String,
) -> Parser(a, tok, ctx) {
  Parser(fn(state) {
    io.print(message)
    io.println(": ")

    runwrap(state, parser)
    |> io.debug
  })
}
