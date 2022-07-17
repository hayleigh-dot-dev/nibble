////

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/float
import gleam/function
import gleam/int
import gleam/io
import gleam/list
import gleam/map.{ Map }
import gleam/option.{ Option }
import gleam/string
import nibble/predicates

// TYPES -----------------------------------------------------------------------

///
pub opaque type Parser(a, e, ctx) {
    Parser(fn (State(ctx)) -> Step(a, e, ctx))
}

type State(ctx) {
    State(
        // The Gleam stdlib doesn't seem to have an `Array` type, so we'll just
        // use a `Map` instead. We only need something for indexed access, to it's
        // not a huge deal.
        //
        // TODO: Louis says making an `Array` backed by tuples in Erlang will
        // be way better for performance. In JavaScript we could just use normal
        // arrays - someone should look into this. 
        src : Map(Int, String),
        offset : Int,
        context : List(Located(ctx)),
        row : Int,
        col : Int
    )
}

type Step(a, e, ctx) {
  Cont(Backtrackable, a, State(ctx))
  Fail(Backtrackable, Bag(e, ctx))   
}

///
pub type Located(ctx) {
    Located(
        row : Int,
        col : Int,
        context : ctx
    )
}

type Backtrackable {
    Commit
    Backtrack
}

// RUNNING PARSERS -------------------------------------------------------------

///
pub fn run (src: String, parser: Parser(a, e, ctx)) -> Result(a, List(DeadEnd(e, ctx))) {
    let graphemes =
        string.to_graphemes(src)
            |> list.index_map(fn (i, grapheme) { #(i, grapheme) })
            |> map.from_list

    let init = State(graphemes, 0, [], 1, 1)

    case runwrap(init, parser) {
        Cont(_, a, _) -> 
            Ok(a)
        
        Fail(_, bag) ->
            Error(to_deadends(bag, []))
    }
}

fn runwrap (state: State(ctx), parser: Parser(a, e, ctx)) -> Step(a, e, ctx) {
    let Parser(parse) = parser;
    parse(state)
}

fn next (state: State(ctx)) -> #(Option(String), State(ctx)) {
    case map.get(state.src, state.offset) {
        Ok("\n") ->
            #(option.Some("\n"), State(..state, offset: state.offset + 1, col: 1, row: state.row + 1))

        Ok(g) ->
            #(option.Some(g), State(..state, offset: state.offset + 1, col: state.col + 1))
        
        Error(_) ->
            #(option.None, state)
    }
}

// CONSTRUCTORS ----------------------------------------------------------------

///
pub fn succeed (a: a) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        Cont(Backtrack, a, state)
    })
}

///
pub fn fail (error: e) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        Fail(Backtrack, bag_from_state(state, error))
    })
}

///
pub fn lazy (parser: fn () -> Parser(a, e, ctx)) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        runwrap(state, parser())
    })
}

///
pub fn from_result (result: Result(a, e)) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        case result {
            Ok(a) ->
                Cont(Backtrack, a, state)
            
            Error(e) ->
                Fail(Backtrack, bag_from_state(state, e))
        }
    })
}

///
pub fn from_option (option: Option(a), error: e) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        case option {
            option.Some(a) ->
                Cont(Backtrack, a, state)
            
            option.None ->
                Fail(Backtrack, bag_from_state(state, error))
        }
    })
}


// BACKTRACKING ----------------------------------------------------------------

///
pub fn backtrackable (parser: Parser(a, e, ctx)) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        case runwrap(state, parser) {
            Cont(_, a, state) ->
                Cont(Backtrack, a, state)
            
            Fail(_, bag) ->
                Fail(Backtrack, bag)
        }
    })
}

///
pub fn commit (to a: a) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        Cont(Commit, a, state)
    })
}

fn should_commit (to_x: Backtrackable, or to_y: Backtrackable) -> Backtrackable {
    case to_x, to_y {
        Commit, _ ->
            Commit

        _, Commit ->
            Commit

        _, _ ->
            Backtrack
    }
}

// MANIPULATING PARSERS --------------------------------------------------------

///
pub fn then (parser: Parser(a, e, ctx), f: fn (a) -> Parser(b, e, ctx)) -> Parser(b, e, ctx) {
    Parser(fn (state) {
        case runwrap(state, parser) {
            Cont(to_a, a, state) ->
                case runwrap(state, f(a)) {
                    Cont(to_b, b, state) ->
                        Cont(should_commit(to_a, or: to_b), b, state)

                    Fail(to_b, bag) ->
                        Fail(should_commit(to_a, or: to_b), bag)
                }

            Fail(p1, bag) ->
                Fail(p1, bag)
        }
    })
}

///
pub fn map (parser: Parser(a, e, ctx), f: fn (a) -> b) -> Parser(b, e, ctx) {
    then(parser, fn (a) {
        succeed(f(a))
    })
}

fn map2 (parse_a: Parser(a, e, ctx), parse_b: Parser(b, e, ctx), f: fn (a, b) -> c) -> Parser(c, e, ctx) {
    then(parse_a, fn (a) {
        map(parse_b, fn (b) {
            f(a, b)
        })
    })
}

///
pub fn replace (parser: Parser(a, e, ctx), with b: b) -> Parser(b, e, ctx) {
    map(parser, fn (_) {
        b
    })
}

// PIPE-FRIENDLY HELPERS -------------------------------------------------------

///
pub fn keep (parse_f: Parser(fn (a) -> b, e, ctx), parse_a: Parser(a, e, ctx)) -> Parser(b, e, ctx) {
    map2(parse_f, parse_a, fn (f, a) {
        f(a)
    })
}

///
pub fn drop (parse_a: Parser(a, e, ctx), parse_x: Parser(x, e, ctx)) -> Parser(a, e, ctx) {
    map2(parse_a, parse_x, fn (a, _) {
        a
    })
}

// SIMPLE PARSERS --------------------------------------------------------------

///
pub fn any (error: fn (String) -> e) -> Parser(String, e, ctx) {
    take_if(function.constant(True), error)
}

///
pub fn eof (error: e) -> Parser(Nil, e, ctx) {
    Parser(fn (state) {
        case next(state) {
            #(option.Some(_), _) ->
                Fail(Backtrack, bag_from_state(state, error))
            
            #(option.None, _) ->
                Cont(Backtrack, Nil, state)
        }
    })
}

// GRAPHEMES AND STRINGS -------------------------------------------------------

///
pub fn grapheme (str: String, error: fn (String) -> e) -> Parser(Nil, e, ctx) {
    take_if(fn (g) { g == str }, error)
        |> map(function.constant(Nil))
}

///
pub fn string (str: String, error: fn (String) -> e) -> Parser(Nil, e, ctx) {
    let graphemes = string.to_graphemes(str)

    Parser(fn (state) {
        case graphemes {
            [ head, ..tail ] -> {
                let parse_each = list.fold(tail, grapheme(head, error), fn (parse, next) {
                    parse |> drop(grapheme(next, error))
                })

                case runwrap(state, parse_each) {
                    Cont(_, _, state) ->
                        Cont(Commit, Nil, state)
                    
                    Fail(_, _) ->
                        Fail(Backtrack, bag_from_state(state, error("")))
                }
            }

            [] ->
                Cont(Commit, Nil, state)
        }
    })
}

// NUMBERS ---------------------------------------------------------------------

///
pub fn int (error: fn (String) -> e) -> Parser(Int, e, ctx) {
    take_if_and_while(predicates.is_digit, error)
        // We can make the following assertion because we know our parser will
        // only consume digits, and is guaranteed to have at least one.
        |> map(fn (digits) {
            assert Ok(int) = int.parse(digits)
            int
        })
}

///
pub fn float (error: fn (String) -> e) -> Parser(Float, e, ctx) {
    let make_float_string = function.curry2(fn (x, y) {
        string.concat([x, ".", y])
    })

    succeed(make_float_string)
        |> keep(take_if_and_while(predicates.is_digit, error))
        |> drop(grapheme(".", error))
        |> keep(take_if_and_while(predicates.is_digit, error))
        // We can make the following assertion because we know our parser will
        // only consume digits, and is guaranteed to have at least one.
        |> map(fn (digits) {
            assert Ok(float) = float.parse(digits)
            float
        })
}

// WHITESPACE ------------------------------------------------------------------

///
pub fn spaces () -> Parser(Nil, e, ctx) {
    take_while(fn (g) { g == " "})
        |> map(function.constant(Nil))
}

///
pub fn whitespace () -> Parser(Nil, e, ctx) {
    take_while(predicates.is_whitespace)
        |> map(function.constant(Nil))
}

// BRANCHING AND LOOPING -------------------------------------------------------

///
pub fn one_of (parsers: List(Parser(a, e, ctx))) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        let init = Fail(Backtrack, Empty)

        list.fold_until(parsers, init, fn (result, next) {
            case result {
                Cont(_, _, _) ->
                    list.Stop(result)
                
                Fail(Commit, _) ->
                    list.Stop(result)
                
                Fail(_, bag) ->
                    runwrap(state, next)
                        |> add_bag_to_step(_, bag)
                        |> list.Continue
            }
        })
    })
}

///
pub fn many (parser: Parser(a, e, ctx), separator: Parser(x, e, ctx)) -> Parser(List(a), e, ctx) {
    one_of([
        parser |> then(more(_, parser, separator)),
        succeed([])
    ])
}

fn more (x: a, parser: Parser(a, e, ctx), separator: Parser(x, e, ctx)) -> Parser(List(a), e, ctx) {
    one_of([
        succeed(list.prepend(_, x))
            |> drop(separator)
            |> keep(many(parser, separator))
            |> backtrackable,
        succeed([x])
    ])
}

// PREDICATES ------------------------------------------------------------------

///
pub fn take_if (predicate: fn (String) -> Bool, error: fn (String) -> e) -> Parser(String, e, ctx) {
    Parser(fn (state) {
        let #(str, next_state) = next(state)
        let should_take = str |> option.map(predicate) |> option.unwrap(False)
        let str = option.unwrap(str, "")

        case should_take {
            True ->
                Cont(Commit, str, next_state)
            
            False ->
                Fail(Backtrack, bag_from_state(state, error(str)))
        }
    })
}

///
pub fn take_while (predicate: fn (String) -> Bool) -> Parser(String, e, ctx) {
    Parser(fn (state) {
        let #(str, next_state) = next(state)
        let should_take = str |> option.map(predicate) |> option.unwrap(False)
        let str = option.unwrap(str, "")

        case should_take {
            True ->
                runwrap(next_state, map(take_while(predicate), string.append(str, _)))
            
            False ->
                Cont(Backtrack, "", state)
        }
    })
}

///
pub fn take_if_and_while (predicate: fn (String) -> Bool, error: fn (String) -> e) -> Parser(String, e, ctx) {
    map2(take_if(predicate, error), take_while(predicate), string.append)

}

///
pub fn take_until (predicate: fn (String) -> Bool) -> Parser(String, e, ctx) {
    take_while(function.compose(predicate, bool.negate))
}

// ERRORS ----------------------------------------------------------------------

///
pub type DeadEnd(e, ctx) {
    DeadEnd(
        row : Int,
        col : Int,
        problem : e,
        context : List(Located(ctx))
    )
}

type Bag(e, ctx) {
    Empty
    Cons(Bag(e, ctx), DeadEnd(e, ctx))
    Append(Bag(e, ctx), Bag(e, ctx))
}

fn bag_from_state (state: State(ctx), problem: e) -> Bag(e, ctx) {
    Cons(Empty, DeadEnd(state.row, state.col, problem, state.context))
}

fn to_deadends (bag: Bag(e, ctx), acc: List(DeadEnd(e, ctx))) -> List (DeadEnd(e, ctx)) {
    case bag {
        Empty ->
            acc

        Cons(Empty, deadend) ->
            [ deadend, ..acc ]

        Cons(bag, deadend) ->
            to_deadends(bag, [ deadend, ..acc])

        Append(left, right) ->
            to_deadends(left, to_deadends(right, acc))
    }
}

fn add_bag_to_step (step: Step(a, e, ctx), left: Bag(e, ctx)) -> Step(a, e, ctx) {
    case step {
        Cont(can_backtrack, a, state) ->
            Cont(can_backtrack, a, state)
        
        Fail(can_backtrack, right) ->
            Fail(can_backtrack, Append(left, right))
    }
}

// CONTEXT ---------------------------------------------------------------------

///
pub fn in (parser: Parser(a, e, ctx), context: ctx) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        case runwrap(push_context(state, context), parser) {
            Cont(can_backtrack, a, state) ->
                Cont(can_backtrack, a, pop_context(state))

            Fail(can_backtrack, bag) ->
                Fail(can_backtrack, bag)
        }
    })
}

fn push_context (state: State(ctx), context: ctx) -> State(ctx) {
    let located = Located(state.row, state.col, context)
    State(..state, context: [ located, ..state.context ])
}

fn pop_context (state: State(ctx)) -> State(ctx) {
    case state.context {
        [] -> 
            state

        [ _, ..context ] ->
            State(..state, context: context)
    }
}

/// Run the given parser and then inspect it's state. 
pub fn inspect (parser: Parser(a, e, ctx), message: String) -> Parser(a, e, ctx) {
    Parser(fn (state) {
        io.print(message)
        io.println(": ")

        runwrap(state, parser)
            |> io.debug
    })
}
