// IMPORTS ---------------------------------------------------------------------

import gleam/list
import nibble.{type Parser}

// TYPES -----------------------------------------------------------------------

pub opaque type Config(a, tok, ctx) {
  Config(
    one_of: List(fn(Config(a, tok, ctx)) -> Parser(a, tok, ctx)),
    and_then_one_of: List(Operator(a, tok, ctx)),
    spaces: Parser(Nil, tok, ctx),
  )
}

pub opaque type Operator(a, tok, ctx) {
  Operator(fn(Config(a, tok, ctx)) -> #(Int, fn(a) -> Parser(a, tok, ctx)))
}

//

pub fn expression(
  one_of first: List(fn(Config(a, tok, ctx)) -> Parser(a, tok, ctx)),
  and_then then: List(Operator(a, tok, ctx)),
  dropping spaces: Parser(Nil, tok, ctx),
) -> Parser(a, tok, ctx) {
  let config = Config(first, then, spaces)
  sub_expression(config, 0)
}

pub fn sub_expression(
  config: Config(a, tok, ctx),
  precedence: Int,
) -> Parser(a, tok, ctx) {
  let expr = {
    use <- nibble.lazy
    config.one_of
    |> list.map(fn(p) { p(config) })
    |> nibble.one_of
  }

  let go = fn(expr) {
    use _ <- nibble.do(config.spaces)

    nibble.one_of([
      operation(expr, config, precedence)
      |> nibble.map(nibble.Continue),
      nibble.return(expr)
      |> nibble.map(nibble.Break),
    ])
  }

  use _ <- nibble.do(config.spaces)
  use e <- nibble.do(expr)

  nibble.loop(e, go)
}

fn operation(
  expr: a,
  config: Config(a, tok, ctx),
  current_precedence: Int,
) -> Parser(a, tok, ctx) {
  config.and_then_one_of
  |> list.filter_map(fn(operator) {
    let Operator(op) = operator
    case op(config) {
      #(precedence, parser) if precedence > current_precedence ->
        Ok(parser(expr))

      _ -> Error(Nil)
    }
  })
  |> nibble.one_of()
}

//

pub fn prefix(
  precedence: Int,
  operator: Parser(Nil, tok, ctx),
  apply: fn(a) -> a,
) -> fn(Config(a, tok, ctx)) -> Parser(a, tok, ctx) {
  fn(config) {
    use _ <- nibble.do(operator)
    use subexpr <- nibble.do(sub_expression(config, precedence))

    nibble.return(apply(subexpr))
  }
}

pub fn infix_left(
  precedence: Int,
  operator: Parser(Nil, tok, ctx),
  apply: fn(a, a) -> a,
) -> Operator(a, tok, ctx) {
  make_infix(#(precedence, precedence), operator, apply)
}

pub fn infix_right(
  precedence: Int,
  operator: Parser(Nil, tok, ctx),
  apply: fn(a, a) -> a,
) -> Operator(a, tok, ctx) {
  make_infix(#(precedence, precedence - 1), operator, apply)
}

pub fn postfix(
  precedence: Int,
  operator: Parser(Nil, tok, ctx),
  apply: fn(a) -> a,
) -> Operator(a, tok, ctx) {
  use _ <- Operator
  #(
    precedence,
    fn(lhs) {
      use _ <- nibble.do(operator)
      nibble.return(apply(lhs))
    },
  )
}

fn make_infix(
  precedence: #(Int, Int),
  operator: Parser(Nil, tok, ctx),
  apply: fn(a, a) -> a,
) -> Operator(a, tok, ctx) {
  let #(left_precedence, right_precedence) = precedence
  use config <- Operator
  #(
    left_precedence,
    fn(lhs) {
      use _ <- nibble.do(operator)
      use subexpr <- nibble.do(sub_expression(config, right_precedence))

      nibble.return(apply(lhs, subexpr))
    },
  )
}
