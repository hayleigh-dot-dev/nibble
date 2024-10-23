// IMPORTS ---------------------------------------------------------------------

import gleam/option.{None, Some}
import nibble.{
  any, do, eof, fail, guard, many, many1, map, one_of, optional, or, replace,
  return, run, sequence, take_at_least, take_exactly, take_if, take_map,
  take_map_while, take_until, take_while, take_while1, token,
}
import nibble/lexer

// TYPES -----------------------------------------------------------------------

pub type TokenT {
  AT
  BT
  CommaT
  LParenT
  RParenT
  NumT(Int)
}

// LEXER -----------------------------------------------------------------------
fn lexer() {
  lexer.simple([
    lexer.token("a", AT),
    lexer.token("b", BT),
    lexer.token(",", CommaT),
    lexer.token("(", LParenT),
    lexer.token(")", RParenT),
    lexer.int(NumT),
    lexer.whitespace(Nil)
      |> lexer.ignore,
  ])
}

// PARSERS ---------------------------------------------------------------------
fn number_parser() {
  use tok <- take_map("expected a number")
  case tok {
    NumT(n) -> Some(n)
    _ -> None
  }
}

// TESTS -----------------------------------------------------------------------

pub fn any_test() {
  let assert Ok(non_empty_tokens) = lexer.run("a", lexer())
  let assert Ok(AT) = run(non_empty_tokens, any())
  let assert Ok(empty_tokens) = lexer.run("", lexer())
  let assert Error(_) = run(empty_tokens, nibble.any())
}

pub fn do_test() {
  let assert Ok(tokens) = lexer.run("a", lexer())
  let parser = {
    use parsed_value <- do(token(AT))
    return(parsed_value)
  }
  let assert Ok(Nil) = run(tokens, parser)
}

pub fn eof_test() {
  let assert Ok(tokens) = lexer.run("aba", lexer())
  let successful_parser = {
    use _ <- do(token(AT))
    use _ <- do(token(BT))
    use _ <- do(token(AT))
    use _ <- do(eof())
    return(Nil)
  }
  let assert Ok(Nil) = run(tokens, successful_parser)
  let failing_parser = {
    use _ <- do(token(AT))
    use _ <- do(token(BT))
    use _ <- do(eof())
    return(Nil)
  }
  let assert Error(_) = run(tokens, failing_parser)
}

pub fn fail_test() {
  let assert Error(_) = run([], fail("I never succeed"))
}

pub fn guard_test() {
  let even_number_parser = {
    use number_value <- do(number_parser())
    use _ <- do(guard(number_value % 2 == 0, "expected an even number"))
    return(number_value)
  }
  let assert Ok(even_number_token) = lexer.run("10", lexer())
  let assert Ok(10) = run(even_number_token, even_number_parser)
  let assert Ok(odd_number_token) = lexer.run("13", lexer())
  let assert Error(_) = run(odd_number_token, even_number_parser)
}

pub fn many_test() {
  let assert Ok(tokens) = lexer.run("aaab", lexer())
  let assert Ok([Nil, Nil, Nil]) = run(tokens, many(token(AT)))
  let assert Ok(tokens) = lexer.run("b", lexer())
  let assert Ok([]) = run(tokens, many(token(AT)))
}

pub fn many1_test() {
  let assert Ok(tokens) = lexer.run("aaab", lexer())
  let assert Ok([Nil, Nil, Nil]) = run(tokens, many1(token(AT)))
  let assert Ok(tokens) = lexer.run("b", lexer())
  let assert Error(_) = run(tokens, many1(token(AT)))
}

pub fn map_test() {
  let double_parser = {
    use doubled_value <- do(number_parser() |> map(fn(n) { n * 2 }))
    return(doubled_value)
  }
  let assert Ok(token) = lexer.run("400", lexer())
  let assert Ok(800) = run(token, double_parser)
}

pub fn one_of_test() {
  let a_or_b_parser = {
    one_of([token(AT), token(BT)])
  }
  let assert Ok(tokens) = lexer.run("a", lexer())
  let assert Ok(Nil) = run(tokens, a_or_b_parser)
  let assert Ok(tokens) = lexer.run("b", lexer())
  let assert Ok(Nil) = run(tokens, a_or_b_parser)
  let assert Ok(tokens) = lexer.run("22", lexer())
  let assert Error(_) = run(tokens, a_or_b_parser)
}

pub fn optional_test() {
  let optional_a_parser = {
    token(AT) |> optional
  }
  let assert Ok(tokens) = lexer.run("a", lexer())
  let assert Ok(Some(Nil)) = run(tokens, optional_a_parser)
  let assert Ok(tokens) = lexer.run("b", lexer())
  let assert Ok(None) = run(tokens, optional_a_parser)
}

pub fn or_test() {
  let assert Ok(tokens) = lexer.run("a", lexer())
  let assert Ok(0) = run(tokens, number_parser() |> or(0))
}

pub fn replace_test() {
  let assert Ok(tokens) = lexer.run("a", lexer())
  let assert Ok("a") = run(tokens, token(AT) |> replace("a"))
}

pub fn sequence_test() {
  let assert Ok(tokens) = lexer.run("a,a,a", lexer())
  let assert Ok([Nil, Nil, Nil]) =
    run(tokens, sequence(token(AT), token(CommaT)))
}

pub fn take_at_least_test() {
  let assert Ok(tokens) = lexer.run("aaa", lexer())
  let assert Ok([Nil, Nil, Nil]) = run(tokens, take_at_least(token(AT), 2))
  let assert Ok(tokens) = lexer.run("abaa", lexer())
  let assert Error(_) = run(tokens, take_at_least(token(AT), 2))
}

pub fn take_exactly_test() {
  let assert Ok(tokens) = lexer.run("aaaaaa", lexer())
  let assert Ok([Nil, Nil]) = run(tokens, take_exactly(token(AT), 2))
  let assert Ok(tokens) = lexer.run("abaa", lexer())
  let assert Error(_) = run(tokens, take_exactly(token(AT), 2))
}

pub fn take_if_test() {
  let a_parser = {
    take_if("expected an 'a'", fn(tok) {
      case tok {
        AT -> True
        _ -> False
      }
    })
  }
  let assert Ok(tokens) = lexer.run("a", lexer())
  let assert Ok(AT) = run(tokens, a_parser)
  let assert Ok(tokens) = lexer.run("b", lexer())
  let assert Error(_) = run(tokens, a_parser)
}

pub fn take_map_test() {
  // See the body of `number_parser` for the use of `take_map`.
  let assert Ok(tokens) = lexer.run("10", lexer())
  let assert Ok(10) = run(tokens, number_parser())
  let assert Ok(tokens) = lexer.run("b", lexer())
  let assert Error(_) = run(tokens, number_parser())
}

pub fn take_map_while_test() {
  let f = fn(tok) {
    case tok {
      NumT(n) if n > 0 -> Some("positive")
      NumT(n) if n == 0 -> Some("zero")
      NumT(n) if n < 0 -> Some("negative")
      CommaT -> Some("comma")
      _ -> None
    }
  }
  let assert Ok(tokens) = lexer.run("10,-10", lexer())
  let assert Ok(["positive", "comma", "negative"]) =
    run(tokens, take_map_while(f))
}

pub fn take_until_test() {
  let not_a_number = fn(tok) {
    case tok {
      NumT(_) -> False
      _ -> True
    }
  }
  let assert Ok(tokens) = lexer.run("a,b,10", lexer())
  let assert Ok([]) = run(tokens, take_until(not_a_number))
  let assert Ok(tokens) = lexer.run("10a", lexer())
  let assert Ok([NumT(10)]) = run(tokens, take_until(not_a_number))
}

pub fn take_while_test() {
  let not_a_number = fn(tok) {
    case tok {
      NumT(_) -> False
      _ -> True
    }
  }
  let assert Ok(tokens) = lexer.run("a,b,10", lexer())
  let assert Ok([AT, CommaT, BT, CommaT]) =
    run(tokens, take_while(not_a_number))
  let assert Ok(tokens) = lexer.run("10", lexer())
  let assert Ok([]) = run(tokens, take_while(not_a_number))
}

pub fn take_while1_test() {
  let not_a_number = fn(tok) {
    case tok {
      NumT(_) -> False
      _ -> True
    }
  }
  let assert Ok(tokens) = lexer.run("a,b,10", lexer())
  let assert Ok([AT, CommaT, BT, CommaT]) =
    run(tokens, take_while1("expected a non-number", not_a_number))
  let assert Ok(tokens) = lexer.run("10", lexer())
  let assert Error(_) =
    run(tokens, take_while1("expected a non-number", not_a_number))
}
