# 00 Introduction

Nibble is a parser combinator library with a twist: it includes a lexer combinator
library as well! If some of those words already started to sound like gibberish
to you then don't worry, this introduction is going to get you up to speed on the
core concepts!

## Your first parser!

```gleam
type T {
  Hello
  Name(String)
}
```

```gleam
fn lexer() {
  lexer.simple([
    lexer.token("hello", Hello),
    lexer.variable(set.new(), Name),
    lexer.whitespace(Nil)
      |> lexer.ignore,
  ])
}
```

```gleam
fn parser() {
  use _ <- nibble.do(nibble.token(Hello))
  use name <- nibble.do(name_parser())

  nibble.return("You are greeting " <> name)
}

fn name_parser() {
  use tok <- nibble.take_map("name")

  case tok {
    Name(name) -> option.Some(name)
    _ -> option.None
  }
}
```

```gleam
pub fn main() {
  let input = "hello joe"

  use tokens <- result.try(
    input
    |> lexer.run(lexer()),
  )

  case tokens |> nibble.run(parser()) {
    Ok(value) -> io.println(value)
    Error(err) -> {
      let _ = io.debug(err)
      Nil
    }
  }

  Ok("")
  //=> "You are greeting joe"
}
```

## Terminology

Throughout Nibble's docs we use words that not all Gleamlins might have come
across before. Here's a quick rundown of the important terms and concepts to
know:

### What is a combinator?

Although you can find some more-formal definitions of what a combinator is –
looking at you, combinatory logic – we're Gleamlins here and we like to keep
things simple. For our purposes we can think of a combinators as functions that
work together like building blocks for more complex behaviour.

You'll have seen combinators already if you've ever written any code using
`gleam/dynamic`! With `gleam/dynamic` you combine decoders together to create more
complex ones:

```gleam
dynamic.field("wibble", dynamic.list(dynamic.int))
```

We can take the simple `dynamic.int` decoder and combine it with `dynamic.list`
to get back a decoder that can decode a _list_ of ints. And we can combine _that_
decoder with `dynamic.field` to get back a decoder that can decode a list of ints
from an object field called `"wibble"`! We can keep going, continuing to build
decoders up from smaller pieces: this is the essence of combinators!

### What is a parser?

In the broadest sense, a parser takes an unstructured sequence of stuff (often
characters in a string or tokens in a list) and turns it into something more
structured. You can imagine all parsers can be thought of as the same basic idea:

```
type Parser(a, b) = fn(List(a)) -> #(b, List(a))
```

In the real world parsers tend to be a bit more complex than this, including things
like errors and failure cases, position tracking, and so on. But in essence parsers
are combinators, and just like `gleam/dynamic` that means we can combine them
together to parse very complex things.

### What is a lexer?
