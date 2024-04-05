# Lexer Modes

Up until now we have been running our lexer using `lexer.simple`. As the name
implies, this is the simplest way to use Nibble's lexer and it is context-free.
Where possible we should try to stick to these simple lexers, but sometimes we
need to be able to lex things that are context-sensitive. That's where lexer
modes come in!

## Indentation Sensitivity

Let's imagine we're writing a lexer for a Python-ish programming language and
we want to produce `Indent` and `Dedent` tokens to represent indentation. We
might define our tokens like this:

```gleam
pub type TokenT {
  Var(String)
  Str(String)
  Num(Int)

  // Keywords
  Def
  For
  In
  Print

  // Indentation
  Indent(Int)
  Dedent(Int)
}
```

We could represent a chunk of code like this:

```python
def wibble arr
  for x in arr
    print x

  print "done!"

def wobble
  wibble [1, 2, 3]
```

Indentation would change the meaning of this program, so we need to know when we
are inside a block of indented code or not. Our `Indent` and `Dedent` tokens
carry with them the level of indentation they represent such that when we come to
parsing we can make sure everything is valid, but how do we produce the tokens in
the first place?

We'll need to do two things: (1) write a custom matcher using `lexer.custom` and
(2) store the current indentation level as the lexer's mode.

```gleam
pub opaque type Lexer(a, mode)
pub opaque type Matcher(a, mode)
```

Modes allow us to chose different matchers for different contexts, or inject
state into our matchers. For our indentation-sensitive lexer, that means we'll
end up with `Lexer` and `Matcher` types like this:

```gleam
type Lexer = nibble.Lexer(TokenT, Int)
type Matcher = nibble.Matcher(TokenT, Int)
```

To write our `indentation` matcher, we'll count the number of spaces that immediately
follow a newline and compare that to the current indentation level. If that number
is less than the current indentation level, we'll produce a `Dedent` token, otherwise
we'll produce an `Indent` token. In either case we'll also update the lexer's
mode with the new indentation level for subsequent lines.

```gleam
fn indentation() -> Matcher(TokenT, Int) {
  let assert Ok(is_indent) = regex.from_string("^\\n[ \\t]*")
  use current_indent, lexeme, lookahead <- lexer.custom

  case regex.check(is_indent, lexeme), lookahead {
    False, _ -> NoMatch
    True, " " | True, "\t" -> Skip
    True, "\n" -> Drop(current_indent)
    True, _ -> {
      let spaces = string.length(lexeme) - 1

      case int.compare(spaces, current_indent) {
        Lt -> Keep(Dedent(spaces), spaces)
        Eq if spaces == 0 -> Drop(0)
        Eq -> Keep(Indent(spaces), spaces)
        Gt -> Keep(Indent(spaces), spaces)
      }
    }
  }
}
```

There's actually a little more going on here than I just described, so let's
break the pattern matching down case by case.

```gleam
False, _ -> NoMatch
```

- This branch should be self-explanatory. If the lexeme doesn't match the regex
  we have for indentation then we don't have a match and should tell the lexer to
  move on and attempt a different matcher.

```gleam
True, " " | True, "\t" -> Skip
```

- If the lexeme matches the indentation regex but we can see from the lookahead
  that it is followed by more indentation, then we don't want to produce a token
  right now. We _also_ don't want the lexer to try any other matchers that might
  consume these lexeme (like `lexer.whitespace`) so we tell the lexer to _skip_
  checking any other matchers and move on to the next lexeme.

```gleam
True, "\n" -> Drop(current_indent)
```

- When the lexeme matches the indentation regex but it's followed by a new line
  that means we have an empty line of just whitespace. You could choose to emit a
  token here if you wanted to, but in this case we'll ignore empty lines entirely
  and instruct the lexer to _drop_ the lexeme without producing anything. Importantly
  we preserve the current indentation level as the lexer's mode going forward.

```gleam
True, _ -> {
  let spaces = string.length(lexeme) - 1

  case int.compare(spaces, current_indent) {
    Lt -> Keep(Dedent(spaces), spaces)
    Eq if spaces == 0 -> Drop(0)
    Eq -> Keep(Indent(spaces), spaces)
    Gt -> Keep(Indent(spaces), spaces)
  }
}
```

- This is the actual indentation logic; we hit this branch when the lookahead is
  any other grapheme. To get the new indentation level we just need to count the
  length of the string, remembering to subtract one for the leading newline. By
  comparing the new indentation to the current, we can work out what token to keep.

  - If the new indentation is less than the current, we keep a `Dedent` token and
    update the lexer's mode to the new indentation level.
  - If the new indentation is is equal to the current _but_ that level is zero,
    then we drop the lexeme without producing any indentation token at all.
  - Otherwise, if the new indentation is equal or greater than the current, we
    keep an `Indent` token and update the lexer's mode to the new indentation level.

## String Interpolation
