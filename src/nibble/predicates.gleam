import gleam/list
import gleam/string

pub fn string(str: String, predicate: fn(String) -> Bool) -> Bool {
  str != "" && list.all(string.to_graphemes(str), predicate)
}

pub fn is_lower_ascii(grapheme: String) -> Bool {
  case grapheme {
    "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" -> True
    "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" -> True
    "w" | "x" | "y" | "z" -> True
    _ -> False
  }
}

pub fn is_upper_ascii(grapheme: String) -> Bool {
  case grapheme {
    "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" -> True
    "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" -> True
    "W" | "X" | "Y" | "Z" -> True
    _ -> False
  }
}

pub fn is_digit(grapheme: String) -> Bool {
  case grapheme {
    "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" -> True
    _ -> False
  }
}

pub fn is_whitespace(grapheme: String) -> Bool {
  case grapheme {
    " " | "\t" | "\r" | "\n" | "\f" | "\r\n" -> True
    _ -> False
  }
}
