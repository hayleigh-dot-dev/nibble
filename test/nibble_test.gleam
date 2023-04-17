import gleeunit
import gleam/result

pub fn main() {
  gleeunit.main()
}

type Error {
  NotANumber
  TooLow
  TooHigh
}

fn parse_number(input: String) -> Result(Int, Error) {
  todo
}

fn check_bounds(input: Int, lower: Int, upper: Int) -> Result(Int, Error) {
  todo
}

fn test() {
  "1"
  |> parse_number
  |> result.map(check_bounds(_, 1, 10))
}
