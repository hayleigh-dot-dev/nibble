// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/int
import gleam/function
import gleeunit/should
import nibble.{type Parser}
import nibble/lexer.{type Lexer}

// TYPES -----------------------------------------------------------------------

type Json {
  Array(List(Json))
  False
  Null
  Number(Float)
  Object(List(#(String, Json)))
  String(String)
  True
}

type JsonT {
  Colon
  Comma
  FalseT
  LBrace
  LBracket
  NullT
  NumT(Float)
  RBrace
  RBracket
  StrT(String)
  TrueT
}

type Context {
  InArray
  InObject
}

// LITERAL TESTS ---------------------------------------------------------------

pub fn json_null_test() {
  use run <- should("parse a JSON null")
  let input = "null"
  let expected = Null

  run(input, expected)
}

pub fn json_true_test() {
  use run <- should("parse a JSON true")
  let input = "true"
  let expected = True

  run(input, expected)
}

pub fn json_false_test() {
  use run <- should("parse a JSON false")
  let input = "false"
  let expected = False

  run(input, expected)
}

pub fn json_number_test() {
  use run <- should("parse a JSON number")
  let input = "123.456"
  let expected = Number(123.456)

  run(input, expected)
}

pub fn json_string_test() {
  use run <- should("parse a JSON string")
  let input = "\"hello world\""
  let expected = String("hello world")

  run(input, expected)
}

// ARRAY TESTS -----------------------------------------------------------------

pub fn json_empty_array_test() {
  use run <- should("parse an empty JSON array")
  let input = "[]"
  let expected = Array([])

  run(input, expected)
}

pub fn json_singleton_array_test() {
  use run <- should("parse a JSON array with one element")
  let input = "[1]"
  let expected = Array([Number(1.0)])

  run(input, expected)
}

pub fn json_array_test() {
  use run <- should("parse a JSON array with multiple elements")
  let input = "[1, 2, 3]"
  let expected = Array([Number(1.0), Number(2.0), Number(3.0)])

  run(input, expected)
}

pub fn json_nested_array_test() {
  use run <- should("parse a nested JSON array")
  let input = "[1, [2, 3], 4]"
  let expected =
    Array([Number(1.0), Array([Number(2.0), Number(3.0)]), Number(4.0)])

  run(input, expected)
}

// OBJECT TESTS ----------------------------------------------------------------

pub fn json_empty_object_test() {
  use run <- should("parse an empty JSON object")
  let input = "{}"
  let expected = Object([])

  run(input, expected)
}

pub fn json_singleton_object_test() {
  use run <- should("parse a JSON object with one element")
  let input = "{\"a\": 1}"
  let expected = Object([#("a", Number(1.0))])

  run(input, expected)
}

pub fn json_object_test() {
  use run <- should("parse a JSON object with multiple elements")
  let input = "{\"a\": 1, \"b\": 2, \"c\": 3}"
  let expected =
    Object([#("a", Number(1.0)), #("b", Number(2.0)), #("c", Number(3.0))])

  run(input, expected)
}

pub fn json_nested_object_test() {
  use run <- should("parse a nested JSON object")
  let input = "{\"a\": 1, \"b\": {\"c\": 2}, \"d\": 3}"
  let expected =
    Object([
      #("a", Number(1.0)),
      #("b", Object([#("c", Number(2.0))])),
      #("d", Number(3.0)),
    ])

  run(input, expected)
}

// REAL WORLD TESTS ------------------------------------------------------------

pub fn json_schema_basic_test() {
  // https://json-schema.org/learn/miscellaneous-examples.html#basic
  use run <- should("parse the JSON Schema basic example")
  let input =
    "{
  \"$id\": \"https://example.com/person.schema.json\",
  \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",
  \"title\": \"Person\",
  \"type\": \"object\",
  \"properties\": {
    \"firstName\": {
      \"type\": \"string\",
      \"description\": \"The person's first name.\"
    },
    \"lastName\": {
      \"type\": \"string\",
      \"description\": \"The person's last name.\"
    },
    \"age\": {
      \"description\": \"Age in years which must be equal to or greater than zero.\",
      \"type\": \"integer\",
      \"minimum\": 0
    }
  }
}"
  let expected =
    Object([
      #("$id", String("https://example.com/person.schema.json")),
      #("$schema", String("https://json-schema.org/draft/2020-12/schema")),
      #("title", String("Person")),
      #("type", String("object")),
      #(
        "properties",
        Object([
          #(
            "firstName",
            Object([
              #("type", String("string")),
              #("description", String("The person's first name.")),
            ]),
          ),
          #(
            "lastName",
            Object([
              #("type", String("string")),
              #("description", String("The person's last name.")),
            ]),
          ),
          #(
            "age",
            Object([
              #(
                "description",
                String(
                  "Age in years which must be equal to or greater than zero.",
                ),
              ),
              #("type", String("integer")),
              #("minimum", Number(0.0)),
            ]),
          ),
        ]),
      ),
    ])

  run(input, expected)
}

pub fn json_scheme_arrays_of_things_test() {
  // https://json-schema.org/learn/miscellaneous-examples.html#arrays-of-things
  use run <- should("parse the JSON Schema 'arrays of things' example")
  let input =
    "{
  \"$id\": \"https://example.com/arrays.schema.json\",
  \"$schema\": \"https://json-schema.org/draft/2020-12/schema\",
  \"description\": \"A representation of a person, company, organization, or place\",
  \"type\": \"object\",
  \"properties\": {
    \"fruits\": {
      \"type\": \"array\",
      \"items\": {
        \"type\": \"string\"
      }
    },
    \"vegetables\": {
      \"type\": \"array\",
      \"items\": { \"$ref\": \"#/$defs/veggie\" }
    }
  },
  \"$defs\": {
    \"veggie\": {
      \"type\": \"object\",
      \"required\": [ \"veggieName\", \"veggieLike\" ],
      \"properties\": {
        \"veggieName\": {
          \"type\": \"string\",
          \"description\": \"The name of the vegetable.\"
        },
        \"veggieLike\": {
          \"type\": \"boolean\",
          \"description\": \"Do I like this vegetable?\"
        }
      }
    }
  }
}"
  let expected =
    Object([
      #("$id", String("https://example.com/arrays.schema.json")),
      #("$schema", String("https://json-schema.org/draft/2020-12/schema")),
      #(
        "description",
        String("A representation of a person, company, organization, or place"),
      ),
      #("type", String("object")),
      #(
        "properties",
        Object([
          #(
            "fruits",
            Object([
              #("type", String("array")),
              #("items", Object([#("type", String("string"))])),
            ]),
          ),
          #(
            "vegetables",
            Object([
              #("type", String("array")),
              #("items", Object([#("$ref", String("#/$defs/veggie"))])),
            ]),
          ),
        ]),
      ),
      #(
        "$defs",
        Object([
          #(
            "veggie",
            Object([
              #("type", String("object")),
              #("required", Array([String("veggieName"), String("veggieLike")])),
              #(
                "properties",
                Object([
                  #(
                    "veggieName",
                    Object([
                      #("type", String("string")),
                      #("description", String("The name of the vegetable.")),
                    ]),
                  ),
                  #(
                    "veggieLike",
                    Object([
                      #("type", String("boolean")),
                      #("description", String("Do I like this vegetable?")),
                    ]),
                  ),
                ]),
              ),
            ]),
          ),
        ]),
      ),
    ])

  run(input, expected)
}

// UTILS -----------------------------------------------------------------------

fn should(description: String, run: fn(fn(String, Json) -> Nil) -> Nil) -> Nil {
  use input, expected <- run

  io.print("should " <> description)

  lexer.run(input, lexer())
  |> should.be_ok
  |> nibble.run(parser())
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}

fn lexer() -> Lexer(JsonT, Nil) {
  lexer.simple([
    lexer.number(function.compose(int.to_float, NumT), NumT),
    lexer.token(":", Colon),
    lexer.token(",", Comma),
    lexer.token("false", FalseT),
    lexer.token("{", LBrace),
    lexer.token("[", LBracket),
    lexer.token("null", NullT),
    lexer.token("true", TrueT),
    lexer.token("}", RBrace),
    lexer.token("]", RBracket),
    lexer.string("\"", StrT),
    //
    lexer.whitespace(Nil)
    |> lexer.ignore,
  ])
}

fn parser() -> Parser(Json, JsonT, Context) {
  nibble.one_of([
    // Structures
    array_parser()
    |> nibble.in(InArray),
    object_parser()
    |> nibble.in(InObject),
    literal_parser(),
  ])
}

fn array_parser() -> Parser(Json, JsonT, Context) {
  use _ <- nibble.do(nibble.token(LBracket))
  use elements <- nibble.do(nibble.sequence(
    nibble.lazy(parser),
    nibble.token(Comma),
  ))
  use _ <- nibble.do(nibble.token(RBracket))

  nibble.return(Array(elements))
}

fn object_parser() -> Parser(Json, JsonT, Context) {
  use _ <- nibble.do(nibble.token(LBrace))
  use elements <- nibble.do(nibble.sequence(
    nibble.lazy(object_element_parser),
    nibble.token(Comma),
  ))
  use _ <- nibble.do(nibble.token(RBrace))

  nibble.return(Object(elements))
}

fn object_element_parser() -> Parser(#(String, Json), JsonT, Context) {
  use key <- nibble.do(
    nibble.backtrackable({
      use t <- nibble.do(nibble.any())

      case t {
        StrT(s) -> nibble.return(s)
        _ -> nibble.fail("Expected string object key")
      }
    }),
  )
  use _ <- nibble.do(nibble.token(Colon))
  use value <- nibble.do(nibble.lazy(parser))

  nibble.return(#(key, value))
}

fn literal_parser() -> Parser(Json, JsonT, Context) {
  nibble.backtrackable({
    use t <- nibble.do(nibble.any())

    case t {
      NumT(n) -> nibble.return(Number(n))
      StrT(s) -> nibble.return(String(s))
      TrueT -> nibble.return(True)
      FalseT -> nibble.return(False)
      NullT -> nibble.return(Null)
      _ -> nibble.fail("Expected a literal value")
    }
  })
}
