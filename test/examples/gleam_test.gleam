// IMPORTS ---------------------------------------------------------------------

import gleam/io
import gleam/queue
import gleam/option.{None, Option, Some}
import gleam/set
import gleam/string
import gleeunit/should
import nibble.{Parser, do, return}
import nibble/lexer.{Span, Token}

// TYPES: TOKENS ---------------------------------------------------------------

type GleamT {
  NameT(String)
  UpNameT(String)
  DiscardNameT(String)
  IntT(Int)
  FloatT(Float)
  StringT(String)
  // Groupings
  LParenT
  RParenT
  LSquareT
  RSquareT
  LBraceT
  RBraceT
  // IntT Operators
  PlusT
  MinusT
  StarT
  SlashT
  LessT
  GreaterT
  LessEqual
  GreaterEqual
  PercentT
  // FloatT Operators
  PlusDot
  MinusDot
  StarDot
  SlashDot
  LessDot
  GreaterDot
  LessEqualDot
  GreaterEqualDot
  // StringT Operators
  LtGtT
  // Other Punctuation
  ColonT
  CommaT
  HashT
  BangT
  EqualT
  EqualEqual
  NotEqualT
  VBarT
  VBarVBar
  AmperAmperT
  LtLtT
  GtGtT
  PipeT
  DotT
  RArrowT
  LArrowT
  DotDot
  // Extra
  CommentDocT(String)
  CommentNormalT(String)
  CommentModuleT(String)
  // Keywords (alphabetically):
  AsT
  Assert
  CaseT
  ConstT
  ExternalT
  FnT
  IfT
  ImportT
  LetT
  OpaqueT
  PanicT
  PubT
  TodoT
  TypeT
  UseT
}

// TYPES: AST ------------------------------------------------------------------

type Argument(a) =
  #(Option(String), String, a)

type Declaration {
  DAlias(docs: String, public: Bool, name: String, args: List(String), t: Type)
  DType(
    docs: String,
    public: Bool,
    exposed: Bool,
    name: String,
    args: List(String),
    variants: List(Variant),
  )
  DExtType(docs: String, public: Bool, name: String, args: List(String))
  DImport(
    docs: String,
    path: List(String),
    name: String,
    alias: Option(String),
    exposing: List(ImportedBinding),
  )
}

type Variant {
  Variant(docs: String, name: String, args: List(Argument(Type)))
}

type ImportedBinding {
  IType(String, Option(String))
  IVar(String, Option(String))
}

type Type {
  TConstructor(Option(String), String, List(Type))
  TFn(List(Type), Type)
  TVar(String)
  TTup(List(Type))
  THole(String)
}

// LEXER TESTS -----------------------------------------------------------------

pub fn hello_world_lexer_test() {
  use test <- should_lex("the Gleam hello world example")
  let input =
    "
import gleam/io

pub fn main() -> Nil {
  io.println(\"hello, friend!\")
}"
  let expected = [
    Token(Span(2, 1, 2, 7), "import", ImportT),
    Token(Span(2, 8, 2, 13), "gleam", NameT("gleam")),
    Token(Span(2, 13, 2, 14), "/", SlashT),
    Token(Span(2, 14, 2, 16), "io", NameT("io")),
    //
    Token(Span(4, 1, 4, 4), "pub", PubT),
    Token(Span(4, 5, 4, 7), "fn", FnT),
    Token(Span(4, 8, 4, 12), "main", NameT("main")),
    Token(Span(4, 12, 4, 13), "(", LParenT),
    Token(Span(4, 13, 4, 14), ")", RParenT),
    Token(Span(4, 15, 4, 17), "->", RArrowT),
    Token(Span(4, 18, 4, 21), "Nil", UpNameT("Nil")),
    Token(Span(4, 22, 4, 23), "{", LBraceT),
    //
    Token(Span(5, 3, 5, 5), "io", NameT("io")),
    Token(Span(5, 5, 5, 6), ".", DotT),
    Token(Span(5, 6, 5, 13), "println", NameT("println")),
    Token(Span(5, 13, 5, 14), "(", LParenT),
    Token(Span(5, 14, 5, 30), "\"hello, friend!\"", StringT("hello, friend!")),
    Token(Span(5, 30, 5, 31), ")", RParenT),
    //
    Token(Span(6, 1, 6, 2), "}", RBraceT),
  ]

  test(input, expected)
}

pub fn reliable_and_scalable_lexer_test() {
  use test <- should_lex("the Gleam 'reliable and scalable' example")
  let input =
    "
fn spawn_task(i) {
  task.async(fn() {
    let n = int.to_string(i)
    io.println(\"Hello from \" <> n)
  })
}

pub fn main() {
  // Run a million threads, no problem
  list.range(0, 1_000_000.0)
  |> list.map(spawn_task)
  |> list.each(task.await_forever)
}"
  let expected = [
    Token(Span(2, 1, 2, 3), "fn", FnT),
    Token(Span(2, 4, 2, 14), "spawn_task", NameT("spawn_task")),
    Token(Span(2, 14, 2, 15), "(", LParenT),
    Token(Span(2, 15, 2, 16), "i", NameT("i")),
    Token(Span(2, 16, 2, 17), ")", RParenT),
    Token(Span(2, 18, 2, 19), "{", LBraceT),
    Token(Span(3, 3, 3, 7), "task", NameT("task")),
    Token(Span(3, 7, 3, 8), ".", DotT),
    Token(Span(3, 8, 3, 13), "async", NameT("async")),
    Token(Span(3, 13, 3, 14), "(", LParenT),
    Token(Span(3, 14, 3, 16), "fn", FnT),
    Token(Span(3, 16, 3, 17), "(", LParenT),
    Token(Span(3, 17, 3, 18), ")", RParenT),
    Token(Span(3, 19, 3, 20), "{", LBraceT),
    Token(Span(4, 5, 4, 8), "let", LetT),
    Token(Span(4, 9, 4, 10), "n", NameT("n")),
    Token(Span(4, 11, 4, 12), "=", EqualT),
    Token(Span(4, 13, 4, 16), "int", NameT("int")),
    Token(Span(4, 16, 4, 17), ".", DotT),
    Token(Span(4, 17, 4, 26), "to_string", NameT("to_string")),
    Token(Span(4, 26, 4, 27), "(", LParenT),
    Token(Span(4, 27, 4, 28), "i", NameT("i")),
    Token(Span(4, 28, 4, 29), ")", RParenT),
    Token(Span(5, 5, 5, 7), "io", NameT("io")),
    Token(Span(5, 7, 5, 8), ".", DotT),
    Token(Span(5, 8, 5, 15), "println", NameT("println")),
    Token(Span(5, 15, 5, 16), "(", LParenT),
    Token(Span(5, 16, 5, 29), "\"Hello from \"", StringT("Hello from ")),
    Token(Span(5, 30, 5, 32), "<>", LtGtT),
    Token(Span(5, 33, 5, 34), "n", NameT("n")),
    Token(Span(5, 34, 5, 35), ")", RParenT),
    Token(Span(6, 3, 6, 4), "}", RBraceT),
    Token(Span(6, 4, 6, 5), ")", RParenT),
    Token(Span(7, 1, 7, 2), "}", RBraceT),
    Token(Span(9, 1, 9, 4), "pub", PubT),
    Token(Span(9, 5, 9, 7), "fn", FnT),
    Token(Span(9, 8, 9, 12), "main", NameT("main")),
    Token(Span(9, 12, 9, 13), "(", LParenT),
    Token(Span(9, 13, 9, 14), ")", RParenT),
    Token(Span(9, 15, 9, 16), "{", LBraceT),
    Token(
      Span(10, 3, 10, 39),
      "// Run a million threads, no problem",
      CommentNormalT(" Run a million threads, no problem"),
    ),
    Token(Span(11, 3, 11, 7), "list", NameT("list")),
    Token(Span(11, 7, 11, 8), ".", DotT),
    Token(Span(11, 8, 11, 13), "range", NameT("range")),
    Token(Span(11, 13, 11, 14), "(", LParenT),
    Token(Span(11, 14, 11, 15), "0", IntT(0)),
    Token(Span(11, 15, 11, 16), ",", CommaT),
    Token(Span(11, 17, 11, 28), "1_000_000.0", FloatT(1_000_000.0)),
    Token(Span(11, 28, 11, 29), ")", RParenT),
    Token(Span(12, 3, 12, 5), "|>", PipeT),
    Token(Span(12, 6, 12, 10), "list", NameT("list")),
    Token(Span(12, 10, 12, 11), ".", DotT),
    Token(Span(12, 11, 12, 14), "map", NameT("map")),
    Token(Span(12, 14, 12, 15), "(", LParenT),
    Token(Span(12, 15, 12, 25), "spawn_task", NameT("spawn_task")),
    Token(Span(12, 25, 12, 26), ")", RParenT),
    Token(Span(13, 3, 13, 5), "|>", PipeT),
    Token(Span(13, 6, 13, 10), "list", NameT("list")),
    Token(Span(13, 10, 13, 11), ".", DotT),
    Token(Span(13, 11, 13, 15), "each", NameT("each")),
    Token(Span(13, 15, 13, 16), "(", LParenT),
    Token(Span(13, 16, 13, 20), "task", NameT("task")),
    Token(Span(13, 20, 13, 21), ".", DotT),
    Token(Span(13, 21, 13, 34), "await_forever", NameT("await_forever")),
    Token(Span(13, 34, 13, 35), ")", RParenT),
    Token(Span(14, 1, 14, 2), "}", RBraceT),
  ]

  test(input, expected)
}

pub fn mulitlingual_lexer_test() {
  use test <- should_lex("the Gleam 'multilingual' example")
  let input =
    "
external fn as_html(
  markdown: String,
  options: Dynamic,
) -> EarmarkResult =
  \"Elixir.Earmark\" \"as_html\"


pub fn register_event_handler() {
  let el = document.query_selector(\"a\")
  element.add_event_listener(el, fn() {
    io.println(\"Clicked!\")
  })
}"
  let expected = [
    Token(Span(2, 1, 2, 9), "external", ExternalT),
    Token(Span(2, 10, 2, 12), "fn", FnT),
    Token(Span(2, 13, 2, 20), "as_html", NameT("as_html")),
    Token(Span(2, 20, 2, 21), "(", LParenT),
    //
    Token(Span(3, 3, 3, 11), "markdown", NameT("markdown")),
    Token(Span(3, 11, 3, 12), ":", ColonT),
    Token(Span(3, 13, 3, 19), "String", UpNameT("String")),
    Token(Span(3, 19, 3, 20), ",", CommaT),
    //
    Token(Span(4, 3, 4, 10), "options", NameT("options")),
    Token(Span(4, 10, 4, 11), ":", ColonT),
    Token(Span(4, 12, 4, 19), "Dynamic", UpNameT("Dynamic")),
    Token(Span(4, 19, 4, 20), ",", CommaT),
    //
    Token(Span(5, 1, 5, 2), ")", RParenT),
    Token(Span(5, 3, 5, 5), "->", RArrowT),
    Token(Span(5, 6, 5, 19), "EarmarkResult", UpNameT("EarmarkResult")),
    Token(Span(5, 20, 5, 21), "=", EqualT),
    //
    Token(Span(6, 3, 6, 19), "\"Elixir.Earmark\"", StringT("Elixir.Earmark")),
    Token(Span(6, 20, 6, 29), "\"as_html\"", StringT("as_html")),
    //
    Token(Span(9, 1, 9, 4), "pub", PubT),
    Token(Span(9, 5, 9, 7), "fn", FnT),
    Token(
      Span(9, 8, 9, 30),
      "register_event_handler",
      NameT("register_event_handler"),
    ),
    Token(Span(9, 30, 9, 31), "(", LParenT),
    Token(Span(9, 31, 9, 32), ")", RParenT),
    Token(Span(9, 33, 9, 34), "{", LBraceT),
    //
    Token(Span(10, 3, 10, 6), "let", LetT),
    Token(Span(10, 7, 10, 9), "el", NameT("el")),
    Token(Span(10, 10, 10, 11), "=", EqualT),
    Token(Span(10, 12, 10, 20), "document", NameT("document")),
    Token(Span(10, 20, 10, 21), ".", DotT),
    Token(Span(10, 21, 10, 35), "query_selector", NameT("query_selector")),
    Token(Span(10, 35, 10, 36), "(", LParenT),
    Token(Span(10, 36, 10, 39), "\"a\"", StringT("a")),
    Token(Span(10, 39, 10, 40), ")", RParenT),
    //
    Token(Span(11, 3, 11, 10), "element", NameT("element")),
    Token(Span(11, 10, 11, 11), ".", DotT),
    Token(
      Span(11, 11, 11, 29),
      "add_event_listener",
      NameT("add_event_listener"),
    ),
    Token(Span(11, 29, 11, 30), "(", LParenT),
    Token(Span(11, 30, 11, 32), "el", NameT("el")),
    Token(Span(11, 32, 11, 33), ",", CommaT),
    Token(Span(11, 34, 11, 36), "fn", FnT),
    Token(Span(11, 36, 11, 37), "(", LParenT),
    Token(Span(11, 37, 11, 38), ")", RParenT),
    Token(Span(11, 39, 11, 40), "{", LBraceT),
    //
    Token(Span(12, 5, 12, 7), "io", NameT("io")),
    Token(Span(12, 7, 12, 8), ".", DotT),
    Token(Span(12, 8, 12, 15), "println", NameT("println")),
    Token(Span(12, 15, 12, 16), "(", LParenT),
    Token(Span(12, 16, 12, 26), "\"Clicked!\"", StringT("Clicked!")),
    Token(Span(12, 26, 12, 27), ")", RParenT),
    //
    Token(Span(13, 3, 13, 4), "}", RBraceT),
    Token(Span(13, 4, 13, 5), ")", RParenT),
    //
    Token(Span(14, 1, 14, 2), "}", RBraceT),
  ]

  test(input, expected)
}

// TESTS: IMPORT PARSERS -------------------------------------------------------

pub fn import_parser_test() {
  use test <- should_parse("a simple import with no path, alias, or exposing")
  let input = "import gleam"
  let expected = DImport("", [], "gleam", None, [])

  test(input, expected, declaration_parser())
}

pub fn import_with_path_parser_test() {
  use test <- should_parse("a simple import with a path")
  let input = "import gleam/stdlib"
  let expected = DImport("", ["gleam"], "stdlib", None, [])

  test(input, expected, declaration_parser())
}

pub fn import_with_long_path_parser_test() {
  use test <- should_parse("a simple import with a long path")
  let input = "import gleam/javascript/promise"
  let expected = DImport("", ["gleam", "javascript"], "promise", None, [])

  test(input, expected, declaration_parser())
}

pub fn import_with_alias_test() {
  use test <- should_parse("a simple import with an alias")
  let input = "import gleam as g"
  let expected = DImport("", [], "gleam", Some("g"), [])

  test(input, expected, declaration_parser())
}

pub fn import_with_exposing_test() {
  use test <- should_parse("a simple import with exposing")
  let input = "import gleam/option.{Option, Some, None}"
  let expected =
    DImport(
      "",
      ["gleam"],
      "option",
      None,
      [IType("Option", None), IType("Some", None), IType("None", None)],
    )

  test(input, expected, declaration_parser())
}

pub fn complex_import_test() {
  use test <- should_parse("a complex import using all features")
  let input =
    "
    /// We're renaming the `option` module to `maybe` because Haskell
    import gleam/option.{Option as Maybe, map} as maybe"
  let expected =
    DImport(
      " We're renaming the `option` module to `maybe` because Haskell",
      ["gleam"],
      "option",
      Some("maybe"),
      [IType("Option", Some("Maybe")), IVar("map", None)],
    )

  test(input, expected, declaration_parser())
}

// TESTS: TYPE DECLARATION PARSERS ---------------------------------------------

pub fn external_type_test() {
  use test <- should_parse("a simple external type with no type parameters")
  let input = "external type Wibble"
  let expected = DExtType("", False, "Wibble", [])

  test(input, expected, declaration_parser())
}

pub fn public_external_type_test() {
  use test <- should_parse("a simple external type with no type parameters")
  let input = "pub external type Wibble"
  let expected = DExtType("", True, "Wibble", [])

  test(input, expected, declaration_parser())
}

pub fn external_type_with_parameters_test() {
  use test <- should_parse("a simple external type with type parameters")
  let input = "external type Wibble(a, b)"
  let expected = DExtType("", False, "Wibble", ["a", "b"])

  test(input, expected, declaration_parser())
}

pub fn complex_external_type_test() {
  use test <- should_parse("an external type with docs and type parameters")
  let input =
    "
    /// Louis really likes the name `wibble` for some reason
    pub external type Wibble(a, b)
  "
  let expected =
    DExtType(
      " Louis really likes the name `wibble` for some reason",
      True,
      "Wibble",
      ["a", "b"],
    )

  test(input, expected, declaration_parser())
}

// UTILS -----------------------------------------------------------------------

fn should_lex(
  description: String,
  run: fn(fn(String, List(Token(GleamT))) -> Nil) -> Nil,
) -> Nil {
  use input, expected <- run
  io.print("should lex " <> description)

  lexer.run(input, lexer())
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}

fn should_parse(
  description: String,
  run: fn(fn(String, a, Parser(a, GleamT, ctx)) -> Nil) -> Nil,
) -> Nil {
  use input, expected, parser <- run
  io.print("should parse " <> description)

  lexer.run(input, lexer())
  |> should.be_ok
  |> nibble.run(parser)
  |> should.be_ok
  |> should.equal(expected)

  io.println(" ✅")
}

// UTILS: LEXER ----------------------------------------------------------------

fn lexer() {
  let keywords =
    set.from_list([
      "as", "assert", "case", "const", "external", "fn", "if", "import", "let",
      "opaque", "panic", "pub", "todo", "type", "use",
    ])

  lexer.simple([
    lexer.identifier("[a-z]", "[a-z0-9_]", keywords, NameT),
    lexer.identifier("[A-Z]", "[a-zA-Z0-9_]", keywords, UpNameT),
    lexer.identifier("_", "[a-z0-9_]", keywords, DiscardNameT),
    lexer.number_with_separator("_", IntT, FloatT),
    lexer.string("\"", StringT),
    // Gour
    lexer.token("(", LParenT),
    lexer.token(")", RParenT),
    lexer.token("[", LSquareT),
    lexer.token("]", RSquareT),
    lexer.token("{", LBraceT),
    lexer.token("}", RBraceT),
    // IntT Operators
    lexer.symbol("+", "[^.]", PlusT),
    lexer.symbol("-", "[^.>]", MinusT),
    lexer.symbol("*", "[^.]", StarT),
    lexer.symbol("/", "[^./]", SlashT),
    lexer.symbol("<", "[^.<->=]", LessT),
    lexer.symbol(">", "[^.>=]", GreaterT),
    lexer.symbol("<=", "[^.]", LessEqual),
    lexer.symbol(">=", "[^.]", GreaterEqual),
    lexer.token("%", PercentT),
    // FloatT Operators
    lexer.token("+.", PlusDot),
    lexer.token("-.", MinusDot),
    lexer.token("*.", StarDot),
    lexer.token("/.", SlashDot),
    lexer.token("<.", LessDot),
    lexer.token(">.", GreaterDot),
    lexer.token("<=.", LessEqualDot),
    lexer.token(">=.", GreaterEqualDot),
    // StringT Operators
    lexer.token("<>", LtGtT),
    // Other Punctuation
    lexer.token(":", ColonT),
    lexer.token(",", CommaT),
    lexer.token("#", HashT),
    lexer.symbol("!", "[^=]", BangT),
    lexer.symbol("=", "[^=]", EqualT),
    lexer.token("==", EqualEqual),
    lexer.token("!=", NotEqualT),
    lexer.symbol("|", "[^|>]", VBarT),
    lexer.token("||", VBarVBar),
    lexer.token("&&", AmperAmperT),
    lexer.token("<<", LtLtT),
    lexer.token(">>", GtGtT),
    lexer.token("|>", PipeT),
    lexer.symbol(".", "[^.]", DotT),
    lexer.token("->", RArrowT),
    lexer.token("<-", LArrowT),
    lexer.token("..", DotDot),
    // Keywords
    lexer.keyword("as", "[\\W\\D]", AsT),
    lexer.keyword("assert", "[\\W\\D]", Assert),
    lexer.keyword("case", "[\\W\\D]", CaseT),
    lexer.keyword("const", "[\\W\\D]", ConstT),
    lexer.keyword("external", "[\\W\\D]", ExternalT),
    lexer.keyword("fn", "[\\W\\D]", FnT),
    lexer.keyword("if", "[\\W\\D]", IfT),
    lexer.keyword("import", "[\\W\\D]", ImportT),
    lexer.keyword("let", "[\\W\\D]", LetT),
    lexer.keyword("opaque", "[\\W\\D]", OpaqueT),
    lexer.keyword("panic", "[\\W\\D]", PanicT),
    lexer.keyword("pub", "[\\W\\D]", PubT),
    lexer.keyword("todo", "[\\W\\D]", TodoT),
    lexer.keyword("type", "[\\W\\D]", TypeT),
    lexer.keyword("use", "[\\W\\D]", UseT),
    // Comments
    lexer.comment("////", CommentModuleT),
    lexer.comment("///", CommentDocT),
    lexer.comment("//", CommentNormalT),
    // Whitespace
    lexer.whitespace(Nil)
    |> lexer.ignore,
  ])
}

// UTILS: PARSERS --------------------------------------------------------------

fn declaration_parser() {
  use docs <- do(nibble.many(doc_comment_parser()))
  let docs = string.join(docs, "\n")
  // All declarations except for imports may begin with the `pub` keyword. That
  // means if we parse that keyword we know we don't need to bother attempting
  // to parse an import declaration.
  use public <- do(public_parser())

  case public {
    True -> nibble.one_of([type_declaration_parser(True, docs)])
    False ->
      nibble.one_of([
        import_declaration_parser(docs),
        type_declaration_parser(False, docs),
      ])
  }
}

// UTILS: IMPORT DECLARATION PARSERS -------------------------------------------

fn import_declaration_parser(docs) {
  use _ <- do(nibble.token(ImportT))
  use path <- do(nibble.sequence(lower_name_parser(), nibble.token(SlashT)))
  use #(name, path) <- do({
    case queue.pop_back(queue.from_list(path)) {
      Ok(#(name, path)) -> return(#(name, queue.to_list(path)))
      //
      Error(_) -> nibble.fail("missing import path")
    }
  })
  use exposing <- do(nibble.one_of([imported_bindings_parser(), return([])]))
  use alias <- do(alias_parser(lower_name_parser()))

  return(DImport(docs, path, name, alias, exposing))
}

fn imported_bindings_parser() {
  use _ <- do(nibble.token(DotT))
  use _ <- do(nibble.token(LBraceT))
  use bindings <- do(nibble.sequence(
    imported_binding_parser(),
    nibble.token(CommaT),
  ))
  use _ <- do(nibble.token(RBraceT))

  return(bindings)
}

fn imported_binding_parser() {
  nibble.one_of([imported_type_parser(), imported_var_parser()])
}

fn imported_type_parser() {
  use name <- do(upper_name_parser())
  use alias <- do(alias_parser(upper_name_parser()))

  return(IType(name, alias))
}

fn imported_var_parser() {
  use name <- do(lower_name_parser())
  use alias <- do(alias_parser(lower_name_parser()))

  return(IVar(name, alias))
}

// UTILS: TYPE DECLARATION PARSERS ---------------------------------------------

fn type_declaration_parser(public, docs) {
  // Type declarations may be preceded by either the `external` keyword or the
  // `opaque` keyword. If we try to parse both now we can emit a more helpful
  // error message if they both exist, and also use the information to choose
  // which type of type declaration to parse (eg type aliases can neither be
  // external nor opaque).
  use is_external <- do(
    nibble.token(ExternalT)
    |> nibble.replace(with: True)
    |> nibble.or(False),
  )
  use is_opaque <- do(
    nibble.token(OpaqueT)
    |> nibble.replace(with: True)
    |> nibble.or(False),
  )
  use _ <- do(nibble.token(TypeT))
  use name <- do(upper_name_parser())
  use args <- do(
    type_declaration_arguments_parser()
    |> nibble.or([]),
  )

  case is_external, is_opaque {
    True, True -> nibble.fail("external types are always opaque")
    True, False -> return(DExtType(docs, public, name, args))
    False, True -> type_custom_parser(docs, public, True, name, args)
    False, False ->
      nibble.one_of([
        type_alias_parser(docs, public, name, args),
        type_custom_parser(docs, public, False, name, args),
      ])
  }
}

fn type_declaration_arguments_parser() {
  use _ <- do(nibble.token(LParenT))
  use args <- do(nibble.sequence(lower_name_parser(), nibble.token(CommaT)))
  use _ <- do(nibble.token(RParenT))

  return(args)
}

fn type_alias_parser(docs, public, name, args) {
  use _ <- do(nibble.token(EqualT))
  use t <- do(type_parser())

  return(DAlias(docs, public, name, args, t))
}

fn type_custom_parser(docs, public, exposed, name, args) {
  use _ <- do(nibble.token(LBraceT))
  use variants <- do(nibble.many(type_custom_variant_parser()))
  use _ <- do(nibble.token(RBraceT))

  case variants {
    [] -> nibble.fail("custom types must have at least one variant")
    _ -> return(DType(docs, public, exposed, name, args, variants))
  }
}

fn type_custom_variant_parser() {
  use docs <- do(nibble.many(doc_comment_parser()))
  let docs = string.join(docs, "\n")
  use name <- do(upper_name_parser())
  use args <- do(
    variant_args_parser()
    |> nibble.or([]),
  )
  return(Variant(docs, name, args))
}

fn variant_args_parser() {
  use _ <- do(nibble.token(LParenT))
  use args <- do(nibble.sequence(variant_arg_parser(), nibble.token(CommaT)))
  use _ <- do(nibble.token(RParenT))

  return(args)
}

fn variant_arg_parser() {
  use name_or_label <- do(lower_name_parser())
  use name <- do(nibble.optional(lower_name_parser()))
  use _ <- do(nibble.token(ColonT))
  use t <- do(type_parser())

  case name {
    Some(name) -> return(#(Some(name_or_label), name, t))
    None -> return(#(None, name_or_label, t))
  }
}

// UTILS: TYPE PARSER ----------------------------------------------------------

fn type_parser() {
  todo
}

fn tconstructor_parser() {
  todo
}

// UTILS: HELPER PARSERS -------------------------------------------------------

fn public_parser() {
  nibble.one_of([
    nibble.token(PubT)
    |> nibble.replace(True),
    return(False),
  ])
}

fn alias_parser(name_parser) {
  nibble.one_of([
    {
      use _ <- do(nibble.token(AsT))
      use alias <- do(name_parser)

      return(Some(alias))
    },
    return(None),
  ])
}

fn lower_name_parser() {
  use tok <- nibble.take_map("a lower name")

  case tok {
    NameT(name) -> Some(name)
    _ -> None
  }
}

fn upper_name_parser() {
  use tok <- nibble.take_map("an upper name")

  case tok {
    UpNameT(name) -> Some(name)
    _ -> None
  }
}

fn doc_comment_parser() {
  use tok <- nibble.take_map("a doc comment")

  case tok {
    CommentDocT(content) -> Some(content)
    _ -> None
  }
}
