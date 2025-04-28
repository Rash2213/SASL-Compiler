package lexer

class LexerSpec extends munit.FunSuite {
  test("parses all words correctly") {
    val words = " def" +
      " if" +
      " then" +
      " else" +
      " where" +
      ":" +
      "(" +
      ")" +
      "[" +
      "]" +
      ";" +
      "." +
      "," +
      " some_id_222_" +
      " 123" +
      " true" +
      "\"som222thing\"" +
      "nil" +
      "+" +
      "-" +
      " not" +
      "*" +
      "/" +
      "=" +
      "~=" +
      "<" +
      ">" +
      "<=" +
      ">=" +
      " and" +
      " or"

    val lexer = Lexer(words.getBytes())
    import Token.*
    assertEquals(lexer.next(), KDef)
    assertEquals(lexer.next(), KIf)
    assertEquals(lexer.next(), KThen)
    assertEquals(lexer.next(), KElse)
    assertEquals(lexer.next(), KWhere)
    assertEquals(lexer.next(), KColon)
    assertEquals(lexer.next(), KOpenParen)
    assertEquals(lexer.next(), KCloseParen)
    assertEquals(lexer.next(), KOpenBracket)
    assertEquals(lexer.next(), KCloseBracket)
    assertEquals(lexer.next(), KSemicolon)
    assertEquals(lexer.next(), KDot)
    assertEquals(lexer.next(), KComma)
    assertEquals(lexer.next(), Id("some_id_222_"))
    assertEquals(lexer.next(), CNum(123))
    assertEquals(lexer.next(), CBool(true))
    assertEquals(lexer.next(), CString("som222thing"))
    assertEquals(lexer.next(), CNil)
    assertEquals(lexer.next(), SPlus)
    assertEquals(lexer.next(), SMinus)
    assertEquals(lexer.next(), SNot)
    assertEquals(lexer.next(), SMul)
    assertEquals(lexer.next(), SDiv)
    assertEquals(lexer.next(), SEqual)
    assertEquals(lexer.next(), SNotEqual)
    assertEquals(lexer.next(), SLess)
    assertEquals(lexer.next(), SGreater)
    assertEquals(lexer.next(), SLessEqual)
    assertEquals(lexer.next(), SGreaterEqual)
    assertEquals(lexer.next(), SAnd)
    assertEquals(lexer.next(), SOr)
    assertEquals(lexer.hasNext, false)
  }

  test("prefers longest match") {
    val id = "anditgoeson"
    val lexer = Lexer(id.getBytes())
    import Token.*
    assertEquals(lexer.next(), Id("anditgoeson"))
    assertEquals(lexer.hasNext, false)
  }

  test("treats whitespace and comments correctly") {
    val white = " def\n\f\t" +
      "\t|| my comment and or if then else\n" +
      " if" +
      "|| my multiline comment\n" +
      "|| goes on\n" +
      " then || also handles inline comments\n" +
      " else" +
      " where"
    val lexer = Lexer(white.getBytes())
    import Token.*
    assertEquals(lexer.next(), KDef)
    assertEquals(lexer.next(), KIf)
    assertEquals(lexer.next(), KThen)
    assertEquals(lexer.next(), KElse)
    assertEquals(lexer.next(), KWhere)
    assertEquals(lexer.hasNext, false)
  }
}
