package parser

import lexer.TestLexer

class ParserSpec extends munit.FunSuite {
  test("syntax checking works") {
    enum Token:
      case Plus
      case Minus
      case Mul
      case Div
      case Num(num: Int)

    enum NonTerminal:
      case Start
      case AddP
      case MulP
      case Factor

    val em = Array(false, true, true, false)
    val fm = Array(true, false, false, false)

    import GrammarSymbolGeneric.*
    import Token.*
    import NonTerminal.*

    val gen = ParserGenerator[Token, NonTerminal]()

    val dm: gen.DerivationsMap = Array(
      // Start
      Array[gen.Derivation](
        Array(NT(Factor), NT(MulP), NT(AddP))
      ),
      // AddP
      Array[gen.Derivation](
        Array(T(Plus), NT(Factor), NT(MulP), NT(AddP)),
        Array(T(Minus), NT(Factor), NT(MulP), NT(AddP))
      ),
      // MulP
      Array[gen.Derivation](
        Array(T(Mul), NT(Factor), NT(MulP)),
        Array(T(Div), NT(Factor), NT(MulP)),
      ),
      // Factor
      Array[gen.Derivation](
        Array(T(Plus), T(Num(0))),
        Array(T(Minus), T(Num(0))),
        Array(T(Num(0))),
      ),
    )

    val empty: gen.Derivation = Array()

    val tbl: gen.ParseTable = Array(
      // inner: Plus, Minus, Mul, Div, Num
      // Start
      Array(Some(dm(0)(0)), Some(dm(0)(0)), None, None, Some(dm(0)(0))),
      // AddP
      Array(Some(dm(1)(0)), Some(dm(1)(1)), None, None, None),
      // MulP
      Array(Some(empty), Some(empty), Some(dm(2)(0)), Some(dm(2)(1)), None),
      // Factor
      Array(Some(dm(3)(0)), Some(dm(3)(1)), None, None, Some(dm(3)(2))),
    )

    val l1 = TestLexer[Token](Array(
      Plus,
      Num(0),
      Plus,
      Minus,
      Num(0),
      Mul,
      Plus,
      Num(0)
    ))

    val parser = Parser[Token, NonTerminal]()

    val res = parser.check(l1, NT(Start), tbl, em)
    assert(res)

    val lerr = TestLexer[Token](Array(
      Plus,
      Num(0),
      Minus,
      Num(0),
      Mul
    ))

    val resErr = parser.check(lerr, NT(Start), tbl, em)
    assert(!resErr)
  }

  test("examples syntax is correct") {
    import lexer.{Token, Lexer}
    import SaslData.{NonTerminal, derMap, emMap}
    import cli.readFileBin

    val gen = ParserGenerator[Token, NonTerminal]()
    val tbl = gen.oracleTable(Token.SOr.ordinal + 1, NonTerminal.values.length, derMap, emMap)
    val parser = Parser[Token, NonTerminal]()

    val lFib = Lexer(readFileBin("./examples/fibonacci.sasl"))
    assert(parser.check(lFib, GrammarSymbolGeneric.NT(NonTerminal.System), tbl, emMap))
    val lQsort = Lexer(readFileBin("./examples/qsort.sasl"))
    assert(parser.check(lQsort, GrammarSymbolGeneric.NT(NonTerminal.System), tbl, emMap))
    val lSieve = Lexer(readFileBin("./examples/sieve.sasl"))
    assert(parser.check(lSieve, GrammarSymbolGeneric.NT(NonTerminal.System), tbl, emMap))
  }
}