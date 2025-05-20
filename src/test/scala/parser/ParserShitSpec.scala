package parser

import lexer.{TestLexer, Token}
import lexer.Token.*
import parser.ParseTree.*
import parser.SaslData.{NonTerminal, derMap, emMap}

import scala.collection.mutable

class ParserShitSpec extends munit.FunSuite {
  val gen: ParserGenerator[Token, NonTerminal] = ParserGenerator()
  val fr = gen.first(NonTerminal.values.length, derMap, emMap)
  val firstSet = fr._1

  def parseSuccess[A](result: Either[ParseError, A])(assertFn: A => Unit)(implicit loc: munit.Location): Unit = {
    result match {
      case Right(ast) => assertFn(ast)
      case Left(e) => fail(s"Parse failed: $e")
    }
  }

  test("simple non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lId = TestLexer(Array(Id("myVar")))
    val lNum = TestLexer(Array(CNum(12)))
    val lBool = TestLexer(Array(CBool(true)))
    val lNil = TestLexer(Array(CNil))

    parseSuccess(parserShitSimple(lId, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Ident("0myVar"))
    }
    parseSuccess(parserShitSimple(lNum, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Num(12)))
    }
    parseSuccess(parserShitSimple(lBool, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Bool(true)))
    }
    parseSuccess(parserShitSimple(lNil, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Nil))
    }
  }

  test("comb' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(CNum(12), CBool(true), CNil))

    parseSuccess(parserShitCombP(l, firstSet, varMap, scopes, 0, Ident("myVar"))) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Application(
              Ident("myVar"),
              Const(Constant.Num(12))
            ),
            Const(Constant.Bool(true))
          ),
          Const(Constant.Nil)
        )
      )
    }
  }

  test("factor non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(Id("myVar"), CNum(12), CBool(true), CNil))
    val lPlus = TestLexer(Array(SPlus, Id("myVar"), CNum(12), CBool(true), CNil))
    val lMinus = TestLexer(Array(SMinus, Id("myVar"), CNum(12), CBool(true), CNil))
    val lNot = TestLexer(Array(SNot, Id("myVar"), CNum(12), CBool(true), CNil))

    parseSuccess(parserShitFactor(l, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Application(
              Ident("0myVar"),
              Const(Constant.Num(12))
            ),
            Const(Constant.Bool(true))
          ),
          Const(Constant.Nil)
        )
      )
    }
    parseSuccess(parserShitFactor(lPlus, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Application(
              Ident("0myVar"),
              Const(Constant.Num(12))
            ),
            Const(Constant.Bool(true))
          ),
          Const(Constant.Nil)
        )
      )
    }
    parseSuccess(parserShitFactor(lMinus, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Ident("inv"),
          Application(
            Application(
              Application(
                Ident("0myVar"),
                Const(Constant.Num(12))
              ),
              Const(Constant.Bool(true))
            ),
            Const(Constant.Nil)
          )
        )
      )
    }
    parseSuccess(parserShitFactor(lNot, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Ident("not"),
          Application(
            Application(
              Application(
                Ident("0myVar"),
                Const(Constant.Num(12))
              ),
              Const(Constant.Bool(true))
            ),
            Const(Constant.Nil)
          )
        )
      )
    }
  }

  test("mul' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lMul = TestLexer(Array(SMul, CNum(2), SDiv, CNum(3)))
    val lDiv = TestLexer(Array(SDiv, CNum(3), SMul, CNum(2)))
    val lNone = TestLexer[Token](Array())

    parseSuccess(parserShitMulP(lMul, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("div"),
            Application(
              Application(
                Ident("mul"),
                Ident("x")
              ),
              Const(Constant.Num(2))
            )
          ),
          Const(Constant.Num(3))
        )
      )
    }

    parseSuccess(parserShitMulP(lDiv, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("mul"),
            Application(
              Application(
                Ident("div"),
                Ident("x")
              ),
              Const(Constant.Num(3))
            )
          ),
          Const(Constant.Num(2))
        )
      )
    }

    parseSuccess(parserShitMulP(lNone, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x"))
    }
  }

  test("add' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lPlusMinus = TestLexer(Array(SPlus, CNum(2), SMinus, CNum(3)))
    val lMinusPlus = TestLexer(Array(SMinus, CNum(3), SPlus, CNum(2)))
    val lNone = TestLexer[Token](Array())

    parseSuccess(parserShitAddP(lPlusMinus, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("minus"),
            Application(
              Application(
                Ident("plus"),
                Ident("x")
              ),
              Const(Constant.Num(2))
            )
          ),
          Const(Constant.Num(3))
        )
      )
    }

    parseSuccess(parserShitAddP(lMinusPlus, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("plus"),
            Application(
              Application(
                Ident("minus"),
                Ident("x")
              ),
              Const(Constant.Num(3))
            )
          ),
          Const(Constant.Num(2))
        )
      )
    }

    parseSuccess(parserShitAddP(lNone, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x"))
    }
  }

  test("compar' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lEq = TestLexer(Array(SEqual, CNum(2), SNotEqual, CNum(3)))
    val lNeq = TestLexer(Array(SNotEqual, CNum(3), SEqual, CNum(2)))
    val lLtGt = TestLexer(Array(SLess, CNum(1), SGreater, CNum(4)))
    val lGtLt = TestLexer(Array(SGreater, CNum(4), SLess, CNum(1)))
    val lLeqGeq = TestLexer(Array(SLessEqual, CNum(2), SGreaterEqual, CNum(3)))
    val lGeqLeq = TestLexer(Array(SGreaterEqual, CNum(3), SLessEqual, CNum(2)))
    val lNone = TestLexer[Token](Array())

    parseSuccess(parserShitComparP(lEq, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("neq"),
            Application(
              Application(
                Ident("eq"),
                Ident("x")
              ),
              Const(Constant.Num(2))
            )
          ),
          Const(Constant.Num(3))
        )
      )
    }

    parseSuccess(parserShitComparP(lNeq, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("eq"),
            Application(
              Application(
                Ident("neq"),
                Ident("x")
              ),
              Const(Constant.Num(3))
            )
          ),
          Const(Constant.Num(2))
        )
      )
    }

    parseSuccess(parserShitComparP(lLtGt, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("gt"),
            Application(
              Application(
                Ident("lt"),
                Ident("x")
              ),
              Const(Constant.Num(1))
            )
          ),
          Const(Constant.Num(4))
        )
      )
    }

    parseSuccess(parserShitComparP(lGtLt, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("lt"),
            Application(
              Application(
                Ident("gt"),
                Ident("x")
              ),
              Const(Constant.Num(4))
            )
          ),
          Const(Constant.Num(1))
        )
      )
    }

    parseSuccess(parserShitComparP(lLeqGeq, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("geq"),
            Application(
              Application(
                Ident("leq"),
                Ident("x")
              ),
              Const(Constant.Num(2))
            )
          ),
          Const(Constant.Num(3))
        )
      )
    }

    parseSuccess(parserShitComparP(lGeqLeq, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("leq"),
            Application(
              Application(
                Ident("geq"),
                Ident("x")
              ),
              Const(Constant.Num(3))
            )
          ),
          Const(Constant.Num(2))
        )
      )
    }

    parseSuccess(parserShitComparP(lNone, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x"))
    }
  }

  test("conjunct' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lAnd = TestLexer(Array(SAnd, CBool(true), SAnd, CBool(false)))
    val lNone = TestLexer[Token](Array())

    parseSuccess(parserShitConjunctP(lAnd, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("and"),
            Application(
              Application(
                Ident("and"),
                Ident("x")
              ),
              Const(Constant.Bool(true))
            )
          ),
          Const(Constant.Bool(false))
        )
      )
    }

    parseSuccess(parserShitConjunctP(lNone, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x"))
    }
  }

  test("opexpr' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lOr = TestLexer(Array(SOr, CBool(true), SOr, CBool(false)))
    val lNone = TestLexer[Token](Array())

    parseSuccess(parserShitOpExprP(lOr, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("or"),
            Application(
              Application(
                Ident("or"),
                Ident("x")
              ),
              Const(Constant.Bool(true))
            )
          ),
          Const(Constant.Bool(false))
        )
      )
    }

    parseSuccess(parserShitOpExprP(lNone, firstSet, varMap, Ident("x"), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x"))
    }
  }


  test("listexpr non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lSingleItemList = TestLexer(Array(CNum(1)))
    val lMultipleItems = TestLexer(Array(CNum(1), KColon, CBool(true), KColon, CNil))

    parseSuccess(parserShitListExpr(lSingleItemList, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Num(1)))
    }

    parseSuccess(parserShitListExpr(lMultipleItems, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("cons"),
            Const(Constant.Num(1))
          ),
          Application(
            Application(
              Ident("cons"),
              Const(Constant.Bool(true))
            ),
            Const(Constant.Nil)
          )
        )
      )
    }
  }

  test("condexpr non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lMultipleItems = TestLexer(Array(CNum(1), KColon, CBool(true), KColon, CNil))
    val lCondition = TestLexer(Array(KIf, CBool(true), KThen, CBool(false), KElse, CBool(true)))

    parseSuccess(parserShitCondExpr(lMultipleItems, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("cons"),
            Const(Constant.Num(1))
          ),
          Application(
            Application(
              Ident("cons"),
              Const(Constant.Bool(true))
            ),
            Const(Constant.Nil)
          )
        )
      )
    }

    parseSuccess(parserShitCondExpr(lCondition, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Application(
              Ident("cond"),
              Const(Constant.Bool(true))
            ),
            Const(Constant.Bool(false))
          ),
          Const(Constant.Bool(true))
        )
      )
    }
  }

  test("abstraction non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(Id("x"), Id("y"), Id("z"), SEqual, Id("x"), SPlus, Id("y"), SDiv, Id("z")))
    parseSuccess(parserShitAbstraction(l, firstSet, varMap, scopes, 0, 1)) { v =>
      val (pt, al): (ParseTree, Array[String]) = v
      assert(al.sameElements(Array("1x", "1y", "1z")))
      assertEquals(pt,
        Application(
          Application(
            Ident("plus"),
            Ident("0x")
          ),
          Application(
            Application(
              Ident("div"),
              Ident("0y")
            ),
            Ident("0z")
          )
        )
      )
    }
  }

  test("funcdefs' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(KDef, Id("id"), Id("x"), SEqual, Id("x"), KDef, Id("id2"), Id("y"), SEqual, Id("y")))
    parseSuccess(parserShitFuncDefsP(l, firstSet, varMap, scopes)) { _ =>
      assertEquals(varMap.values.size, 2)
    }
  }

  test("expr' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(KWhere, Id("id"), Id("x"), SEqual, Id("x"), KSemicolon, Id("id2"), Id("y"), SEqual, Id("y")))
    parseSuccess(parserShitExprP(l, firstSet, varMap, scopes, 0)) { _ =>
      assertEquals(varMap.values.size, 2)
    }
  }

  test("list' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lEmpty = TestLexer(Array(KCloseBracket))
    parseSuccess(parserShitListP(lEmpty, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Nil))
    }

    val lSingle = TestLexer(Array(CNum(1), KCloseBracket))
    parseSuccess(parserShitListP(lSingle, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("cons"),
            Const(Constant.Num(1))
          ),
          Const(Constant.Nil)
        )
      )
    }

    val lMultiple = TestLexer(Array(CNum(1), KComma, CBool(true), KComma, CString("test"), KCloseBracket))
    parseSuccess(parserShitListP(lMultiple, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("cons"),
            Const(Constant.Num(1))
          ),
          Application(
            Application(
              Ident("cons"),
              Const(Constant.Bool(true))
            ),
            Application(
              Application(
                Ident("cons"),
                Const(Constant.Str("test"))
              ),
              Const(Constant.Nil)
            )
          )
        )
      )
    }
  }

  test("system non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(KDef, Id("id"), Id("x"), SEqual, Id("x"), KDef, Id("id2"), Id("y"), SEqual, Id("y"), KDot, Id("id2"), Id("x"), KWhere, Id("x"), SEqual, CNum(42)))
    parseSuccess(parserShitSystem(l, firstSet, varMap, scopes)) { pt =>
      assertEquals(varMap.values.size, 3)
      assertEquals(pt,
        Application(
          Ident("0id2"),
          Ident("0x")
        )
      )
    }
  }
}
