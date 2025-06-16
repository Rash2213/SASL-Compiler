package parser

import lexer.{TestLexer, Token}
import lexer.Token.*
import parser.ParseTree.*
import parser.SaslData.{NonTerminal, derMap, emMap}

import scala.collection.mutable

class ParserRDSpec extends munit.FunSuite {
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

    parseSuccess(parserRDSimple(lId, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Ident("myVar", Some(0)))
    }
    parseSuccess(parserRDSimple(lNum, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Num(12)))
    }
    parseSuccess(parserRDSimple(lBool, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Bool(true)))
    }
    parseSuccess(parserRDSimple(lNil, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Nil))
    }
  }

  test("comb' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(CNum(12), CBool(true), CNil))

    parseSuccess(parserRDCombP(l, firstSet, varMap, scopes, 0, Ident("myVar", None))) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Application(
              Ident("myVar", None),
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

    parseSuccess(parserRDFactor(l, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Application(
              Ident("myVar", Some(0)),
              Const(Constant.Num(12))
            ),
            Const(Constant.Bool(true))
          ),
          Const(Constant.Nil)
        )
      )
    }
    parseSuccess(parserRDFactor(lPlus, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Application(
              Ident("myVar", Some(0)),
              Const(Constant.Num(12))
            ),
            Const(Constant.Bool(true))
          ),
          Const(Constant.Nil)
        )
      )
    }
    parseSuccess(parserRDFactor(lMinus, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Ident("inv", None),
          Application(
            Application(
              Application(
                Ident("myVar", Some(0)),
                Const(Constant.Num(12))
              ),
              Const(Constant.Bool(true))
            ),
            Const(Constant.Nil)
          )
        )
      )
    }
    parseSuccess(parserRDFactor(lNot, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Ident("not", None),
          Application(
            Application(
              Application(
                Ident("myVar", Some(0)),
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

    parseSuccess(parserRDMulP(lMul, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("div", None),
            Application(
              Application(
                Ident("mul", None),
                Ident("x", None)
              ),
              Const(Constant.Num(2))
            )
          ),
          Const(Constant.Num(3))
        )
      )
    }

    parseSuccess(parserRDMulP(lDiv, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("mul", None),
            Application(
              Application(
                Ident("div", None),
                Ident("x", None)
              ),
              Const(Constant.Num(3))
            )
          ),
          Const(Constant.Num(2))
        )
      )
    }

    parseSuccess(parserRDMulP(lNone, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x", None))
    }
  }

  test("add' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lPlusMinus = TestLexer(Array(SPlus, CNum(2), SMinus, CNum(3)))
    val lMinusPlus = TestLexer(Array(SMinus, CNum(3), SPlus, CNum(2)))
    val lNone = TestLexer[Token](Array())

    parseSuccess(parserRDAddP(lPlusMinus, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("minus", None),
            Application(
              Application(
                Ident("plus", None),
                Ident("x", None)
              ),
              Const(Constant.Num(2))
            )
          ),
          Const(Constant.Num(3))
        )
      )
    }

    parseSuccess(parserRDAddP(lMinusPlus, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("plus", None),
            Application(
              Application(
                Ident("minus", None),
                Ident("x", None)
              ),
              Const(Constant.Num(3))
            )
          ),
          Const(Constant.Num(2))
        )
      )
    }

    parseSuccess(parserRDAddP(lNone, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x", None))
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

    parseSuccess(parserRDComparP(lEq, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("neq", None),
            Application(
              Application(
                Ident("eq", None),
                Ident("x", None)
              ),
              Const(Constant.Num(2))
            )
          ),
          Const(Constant.Num(3))
        )
      )
    }

    parseSuccess(parserRDComparP(lNeq, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("eq", None),
            Application(
              Application(
                Ident("neq", None),
                Ident("x", None)
              ),
              Const(Constant.Num(3))
            )
          ),
          Const(Constant.Num(2))
        )
      )
    }

    parseSuccess(parserRDComparP(lLtGt, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("gt", None),
            Application(
              Application(
                Ident("lt", None),
                Ident("x", None)
              ),
              Const(Constant.Num(1))
            )
          ),
          Const(Constant.Num(4))
        )
      )
    }

    parseSuccess(parserRDComparP(lGtLt, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("lt", None),
            Application(
              Application(
                Ident("gt", None),
                Ident("x", None)
              ),
              Const(Constant.Num(4))
            )
          ),
          Const(Constant.Num(1))
        )
      )
    }

    parseSuccess(parserRDComparP(lLeqGeq, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("geq", None),
            Application(
              Application(
                Ident("leq", None),
                Ident("x", None)
              ),
              Const(Constant.Num(2))
            )
          ),
          Const(Constant.Num(3))
        )
      )
    }

    parseSuccess(parserRDComparP(lGeqLeq, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("leq", None),
            Application(
              Application(
                Ident("geq", None),
                Ident("x", None)
              ),
              Const(Constant.Num(3))
            )
          ),
          Const(Constant.Num(2))
        )
      )
    }

    parseSuccess(parserRDComparP(lNone, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x", None))
    }
  }

  test("conjunct' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lAnd = TestLexer(Array(SAnd, CBool(true), SAnd, CBool(false)))
    val lNone = TestLexer[Token](Array())

    parseSuccess(parserRDConjunctP(lAnd, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("and", None),
            Application(
              Application(
                Ident("and", None),
                Ident("x", None)
              ),
              Const(Constant.Bool(true))
            )
          ),
          Const(Constant.Bool(false))
        )
      )
    }

    parseSuccess(parserRDConjunctP(lNone, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x", None))
    }
  }

  test("opexpr' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lOr = TestLexer(Array(SOr, CBool(true), SOr, CBool(false)))
    val lNone = TestLexer[Token](Array())

    parseSuccess(parserRDOpExprP(lOr, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("or", None),
            Application(
              Application(
                Ident("or", None),
                Ident("x", None)
              ),
              Const(Constant.Bool(true))
            )
          ),
          Const(Constant.Bool(false))
        )
      )
    }

    parseSuccess(parserRDOpExprP(lNone, firstSet, varMap, Ident("x", None), scopes, 0)) { pt =>
      assertEquals(pt, Ident("x", None))
    }
  }


  test("listexpr non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lSingleItemList = TestLexer(Array(CNum(1)))
    val lMultipleItems = TestLexer(Array(CNum(1), KColon, CBool(true), KColon, CNil))

    parseSuccess(parserRDListExpr(lSingleItemList, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Num(1)))
    }

    parseSuccess(parserRDListExpr(lMultipleItems, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("cons", None),
            Const(Constant.Num(1))
          ),
          Application(
            Application(
              Ident("cons", None),
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

    parseSuccess(parserRDCondExpr(lMultipleItems, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("cons", None),
            Const(Constant.Num(1))
          ),
          Application(
            Application(
              Ident("cons", None),
              Const(Constant.Bool(true))
            ),
            Const(Constant.Nil)
          )
        )
      )
    }

    parseSuccess(parserRDCondExpr(lCondition, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Application(
              Ident("cond", None),
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
    parseSuccess(parserRDAbstraction(l, firstSet, varMap, scopes, 0, 1)) { v =>
      val (pt, al): (ParseTree, Array[String]) = v
      assert(al.sameElements(Array("1x", "1y", "1z")))
      assertEquals(pt,
        Application(
          Application(
            Ident("plus", None),
            Ident("x", Some(0))
          ),
          Application(
            Application(
              Ident("div", None),
              Ident("y", Some(0))
            ),
            Ident("z", Some(0))
          )
        )
      )
    }
  }

  test("funcdefs' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(KDef, Id("id"), Id("x"), SEqual, Id("x"), KDef, Id("id2"), Id("y"), SEqual, Id("y")))
    parseSuccess(parserRDFuncDefsP(l, firstSet, varMap, scopes)) { _ =>
      assertEquals(varMap.values.size, 2)
    }
  }

  test("expr' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val l = TestLexer(Array(KWhere, Id("id"), Id("x"), SEqual, Id("x"), KSemicolon, Id("id2"), Id("y"), SEqual, Id("y")))
    parseSuccess(parserRDExprP(l, firstSet, varMap, scopes, 0)) { _ =>
      assertEquals(varMap.values.size, 2)
    }
  }

  test("list' non-terminal is derived correctly") {
    val varMap: VariableMap = mutable.Map()
    val scopes: Scopes = mutable.ArrayBuffer()

    val lEmpty = TestLexer(Array(KCloseBracket))
    parseSuccess(parserRDListP(lEmpty, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt, Const(Constant.Nil))
    }

    val lSingle = TestLexer(Array(CNum(1), KCloseBracket))
    parseSuccess(parserRDListP(lSingle, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("cons", None),
            Const(Constant.Num(1))
          ),
          Const(Constant.Nil)
        )
      )
    }

    val lMultiple = TestLexer(Array(CNum(1), KComma, CBool(true), KComma, CString("test"), KCloseBracket))
    parseSuccess(parserRDListP(lMultiple, firstSet, varMap, scopes, 0)) { pt =>
      assertEquals(pt,
        Application(
          Application(
            Ident("cons", None),
            Const(Constant.Num(1))
          ),
          Application(
            Application(
              Ident("cons", None),
              Const(Constant.Bool(true))
            ),
            Application(
              Application(
                Ident("cons", None),
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
    parseSuccess(parserRDSystem(l, firstSet, varMap, scopes)) { pt =>
      assertEquals(varMap.values.size, 3)
      assertEquals(pt,
        Application(
          Ident("id2", Some(0)),
          Ident("x", Some(0))
        )
      )
    }
  }
}
