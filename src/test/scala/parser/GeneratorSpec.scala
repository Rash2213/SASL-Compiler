package parser

class GeneratorSpec extends munit.FunSuite {
  test("generates parse table correctly") {
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

    val tbl = gen.oracleTable(5, 4, dm, em/*, fm*/)

    val empty: gen.Derivation = Array()

    val res: gen.ParseTable = Array(
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

    for (nt <- 0 until 4) {
      for (t <- 0 until 5) {
        (tbl(nt)(t), res(nt)(t)) match {
          case (Some(a1), Some(a2)) =>
            assertEquals(a1.toSeq, a2.toSeq, "At " + nt + ", " + t)
          case (None, None) => {}
          case _ =>
            assertEquals(tbl(nt)(t), res(nt)(t), "At " + nt + ", " + t)
        }
      }
    }
  }
}
