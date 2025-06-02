package vm

import ReductionTree.*
import parser.Constant

class VMSpec extends munit.FunSuite {
  test("function application") {
    // def inc 𝑥 = 1 + 𝑥
    // S @ (S @ (K @ plus) @ (K @ 1)) @ I
    val inc = Application(
      Application(
        S,
        Application(
          Application(
            S,
            Application(
              K,
              Plus
            )
          ),
          Application(
            K,
            Const(Constant.Num(1))
          )
        )
      ),
      I
    )

    val dec = Application(
      Application(
        S,
        Application(
          Application(
            S,
            Application(
              K,
              Minus
            )
          ),
          I
        )
      ),
      Application(
        K,
        Const(Constant.Num(1))
      )
    )

    val incN5 = Application(inc, Const(Constant.Num(-5)))
    val inc647 = Application(inc, Const(Constant.Num(647)))

    reduce(incN5) match {
      case Right(v) => assertEquals(v, ReductionResult.Const(Constant.Num(-4)))
      case Left(e) => assert(false, "Reduction failed: " + e)
    }

    reduce(inc647) match {
      case Right(v) => assertEquals(v, ReductionResult.Const(Constant.Num(648)))
      case Left(e) => assert(false, "Reduction failed: " + e)
    }

    val dec0 = Application(dec, Const(Constant.Num(0)))
    val dec647 = Application(dec, Const(Constant.Num(647)))

    reduce(dec0) match {
      case Right(v) => assertEquals(v, ReductionResult.Const(Constant.Num(-1)))
      case Left(e) => assert(false, "Reduction failed: " + e)
    }

    reduce(dec647) match {
      case Right(v) => assertEquals(v, ReductionResult.Const(Constant.Num(646)))
      case Left(e) => assert(false, "Reduction failed: " + e)
    }

    // def idC 𝑥 = x * x / x
    // div @ (mul @ x @ x) @ x
    // S @ (S @ (K @ div) @ (S @ (S @ (K @ mul) @ I) @ I)) @ I
    val idC = Application(
      Application(
        S,
        Application(
          Application(
            S,
            Application(
              K,
              Div,
            )
          ),
          Application(
            Application(
              S,
              Application(
                Application(
                  S,
                  Application(
                    K,
                    Mul
                  )
                ),
                I
              )
            ),
            I
          )
        )
      ),
      I
    )

    val idCN5 = Application(idC, Const(Constant.Num(-5)))
    val idC647 = Application(idC, Const(Constant.Num(647)))

    reduce(idCN5) match {
      case Right(v) => assertEquals(v, ReductionResult.Const(Constant.Num(-5)))
      case Left(e) => assert(false, "Reduction failed: " + e)
    }

    reduce(idC647) match {
      case Right(v) => assertEquals(v, ReductionResult.Const(Constant.Num(647)))
      case Left(e) => assert(false, "Reduction failed: " + e)
    }
  }
}
