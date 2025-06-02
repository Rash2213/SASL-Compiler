package visualizer

import vm.ReductionTree.*
import parser.Constant

class VisualizerReductionTreeSpec extends munit.FunSuite {
  test("visualization of static reduction trees") {
    val rt = Application(
      Application(
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
      ),
      Const(Constant.Num(20))
    )

    val lInf: Application = Application(
      Application(
        Cons,
        Const(Constant.Num(1))
      ),
      Const(Constant.Num(1))
    )
    lInf.operand = lInf

    val visualizer = VisualizerReductionTree()
    val ds = visualizer.generateDot(rt)
    visualizer.saveDotToFile(ds, "visualizations/rt/test.dot")
    println("Reduction tree visualization for test saved to visualizations/rt/test.dot")
    val dsi = visualizer.generateDot(lInf)
    visualizer.saveDotToFile(dsi, "visualizations/rt/inf.dot")
    println("Reduction tree visualization for infinite list saved to visualizations/rt/inf.dot")
  }
}
