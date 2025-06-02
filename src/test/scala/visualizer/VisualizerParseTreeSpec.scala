package visualizer

import parser.ParseTree.*
import parser.{ParseTree, VariableMap, Constant, ParserGenerator, Scopes, parserRDSystem}
import cli.{getFilename, readFileBin}
import lexer.{Lexer, Token}
import parser.SaslData.{NonTerminal, derMap, emMap}

import scala.collection.mutable

class VisualizerParseTreeSpec extends munit.FunSuite {
  test("visualization of static parse trees") {
    val pte: ParseTree = Application(
      Ident("null"),
      Ident("l")
    )
    val vme: VariableMap = mutable.Map()
    vme("l") = (Application(
      Application(
        Ident("cons"),
        Const(Constant.Num(1))
      ),
      Const(Constant.Nil)
    ), Array())
    vme("null") = (Application(
      Application(
        Ident("eq"),
        Ident("xs"),
      ),
      Const(Constant.Nil)
    ), Array("xs"))

    val ptt: ParseTree = Application(
      Application(
        Ident("myplus"),
        Ident("x")
      ),
      Ident("y")
    )

    val vmt: VariableMap = mutable.Map()
    vmt("x") = (Const(Constant.Num(12)), Array())
    vmt("y") = (Const(Constant.Num(42)), Array())
    vmt("myplus") = (Application(
      Application(
        Ident("plus"),
        Ident("x"),
      ),
      Ident("y"),
    ), Array("x", "y"))

    val visualizer = VisualizerParseTree()
    val dse = visualizer.generateDot(pte, vme)
    visualizer.saveDotToFile(dse, "visualizations/exercise.dot")
    println("Parse tree visualization for exercise saved to visualizations/exercise.dot")
    val dst = visualizer.generateDot(ptt, vmt)
    visualizer.saveDotToFile(dst, "visualizations/test.dot")
    println("Parse tree visualization for test saved to visualizations/test.dot")
  }

  test("visualization of examples") {
    val files = Array("examples/qsort.sasl", "examples/sieve.sasl", "examples/fibonacci.sasl")
    for (file <- files) {
      val raw = readFileBin(file)
      val lexer = Lexer(raw)
      val gen: ParserGenerator[Token, NonTerminal] = ParserGenerator()
      val fr = gen.first(NonTerminal.values.length, derMap, emMap)
      val firstSet = fr._1
      val varMap: VariableMap = mutable.Map()
      val scopes: Scopes = mutable.ArrayBuffer()
      parserRDSystem(lexer, firstSet, varMap, scopes) match {
        case Right(pt) =>
          val visualizer = VisualizerParseTree()
          val d = visualizer.generateDot(pt, varMap)
          val fn = getFilename(file)
          visualizer.saveDotToFile(d, s"visualizations/$fn.dot")
          println(s"Visualization saved to visualizations/$fn.dot")
        case Left(e) =>
          println("Encountered error during parsing!")
          assert(false, file)
      }
    }
  }
}
