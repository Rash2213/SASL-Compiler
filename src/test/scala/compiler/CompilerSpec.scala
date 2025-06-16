package compiler

import munit.FunSuite
import munit.Assertions.*
import parser.{Constant, ParseTree, ScopeEntry, VariableMap}
import parser.ParseTree.*
import vm.ReductionTree
import scala.collection.mutable

class CompilerSpec extends munit.FunSuite {
  test("compiles programs without defs correctly") {
    val pt = Application(
      Application(
        Ident("plus", None),
        Const(Constant.Num(5))
      ),
      Const(Constant.Num(10))
    )

    val c = compiler.Compiler()
    val vM: VariableMap = mutable.Map()
    c.compileProgram(pt, mutable.ArrayBuffer(ScopeEntry(0)), vM) match {
      case Left(e) =>
        assert(false, e)
      case Right(rt) =>
        assertEquals(rt, ReductionTree.Application(
          ReductionTree.Application(
            ReductionTree.Plus,
            ReductionTree.Const(Constant.Num(5))
          ),
          ReductionTree.Const(Constant.Num(10))
        ))
    }
  }

  test("compiles function with two arguments") {
    val pt = Application(
      Application(
        Ident("plus", None),
        Ident("x", Some(0))
      ),
      Ident("y", Some(0))
    )

    val varMap: VariableMap = mutable.Map("0f" -> (pt, Array("x", "y")))
    val scopes = mutable.ArrayBuffer(ScopeEntry(0))
    val compiler = new Compiler()

    val scopedArgs = Set("0x", "0y", "0f")

    val result = compiler.compileLoose(pt, scopes, scopedArgs)

    result match {
      case Right(rt) =>
        assert(rt.toString.contains("Application"))
      case Left(e) =>
        fail("Unexpected compiler error: " + e)
    }
  }

  test("builtin function without scope compiles correctly") {
    val pt = Ident("plus", None)
    val vM: VariableMap = mutable.Map()
    val scopes = mutable.ArrayBuffer(ScopeEntry(0))

    val c = new Compiler()
    c.compileProgram(pt, scopes, vM) match {
      case Right(rt) => assertEquals(rt, ReductionTree.Plus)
      case Left(e) => fail("Unexpected error: " + e)
    }
  }

  test("fails on unresolved scoped variable") {
    val pt = Ident("x", Some(1))
    val vM: VariableMap = mutable.Map() // No x defined

    val scopes = mutable.ArrayBuffer(ScopeEntry(0), ScopeEntry(0))
    val c = new Compiler()

    c.compileProgram(pt, scopes, vM) match {
      case Left(CompilerError.UnresolvedVariable("x")) => assert(true)
      case _ => fail("Expected unresolved variable error")
    }
  }
}
