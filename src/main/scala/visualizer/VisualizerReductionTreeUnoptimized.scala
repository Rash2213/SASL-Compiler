package visualizer

import vm.ReductionTreeUnoptimized
import vm.ReductionTreeUnoptimized.*
import parser.Constant

import scala.collection.mutable

class VisualizerReductionTreeUnoptimized {
  private var idCounter = 0
  private val sb = new StringBuilder

  private def nextId(): String = {
    idCounter += 1
    "node" + idCounter
  }

  def visit(pt: ReductionTreeUnoptimized, visited: mutable.Map[Int, String]): String = {
    val id = nextId()
    pt match {
      case op @ (S | K | I | Y | U | Plus | Minus | Mul | Div | Cond | Cons | Hd | Tl) =>
        val name = op match {
          case S => "S"
          case K => "K"
          case I => "I"
          case Y => "Y"
          case U => "U"
          case Plus => "Plus"
          case Minus => "Minus"
          case Mul => "Mul"
          case Div => "Div"
          case Cond => "Cond"
          case Cons => "Cons"
          case Hd => "Hd"
          case Tl => "Tl"
        }
        sb.append(s"""  $id [label="$name"];\n""")
        return id
      case _ =>
    }
    visited.get(System.identityHashCode(pt)) match {
      case Some(rId) =>
        sb.append(s"""  $id [label=""];\n""")
        sb.append(s"  $id -> $rId;\n")
        return id
      case None =>
    }
    visited.addOne(System.identityHashCode(pt), id)
    pt match {
      case Const(Constant.Num(n)) =>
        sb.append(s"""  $id [label="$n"];\n""")
      case Const(Constant.Bool(b)) =>
        sb.append(s"""  $id [label="$b"];\n""")
      case Const(Constant.Str(s)) =>
        sb.append(s"""  $id [label="$s"];\n""")
      case Const(Constant.Nil) =>
        sb.append(s"""  $id [label="Nil"];\n""")
      case Application(lhs, rhs) =>
        sb.append(s"""  $id [label="@"];\n""")
        val lhsId = visit(lhs, visited)
        val rhsId = visit(rhs, visited)
        sb.append(s"  $id -> $lhsId;\n")
        sb.append(s"  $id -> $rhsId;\n")
      case Pair(l, r) =>
        sb.append(s"""  $id [label="pair"];\n""")
        val lId = visit(l, visited)
        val rId = visit(r, visited)
        sb.append(s"  $id -> $lId;\n")
        sb.append(s"  $id -> $rId;\n")
      case _ =>
    }
    id
  }

  def generateDot(tree: ReductionTreeUnoptimized): String = {
    sb.clear()
    val pid = nextId()
    sb.append("digraph ReductionTree {\n")
    sb.append(s"  $pid [label=\"Program\"];\n")
    val id = visit(tree, mutable.Map())
    sb.append(s"  $pid -> $id;\n")
    sb.append("}\n")
    sb.toString()
  }

  def saveDotToFile(dotString: String, filename: String): Unit = {
    import java.io.PrintWriter
    import java.io.File

    val pw = new PrintWriter(new File(filename))
    try {
      pw.write(dotString)
    } finally {
      pw.close()
    }
  }
}
