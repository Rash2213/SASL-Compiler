package visualizer

import parser.ParseTreeUnoptimized.*
import parser.{ParseTreeUnoptimized, Constant, VariableMapUnoptimized}

import scala.collection.mutable

class VisualizerParseTreeUnoptimized {
  private var idCounter = 0
  private val sb = new StringBuilder

  private def nextId(): String = {
    idCounter += 1
    "node" + idCounter
  }

  def visit(pt: ParseTreeUnoptimized): String = {
    val id = nextId()
    pt match {
      case Ident(name, scope) =>
        val l = scope match {
          case Some(scope) => scope.toString + name
          case None => name
        }
        sb.append(s"""  $id [label="$l"];\n""")
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
        val lhsId = visit(lhs)
        val rhsId = visit(rhs)
        sb.append(s"  $id -> $lhsId;\n")
        sb.append(s"  $id -> $rhsId;\n")
      case parser.ParseTreeUnoptimized.Where(mt, fs) =>
        sb.append(s"""  $id [label="Where"];\n""")
        val mtId = visit(mt)
        sb.append(s"  $id -> $mtId;\n")
        fs.foreach(f => {
          val (n, (pt, args)) = f
          val l = s"$n(${args.mkString(", ")})"
          val fId = nextId()
          sb.append(s"  $fId [label=\"$l\"];\n")
          sb.append(s"  $id -> $fId;\n")
          val fbId = visit(pt)
          sb.append(s"  $fId -> $fbId;\n")
        })
    }
    id
  }

  def generateDot(tree: ParseTreeUnoptimized, varMap: VariableMapUnoptimized): String = {
    sb.clear()
    val pid = nextId()
    sb.append("digraph ParseTree {\n")
    sb.append(s"  $pid [label=\"Program\"];\n")
    val id = visit(tree)
    sb.append(s"  $pid -> $id;\n")
    for ((s, (pt, al)) <- varMap.toList) {
      val fid = nextId()
      if (al.length == 0) {
        sb.append(s"  $fid [label=\"$s\"];\n")
      } else {
        val l = s"$s(${al.mkString(", ")})"
        sb.append(s"  $fid [label=\"$l\"];\n")
      }
      val bid = visit(pt)
      sb.append(s"  $fid -> $bid;\n")
    }
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
