package parser

import ParseTree.*

import scala.collection.mutable

class Visualizer {
  private var idCounter = 0
  private val sb = new StringBuilder

  private def nextId(): String = {
    idCounter += 1
    "node" + idCounter
  }

  def visit(pt: ParseTree): String = {
    val id = nextId()
    pt match {
      /*case Program(definitions, expr) =>
        sb.append(s"  $id [label=\"Program\"];\n")
        val exprId = visit(expr)
        sb.append(s"  $id -> $exprId;\n")
        for (defn <- definitions) {
          val defId = nextId()
          sb.append(s"  $defId [label=\"${defn.name}\"];\n")
          val bodyId = visit(defn.value)
          sb.append(s"  $defId -> $bodyId;\n")
          sb.append(s"  $id -> $defId;\n")
        }
      case Abstraction(params, body) =>
        sb.append(s"""  $id [label="${params.mkString(", ")}"];\n""")
        val bodyId = visit(body)
        sb.append(s"  $id -> $bodyId;\n")*/
      case Ident(name) =>
        sb.append(s"""  $id [label="$name"];\n""")
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
      /*case Where(expr, definitions) =>
        sb.append(s"""  $id [label="Where"];\n""")
        val exprId = visit(expr)
        sb.append(s"  $id -> $exprId;\n")
        for (defn <- definitions) {
          val defId = nextId()
          sb.append(s"""  $defId [label="${defn.name}"];\n""")
          val bodyId = visit(defn.value)
          sb.append(s"  $defId -> $bodyId;\n")
          sb.append(s"  $id -> $defId;\n")
        }
      case Cons(head, tail) =>
        sb.append(s"""  $id [label="Cons"];\n""")
        val headId = visit(head)
        val tailId = visit(tail)
        sb.append(s"  $id -> $headId;\n")
        sb.append(s"  $id -> $tailId;\n")*/
    }
    id
  }

  def generateDot(tree: ParseTree, varMap: VariableMap): String = {
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
