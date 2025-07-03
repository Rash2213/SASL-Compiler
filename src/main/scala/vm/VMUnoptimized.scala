package vm

import parser.Constant

import scala.collection.mutable

enum Ownership[T]:
  case Strong(i: T)
  case Weak(i: T)

  def inner: T = this match {
      case Strong(i) => i
      case Weak(i) => i
    }

import Ownership.*

enum ReductionTreeUnoptimized:
  case Const(c: Constant)
  case S
  case K
  case I
  case Y
  case U
  case Plus
  case Minus
  case Mul
  case Div
  case Leq
  case Lt
  case Geq
  case Gt
  case Eq
  case Neq
  case And
  case Or
  case Cond
  case Cons
  case Hd
  case Tl
  case Application(var operator: Ownership[ReductionTreeUnoptimized], var operand: Ownership[ReductionTreeUnoptimized])
  case Pair(hd: ReductionTreeUnoptimized, tl: ReductionTreeUnoptimized)
  case Unresolved(name: String)

import ReductionTreeUnoptimized.*

enum ReductionResultUnoptimized:
  case Const(c: Constant)
  case List(p: Pair)

def reduceUnoptimized(tree: ReductionTreeUnoptimized): Either[String, ReductionResultUnoptimized] = {
  val s: mutable.Stack[ReductionTreeUnoptimized] = mutable.Stack()
  s.push(tree)
  while (s.nonEmpty) {
    s.top match {
      case Const(c: Constant) =>
        return Right(ReductionResultUnoptimized.Const(c))
      case p: Pair =>
        return Right(ReductionResultUnoptimized.List(p))
      case Application(op, _) =>
        s.push(op.inner)
      case S =>
        if (s.length < 4) {
          return Left("Encountered S combinator with too few arguments, expected 4 received " + s.length)
        }
        s.pop()
        val a3 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for S combinator")
        }
        val a2 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for S combinator")
        }
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for S combinator")
        }
        val f = a3.operand
        val g = a2.operand
        val x = a1.operand

        a1.operator = Strong(Application(f, x))
        a1.operand = Strong(Application(g, x))

        s.push(a1)
        s.push(a1.operator.inner)
        s.push(f.inner)
      case K =>
        if (s.length < 3) {
          return Left("Encountered K combinator with too few arguments")
        }
        s.pop()
        val a2 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for K combinator")
        }
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for K combinator")
        }
        val x = a2.operand
        a1.operator = Strong(I)
        a1.operand = x
        s.push(x.inner)
      case I =>
        if (s.length < 2) {
          return Left("Encountered I combinator with too few arguments")
        }
        s.pop()
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for I combinator")
        }
        s.push(a1.operand.inner)
      case op @ (Eq | Neq) =>
        if (s.length < 3) {
          return Left("Encountered built-int operator \"" + op.toString + "\" with too few arguments")
        }
        s.pop()
        val a2 = s.pop() match {
          case a: Application => a
          case v @ _ => return Left("Encountered wrong argument for " + op.toString + " expected Application, got " + v.toString)
        }
        val a1 = s.pop() match {
          case a: Application => a
          case v @ _ => return Left("Encountered wrong argument for " + op.toString + " expected Application, got " + v.toString)
        }
        val v2 = reduceUnoptimized(a2.operand.inner) match {
          case Right(res) => res
          case Left(e) => return Left("Reduce in eq/neq failed: " + e)
        }
        val v1 = reduceUnoptimized(a1.operand.inner) match {
          case Right(res) => res
          case Left(e) => return Left("Reduce in eq/neq failed: " + e)
        }
        a1.operator = Strong(I)
        a1.operand = (v2, v1) match {
          case (ReductionResultUnoptimized.Const(Constant.Bool(b2)), ReductionResultUnoptimized.Const(Constant.Bool(b1))) =>
            Strong(Const(Constant.Bool(op match {
              case Eq => b2 == b1
              case Neq => b2 != b1
            })))
          case (ReductionResultUnoptimized.Const(Constant.Num(n2)), ReductionResultUnoptimized.Const(Constant.Num(n1))) =>
            Strong(Const(Constant.Bool(op match {
              case Eq => n2 == n1
              case Neq => n2 != n1
            })))
          case (ReductionResultUnoptimized.Const(Constant.Str(s2)), ReductionResultUnoptimized.Const(Constant.Str(s1))) =>
            Strong(Const(Constant.Bool(op match {
              case Eq => s2 == s1
              case Neq => s2 != s1
            })))
          case (ReductionResultUnoptimized.Const(Constant.Nil), ReductionResultUnoptimized.Const(Constant.Nil)) =>
            Strong(Const(Constant.Bool(op match {
              case Eq => true
              case Neq => false
            })))
          case (ReductionResultUnoptimized.List(p1), ReductionResultUnoptimized.List(p2)) =>
            // Todo
            Strong(Const(Constant.Bool(false)))
          case _ =>
            op match {
              case Neq => Strong(Const(Constant.Bool(true)))
              case Eq => Strong(Const(Constant.Bool(false)))
            }
        }
        s.push(a1.operand.inner)
      case op @ (Plus | Minus | Mul | Div | Leq | Lt | Geq | Gt) =>
        if (s.length < 3) {
          return Left("Encountered built-in operator with too few arguments")
        }
        s.pop()
        val a2 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val n2 = reduceUnoptimized(a2.operand.inner) match {
          case Right(res) => res match {
            case ReductionResultUnoptimized.Const(Constant.Num(n)) =>
              n
            case _ => return Left("Encountered wrong argument for built-in operator")
          }
          case Left(e) => return Left(e)
        }
        val n1 = reduceUnoptimized(a1.operand.inner) match {
          case Right(res) => res match {
            case ReductionResultUnoptimized.Const(Constant.Num(n)) =>
              n
            case _ => return Left("Encountered wrong argument for built-in operator")
          }
          case Left(e) => return Left(e)
        }
        a1.operator = Strong(I)
        a1.operand = op match {
          case op @ (Plus | Minus | Mul | Div) =>
            Strong(Const(Constant.Num(
              op match {
                case Plus => n2 + n1
                case Minus => n2 - n1
                case Mul => n2 * n1
                case Div => n2 / n1
              }
            )))
          case op @ (Leq | Lt | Geq | Gt) =>
            Strong(Const(Constant.Bool(
              op match {
                case Leq => n2 <= n1
                case Lt => n2 < n1
                case Geq => n2 >= n1
                case Gt => n2 > n1
              }
            )))
        }
        s.push(a1.operand.inner)
      case op @ (And | Or) =>
        if (s.length < 3) {
          return Left("Encountered built-in operator with too few arguments")
        }
        s.pop()
        val a2 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val n2 = reduceUnoptimized(a2.operand.inner) match {
          case Right(res) => res match {
            case ReductionResultUnoptimized.Const(Constant.Bool(b)) => b
            case _ => return Left("Encountered wrong argument for built-in operator")
          }
          case Left(e) => return Left(e)
        }
        val n1 = reduceUnoptimized(a1.operand.inner) match {
          case Right(res) => res match {
            case ReductionResultUnoptimized.Const(Constant.Bool(b)) => b
            case _ => return Left("Encountered wrong argument for built-in operator")
          }
          case Left(e) => return Left(e)
        }
        a1.operator = Strong(I)
        // ToDo: can increase laziness: false && x, true || x do not require evaluating x
        a1.operand = Strong(Const(Constant.Bool(op match {
          case And => n2 && n1
          case Or => n2 || n1
        })))
        s.push(a1.operand.inner)
      case Cond =>
        if (s.length < 4) {
          return Left("Encountered built-in operator with too few arguments")
        }
        s.pop()
        val a3 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val a2 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val b = reduceUnoptimized(a3.operand.inner) match {
          case Right(res) => res match {
            case ReductionResultUnoptimized.Const(Constant.Bool(b)) => b
            case _ => return Left("")
          }
          case Left(e) => return Left(e)
        }
        a1.operator = Strong(I)
        a1.operand = if b then a2.operand else a1.operand
        s.push(a1.operand.inner)
      case Cons =>
        if (s.length < 3) {
          return Left("Encountered cons with too few arguments")
        }
        s.pop()
        val a2 = s.pop() match {
          case a: Application => a
          case v @ _ => return Left("Encountered wrong argument for cons, expected Application, received " + v.toString)
        }
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for cons 2")
        }
        a1.operator = Strong(I)
        a1.operand = Strong(Pair(a2.operand.inner, a1.operand.inner))
        s.push(a1.operand.inner)
      case op @ (Hd | Tl) =>
        if (s.length < 2) {
          return Left("Encountered list operator with too few arguments")
        }
        s.pop()
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val p = reduceUnoptimized(a1.operand.inner) match {
          case Right(res) => res match {
            case ReductionResultUnoptimized.List(p) => p
            case _ => return Left("")
          }
          case Left(e) => return Left(e)
        }
        a1.operator = Strong(I)
        a1.operand = Strong(op match {
          case Hd => p.hd
          case Tl => p.tl
        })
        s.push(a1.operand.inner)
      case Y =>
        if (s.length < 2) {
          return Left("Encountered Y combinator with too few arguments")
        }
        s.pop()
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for Y combinator")
        }
        a1.operator = a1.operand
        a1.operand = Weak(a1)
        s.push(a1)
        s.push(a1.operator.inner)
      case U =>
        if (s.length < 3) {
          return Left("Encountered U combinator with too few arguments")
        }
        s.pop()
        val a2 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for Y combinator")
        }
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for Y combinator")
        }
        val f = a2.operand
        val z = a1.operand
        a1.operator = Strong(Application(f, Strong(Application(Strong(Hd), z))))
        a1.operand = Strong(Application(Strong(Tl), z))
        s.push(a1)
        s.push(a1.operator.inner)
        s.push(f.inner)
      case Unresolved(n) =>
        return Left("Encountered unresolved variable: \"" + n + "\"")
    }
  }
  Left("Reduction of program did not return a value")
}

/**
 * Prints the result of evaluating the `tree` (a constant or a potentially infinite list).
 * Does not print a newline after its output.
 * @param tree a compilation tree as received from the compiler
 */
def evalAndPrintUnoptimized(tree: ReductionTreeUnoptimized): Unit = {
  reduceUnoptimized(tree) match {
    case Left(e) => print("Reduction Error: " + e)
    case Right(res) =>
      res match {
        case ReductionResultUnoptimized.Const(c) =>
          c match {
            case Constant.Bool(b) =>
              print(b.toString + " (bool)")
            case Constant.Num(n) =>
              print(n.toString + " (num)")
            case Constant.Str(s) =>
              print("\"" + s + "\" (string)")
            case Constant.Nil =>
              print("nil")
          }
        case ReductionResultUnoptimized.List(p) =>
          evalAndPrintUnoptimized(p.hd)
          print(" : ")
          evalAndPrintUnoptimized(p.tl)
      }
  }
}