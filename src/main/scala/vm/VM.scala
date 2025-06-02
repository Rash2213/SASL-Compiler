package vm

import parser.Constant

import scala.collection.mutable

enum ReductionTree:
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
  case Cond
  case Cons
  case Hd
  case Tl
  case Application(var operator: ReductionTree, var operand: ReductionTree)
  case Pair(hd: ReductionTree, tl: ReductionTree)

import ReductionTree.*

enum ReductionResult:
  case Const(c: Constant)
  case List(p: Pair)

def reduce(tree: ReductionTree): Either[String, ReductionResult] = {
  val s: mutable.Stack[ReductionTree] = mutable.Stack()
  s.push(tree)
  while (s.nonEmpty) {
    s.top match {
      case Const(c: Constant) =>
        return Right(ReductionResult.Const(c))
      case p: Pair =>
        return Right(ReductionResult.List(p))
      case Application(op, _) =>
        s.push(op)
      case S =>
        if (s.length < 4) {
          return Left("Encountered S combinator with too few arguments")
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

        a1.operator = Application(f, x)
        a1.operand = Application(g, x)

        s.push(a1)
        s.push(a1.operator)
        s.push(f)
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
        a1.operator = I
        a1.operand = x
        s.push(x)
      case I =>
        if (s.length < 2) {
          return Left("Encountered I combinator with too few arguments")
        }
        s.pop()
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for I combinator")
        }
        s.push(a1.operand)
      case op @ (Plus | Minus | Mul | Div) =>
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
        val n2 = reduce(a2.operand) match {
          case Right(res) => res match {
            case ReductionResult.Const(Constant.Num(n)) =>
              n
            case _ => return Left("Encountered wrong argument for built-in operator")
          }
          case Left(e) => return Left(e)
        }
        val n1 = reduce(a1.operand) match {
          case Right(res) => res match {
            case ReductionResult.Const(Constant.Num(n)) =>
              n
            case _ => return Left("Encountered wrong argument for built-in operator")
          }
          case Left(e) => return Left(e)
        }
        a1.operator = I
        a1.operand = Const(Constant.Num(
          op match {
            case Plus => n2+n1
            case Minus => n2-n1
            case Mul => n2*n1
            case Div => n2/n1
          }
        ))
        s.push(a1.operand)
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
        val b = reduce(a3.operand) match {
          case Right(res) => res match {
            case ReductionResult.Const(Constant.Bool(b)) => b
            case _ => return Left("")
          }
          case Left(e) => return Left(e)
        }
        a1.operator = I
        a1.operand = if b then a2.operand else a1.operand
        s.push(a1.operand)
      case Cons =>
        if (s.length < 3) {
          return Left("Encountered built-in operator with too few arguments")
        }
        val a2 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        a1.operator = I
        a1.operand = Pair(a2.operand, a1.operand)
        s.push(a1.operand)
      case op @ (Hd | Tl) =>
        if (s.length < 2) {
          return Left("Encountered list operator with too few arguments")
        }
        s.pop()
        val a1 = s.pop() match {
          case a: Application => a
          case _ => return Left("Encountered wrong argument for built-in operator")
        }
        val p = reduce(a1.operand) match {
          case Right(res) => res match {
            case ReductionResult.List(p) => p
            case _ => return Left("")
          }
          case Left(e) => return Left(e)
        }
        a1.operator = I
        a1.operand = op match {
          case Hd => p.hd
          case Tl => p.tl
        }
        s.push(a1.operand)
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
        a1.operand = a1
        s.push(a1)
        s.push(a1.operator)
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
        a1.operator = Application(f, Application(Hd, z))
        a1.operand = Application(Tl, z)
        s.push(a1)
        s.push(a1.operator)
        s.push(f)
    }
  }
  Left("Reduction of program did not return a value")
}

/**
 * Prints the result of evaluating the `tree` (a constant or a potentially infinite list).
 * Does not print a newline after its output.
 * @param tree a compilation tree as received from the compiler
 */
def evalAndPrint(tree: ReductionTree): Unit = {
    reduce(tree) match {
      case Left(e) => print("Reduction Error: " + e)
      case Right(res) =>
        res match {
          case ReductionResult.Const(c) =>
            c match {
              case Constant.Bool(b) =>
                print(b.toString + " (bool)")
              case Constant.Num(n) =>
                print(n.toString + " (num)")
              case Constant.Str(s) =>
                print('"' + s.toString + '"' + " (string)")
              case Constant.Nil =>
                print("nil")
            }
          case ReductionResult.List(p) =>
            evalAndPrint(p.hd)
            print(" : ")
            evalAndPrint(p.tl)
        }
    }
}