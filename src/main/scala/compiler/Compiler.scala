package compiler

import compiler.CompilerError.UnresolvedVariable
import vm.ReductionTree
import parser.{ParseTree, Scopes, VariableMap}
import parser.ParseTree.*

enum CompilerError:
  case UnresolvedVariable(n: String)

class Compiler {

  val builtins: Map[String, ReductionTree] = Map(
    /*"eq" -> CombTree.Ident("eq"),
    "neq" -> CombTree.Ident("neq"),
    "lt" -> CombTree.Ident("lt"),
    "gt" -> CombTree.Ident("gt"),
    "leq" -> CombTree.Ident("leq"),
    "geq" -> CombTree.Ident("geq"),*/
    "plus" -> ReductionTree.Plus,
    "minus" -> ReductionTree.Minus,
    "mul" -> ReductionTree.Mul,
    "div" -> ReductionTree.Div,
    /*"not" -> CombTree.Ident("not"),
    "inv" -> CombTree.Ident("inv"),*/
    "cond" -> ReductionTree.Cond,
    "cons" -> ReductionTree.Cons,
  )

  def resolveVarScopedName(name: String, scope: Int, scopes: Scopes, varNames: Set[String]): Option[String] = {
    var s = scope
    while (s != 0) {
      val scopedName = s.toString + name
      if (varNames.contains(scopedName)) {
        return Some(scopedName)
      }
      s = scopes(s).parent
    }
    val scopedName = "0" + name
    Some(scopedName).filter(varNames.contains)
  }

  def resolveVarScoped(name: String, scope: Int, scopes: Scopes, envMap: Map[String, ReductionTree]): Option[ReductionTree] = {
    var s = scope
    while (s != 0) {
      envMap.get(s.toString + name) match {
        case Some(v) => return Some(v)
        case None =>
      }
      s = scopes(s).parent
    }
    envMap.get("0" + name)
  }

  def compileLoose(tree: ParseTree, scopes: Scopes, varNames: Set[String]): Either[CompilerError, ReductionTree] = {
    tree match {
      case Const(c) => Right(ReductionTree.Const(c))
      case Application(f, x) =>
        Right(ReductionTree.Application(
          compileLoose(f, scopes, varNames) match {
            case Left(e) => return Left(e)
            case Right(v) => v
          },
          compileLoose(x, scopes, varNames) match {
            case Left(e) => return Left(e)
            case Right(v) => v
          }
        ))
      case Ident(name, scope) =>
        scope match {
          case Some(scope) =>
            resolveVarScopedName(name, scope, scopes, varNames) match {
              case None => builtins.get(name) match {
                case None => Left(UnresolvedVariable(name))
                case Some(v) => Right(v)
              }
              case Some(n) =>
                Right(ReductionTree.Unresolved(n))
            }
          case None =>
            builtins.get(name) match {
              case None => Left(UnresolvedVariable(name))
              case Some(v) => Right(v)
            }
        }
    }
  }

  def compileStrict(tree: ParseTree, scopes: Scopes, env: Map[String, ReductionTree]): Either[CompilerError, ReductionTree] = {
    tree match {
      case Const(c) => Right(ReductionTree.Const(c))
      case Ident(name, scope) =>
        scope match {
          case Some(scope) =>
            resolveVarScoped(name, scope, scopes, env) match {
              case Some(v) => Right(v)
              case None => builtins.get(name) match {
                case None => Left(UnresolvedVariable(name))
                case Some(v) => Right(v)
              }
            }
          case None =>
            builtins.get(name) match {
              case None => Left(UnresolvedVariable(name))
              case Some(v) => Right(v)
            }
        }
      case Application(f, x) =>
        Right(ReductionTree.Application(
          compileStrict(f, scopes, env) match {
            case Right(rt) => rt
            case Left(e) => return Left(e)
          },
          compileStrict(x, scopes, env) match {
            case Right(rt) => rt
            case Left(e) => return Left(e)
          }
        ))
    }
  }

  private def usesVar(name: String, tree: ReductionTree): Boolean = tree match {
    case ReductionTree.Unresolved(n) => n == name
    case ReductionTree.Application(f, x) =>
      usesVar(name, f) || usesVar(name, x)
    case _ => false
  }

  def abstractVar(name: String, body: ReductionTree): ReductionTree = body match {
    case ReductionTree.S
      | ReductionTree.K
      | ReductionTree.I
      /*| ReductionTree.U
      | ReductionTree.Y*/
      | ReductionTree.Plus
      | ReductionTree.Minus
      | ReductionTree.Mul
      | ReductionTree.Div
      | ReductionTree.Const(_)
      | ReductionTree.Cond
      | ReductionTree.Cons
      | ReductionTree.Hd
      | ReductionTree.Tl
      =>
      ReductionTree.Application(
        ReductionTree.K,
        body
      )
    case ReductionTree.Application(f, x) =>
      ReductionTree.Application(
        ReductionTree.Application(
          ReductionTree.S,
          abstractVar(name, f)
        ),
        abstractVar(name, x)
      )
    case u @ ReductionTree.Unresolved(n) =>
      if (n == name) {
        ReductionTree.I
      } else {
        ReductionTree.Application(
          ReductionTree.K,
          u
        )
      }
    case ReductionTree.Pair(_, _) =>
      // Should never occur, Pair should only exist after reduction
      assert(false)
      body
  }

  /*def compileFunction(name: String, tree: ParseTree, args: Array[String]): ReductionTree = {
    val compiledBody: ReductionTree = compile(tree, env ++ argEnv)

    val finalTree = args.foldRight(compiledBody) { (arg, acc) =>
      abstractVar(arg, acc)
    }

    finalTree
  }*/

  def resolveTree(rt: ReductionTree, env: Map[String, ReductionTree]): Either[CompilerError, ReductionTree] = {
    rt match {
      case a @ ReductionTree.Application(f, x) =>
        a.operator = resolveTree(f, env) match {
          case Right(rt) => rt
          case Left(e) => return Left(e)
        }
        a.operand = resolveTree(x, env) match {
          case Right(rt) => rt
          case Left(e) => return Left(e)
        }
        Right(a)
      case ReductionTree.Unresolved(n) =>
        env.get(n) match {
          case Some(rt) => Right(rt)
          case None => Left(UnresolvedVariable(n))
        }
      case _ => Right(rt)
    }
  }

  def compileProgram(
                      mainTree: ParseTree,
                      scopes: Scopes,
                      varMap: VariableMap,
                    ): Either[CompilerError, ReductionTree] = {
    val functionNames = varMap.keys.toSet
    val compiledDefs: Map[String, ReductionTree] = varMap.toList.foldLeft(Map.empty[String, ReductionTree]) {
      case (current, (name, (pt, args))) =>
        val names = functionNames ++ args
        val compiledBody = compileLoose(pt, scopes, names) match {
          case Right(rt) => rt
          case Left(e) => return Left(e)
        }
        val abstracted = args.foldRight(compiledBody) { (arg, acc) => abstractVar(arg, acc) }
        current + (name -> abstracted)
    }
    for (d <- compiledDefs.values) {
      // ToDo: won't work for functions containing a single unresolved as a top node
      resolveTree(d, compiledDefs)
    }

    compileStrict(mainTree, scopes, compiledDefs)
  }
}
