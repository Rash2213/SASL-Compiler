package compiler

import compiler.CompilerError.UnresolvedVariable
import vm.ReductionTreeUnoptimized
import parser.{Constant, ParseTreeUnoptimized, Scopes, VariableMapUnoptimized}
import parser.ParseTreeUnoptimized.*

class CompilerUnoptimized {

  val builtins: Map[String, ReductionTreeUnoptimized] = Map(
    "eq" -> ReductionTreeUnoptimized.Eq,
    "neq" -> ReductionTreeUnoptimized.Neq,
    "lt" -> ReductionTreeUnoptimized.Lt,
    "gt" -> ReductionTreeUnoptimized.Gt,
    "leq" -> ReductionTreeUnoptimized.Leq,
    "geq" -> ReductionTreeUnoptimized.Geq,
    "plus" -> ReductionTreeUnoptimized.Plus,
    "minus" -> ReductionTreeUnoptimized.Minus,
    "mul" -> ReductionTreeUnoptimized.Mul,
    "div" -> ReductionTreeUnoptimized.Div,
    /*"not" -> CombTree.Ident("not"),
    "inv" -> CombTree.Ident("inv"),*/
    "cond" -> ReductionTreeUnoptimized.Cond,
    "cons" -> ReductionTreeUnoptimized.Cons,
    "tl" -> ReductionTreeUnoptimized.Tl,
    "hd" -> ReductionTreeUnoptimized.Hd,
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

  def resolveVarScopedArg(name: String, scope: Int, scopes: Scopes, envMap: Map[String, ReductionTreeUnoptimized], args: Set[String]): Option[ReductionTreeUnoptimized] = {
    var s = scope
    while (s != 0) {
      envMap.get(s.toString + name) match {
        case Some(v) => return Some(v)
        case None =>
      }
      if (args.contains(s.toString + name)) {
        return Some(ReductionTreeUnoptimized.Unresolved(s.toString + name))
      }
      s = scopes(s).parent
    }
    envMap.get("0" + name)
  }

  def resolveVarScoped(name: String, scope: Int, scopes: Scopes, envMap: Map[String, ReductionTreeUnoptimized]): Option[ReductionTreeUnoptimized] = {
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

  def compileLoose(tree: ParseTreeUnoptimized, scopes: Scopes, envMap: Map[String, ReductionTreeUnoptimized], argNames: Set[String]): Either[CompilerError, ReductionTreeUnoptimized] = {
    tree match {
      case Const(c) => Right(ReductionTreeUnoptimized.Const(c))
      case Application(f, x) =>
        Right(ReductionTreeUnoptimized.Application(
          compileLoose(f, scopes, envMap, argNames) match {
            case Left(e) => return Left(e)
            case Right(v) => v
          },
          compileLoose(x, scopes, envMap, argNames) match {
            case Left(e) => return Left(e)
            case Right(v) => v
          }
        ))
      case Ident(name, scope) =>
        scope match {
          case Some(scope) =>
            resolveVarScopedArg(name, scope, scopes, envMap, argNames) match {
              case None => builtins.get(name) match {
                case None => Left(UnresolvedVariable(name))
                case Some(v) => Right(v)
              }
              case Some(rt) =>
                Right(rt)
            }
          case None =>
            builtins.get(name) match {
              case None => Left(UnresolvedVariable(name))
              case Some(v) => Right(v)
            }
        }
      case Where(pt, fs) =>
        // U @ ([𝑓] (U @ [𝑔] (K @ 𝐸1))) @ (Y @ (U @ ([𝑓] (U @ [𝑔] (K @ [[𝑥] 𝐸2,[𝑦] 𝐸3])))))
        val bodyList = fs.foldRight(ReductionTreeUnoptimized.Const(Constant.Nil)) {
          case ((name, (pt, args)), current) =>
            val names = argNames ++ args
            val compiledBody = compileLoose(pt, scopes, envMap, names) match {
              case Right(rt) => rt
              case Left(e) => return Left(e)
            }
            val abstracted = args.foldRight(compiledBody) { (arg, acc) => abstractVar(arg, acc) }
            ReductionTreeUnoptimized.Application(
              ReductionTreeUnoptimized.Application(
                ReductionTreeUnoptimized.Cons,
                abstracted
              ),
              current
            )
        }
        val bodyExpr = fs.map(f => f._1).foldRight(
          ReductionTreeUnoptimized.Application(
            ReductionTreeUnoptimized.K,
            bodyList
          )
        ) {
          case (name, current) =>
            ReductionTreeUnoptimized.Application(
              ReductionTreeUnoptimized.U,
              abstractVar(name, current)
            )
        }
        val mainExpr = fs.map(f => f._1).foldRight(
          ReductionTreeUnoptimized.Application(
            ReductionTreeUnoptimized.K,
            compileLoose(pt, scopes, envMap, argNames) match {
              case Right(rt) => rt
              case Left(e) => return Left(e)
            }
          )
        ) {
          case (name, current) =>
            ReductionTreeUnoptimized.Application(
              ReductionTreeUnoptimized.U,
              abstractVar(name, current)
            )
        }

        Right(ReductionTreeUnoptimized.Application(
          mainExpr,
          ReductionTreeUnoptimized.Application(
            ReductionTreeUnoptimized.Y,
            bodyExpr
          )
        ))
    }
  }

  def compileStrict(tree: ParseTreeUnoptimized, scopes: Scopes, env: Map[String, ReductionTreeUnoptimized]): Either[CompilerError, ReductionTreeUnoptimized] = {
    tree match {
      case Const(c) => Right(ReductionTreeUnoptimized.Const(c))
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
        Right(ReductionTreeUnoptimized.Application(
          compileStrict(f, scopes, env) match {
            case Right(rt) => rt
            case Left(e) => return Left(e)
          },
          compileStrict(x, scopes, env) match {
            case Right(rt) => rt
            case Left(e) => return Left(e)
          }
        ))
      case Where(pt, fs) =>
        // U @ ([𝑓] (U @ [𝑔] (K @ 𝐸1))) @ (Y @ (U @ ([𝑓] (U @ [𝑔] (K @ [[𝑥] 𝐸2,[𝑦] 𝐸3])))))
        val functionNames = env.keys.toSet
        val bodyList = fs.foldRight(ReductionTreeUnoptimized.Const(Constant.Nil)) {
          case ((name, (pt, args)), current) =>
            val names = functionNames ++ args
            val compiledBody = compileLoose(pt, scopes, env, names) match {
              case Right(rt) => rt
              case Left(e) => return Left(e)
            }
            val abstracted = args.foldRight(compiledBody) { (arg, acc) => abstractVar(arg, acc) }
            ReductionTreeUnoptimized.Application(
              ReductionTreeUnoptimized.Application(
                ReductionTreeUnoptimized.Cons,
                abstracted
              ),
              current
            )
        }
        val bodyExpr = fs.map(f => f._1).foldRight(
          ReductionTreeUnoptimized.Application(
            ReductionTreeUnoptimized.K,
            bodyList
          )
        ) {
          case (name, current) =>
            ReductionTreeUnoptimized.Application(
              ReductionTreeUnoptimized.U,
              abstractVar(name, current)
            )
        }
        val mainExpr = fs.map(f => f._1).foldRight(
          ReductionTreeUnoptimized.Application(
            ReductionTreeUnoptimized.K,
            compileLoose(pt, scopes, env, functionNames) match {
              case Right(rt) => rt
              case Left(e) => return Left(e)
            }
          )
        ) {
          case (name, current) =>
            ReductionTreeUnoptimized.Application(
              ReductionTreeUnoptimized.U,
              abstractVar(name, current)
            )
        }

        Right(ReductionTreeUnoptimized.Application(
          mainExpr,
          ReductionTreeUnoptimized.Application(
            ReductionTreeUnoptimized.Y,
            bodyExpr
          )
        ))
    }
  }

  private def usesVar(name: String, tree: ReductionTreeUnoptimized): Boolean = tree match {
    case ReductionTreeUnoptimized.Unresolved(n) => n == name
    case ReductionTreeUnoptimized.Application(f, x) =>
      usesVar(name, f) || usesVar(name, x)
    case _ => false
  }

  def abstractVar(name: String, body: ReductionTreeUnoptimized): ReductionTreeUnoptimized = body match {
    case ReductionTreeUnoptimized.S
         | ReductionTreeUnoptimized.K
         | ReductionTreeUnoptimized.I
         | ReductionTreeUnoptimized.U
         | ReductionTreeUnoptimized.Y
         | ReductionTreeUnoptimized.Plus
         | ReductionTreeUnoptimized.Minus
         | ReductionTreeUnoptimized.Mul
         | ReductionTreeUnoptimized.Div
         | ReductionTreeUnoptimized.Leq
         | ReductionTreeUnoptimized.Lt
         | ReductionTreeUnoptimized.Geq
         | ReductionTreeUnoptimized.Gt
         | ReductionTreeUnoptimized.Neq
         | ReductionTreeUnoptimized.Eq
         | ReductionTreeUnoptimized.Const(_)
         | ReductionTreeUnoptimized.Cond
         | ReductionTreeUnoptimized.Cons
         | ReductionTreeUnoptimized.Hd
         | ReductionTreeUnoptimized.Tl
    =>
      ReductionTreeUnoptimized.Application(
        ReductionTreeUnoptimized.K,
        body
      )
    case ReductionTreeUnoptimized.Application(f, x) =>
      ReductionTreeUnoptimized.Application(
        ReductionTreeUnoptimized.Application(
          ReductionTreeUnoptimized.S,
          abstractVar(name, f)
        ),
        abstractVar(name, x)
      )
    case u @ ReductionTreeUnoptimized.Unresolved(n) =>
      if (n == name) {
        ReductionTreeUnoptimized.I
      } else {
        ReductionTreeUnoptimized.Application(
          ReductionTreeUnoptimized.K,
          u
        )
      }
    case ReductionTreeUnoptimized.Pair(_, _) =>
      // Should never occur, Pair should only exist after reduction
      assert(false)
      body
  }

  def compileProgram(
                      mainTree: ParseTreeUnoptimized,
                      scopes: Scopes,
                      varMap: VariableMapUnoptimized,
                    ): Either[CompilerError, ReductionTreeUnoptimized] = {
    val compiledDefs: Map[String, ReductionTreeUnoptimized.Application] = varMap.map {
      case (name, _) =>
        val rt: ReductionTreeUnoptimized.Application = ReductionTreeUnoptimized.Application(ReductionTreeUnoptimized.I, ReductionTreeUnoptimized.I)
        name -> rt
    }.toMap
    varMap.toList.foldLeft(()) {
      case (_, (name, (pt, args))) =>
        val compiledBody = compileLoose(pt, scopes, compiledDefs, args.toSet) match {
          case Right(rt) => rt
          case Left(e) => return Left(e)
        }
        val abstracted = args.foldRight(compiledBody) { (arg, acc) => abstractVar(arg, acc) }
        abstracted match {
          case ReductionTreeUnoptimized.Application(f, x) =>
            compiledDefs(name).operator = f
            compiledDefs(name).operand = x
          case x =>
            compiledDefs(name).operand = x
        }
    }

    compileStrict(mainTree, scopes, compiledDefs)
  }
}