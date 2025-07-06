package compiler

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.all.*
import compiler.CompilerError.UnresolvedVariable
import vm.{Ownership, ReductionTreeUnoptimized}
import vm.Ownership.*
import parser.{Constant, ParseTreeUnoptimized, ScopesRes, VariableMapUnoptimizedRes}
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
    "and" -> ReductionTreeUnoptimized.And,
    "or" -> ReductionTreeUnoptimized.Or,
    /*"not" -> CombTree.Ident("not"),
    "inv" -> CombTree.Ident("inv"),*/
    "cond" -> ReductionTreeUnoptimized.Cond,
    "cons" -> ReductionTreeUnoptimized.Cons,
    "tl" -> ReductionTreeUnoptimized.Tl,
    "hd" -> ReductionTreeUnoptimized.Hd,
  )

  def resolveVarScopedName(name: String, scope: Int, scopes: ScopesRes, varNames: Set[String]): Option[String] = {
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

  def resolveVarScopedArg(name: String, scope: Int, scopes: ScopesRes, envMap: Map[String, ReductionTreeUnoptimized], args: Set[String]): Option[Ownership[ReductionTreeUnoptimized]] = {
    var s = scope
    while (s != 0) {
      envMap.get(s.toString + name) match {
        case Some(v) => return Some(Weak(v))
        case None =>
      }
      if (args.contains(s.toString + name)) {
        return Some(Strong(ReductionTreeUnoptimized.Unresolved(s.toString + name)))
      }
      s = scopes(s).parent
    }
    envMap.get("0" + name) match {
      case Some(v) => Some(Weak(v))
      case None => None
    }
  }

  def resolveVarScoped(name: String, scope: Int, scopes: ScopesRes, envMap: Map[String, ReductionTreeUnoptimized]): Option[ReductionTreeUnoptimized] = {
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

  def compileLoose(tree: ParseTreeUnoptimized, scopes: ScopesRes, envMap: Map[String, ReductionTreeUnoptimized], argNames: Set[String]): Either[CompilerError, Ownership[ReductionTreeUnoptimized]] = {
    tree match {
      case Const(c) => Right(Strong(ReductionTreeUnoptimized.Const(c)))
      case Application(f, x) =>
        Right(Strong(ReductionTreeUnoptimized.Application(
          compileLoose(f, scopes, envMap, argNames) match {
            case Left(e) => return Left(e)
            case Right(v) => v
          },
          compileLoose(x, scopes, envMap, argNames) match {
            case Left(e) => return Left(e)
            case Right(v) => v
          }
        )))
      case Ident(name, scope) =>
        scope match {
          case Some(scope) =>
            resolveVarScopedArg(name, scope, scopes, envMap, argNames) match {
              case None => builtins.get(name) match {
                case None => Left(UnresolvedVariable(name))
                case Some(v) => Right(Strong(v))
              }
              case Some(rt) =>
                Right(rt)
            }
          case None =>
            builtins.get(name) match {
              case None => Left(UnresolvedVariable(name))
              case Some(v) => Right(Strong(v))
            }
        }
      case Where(pt, fs) =>
        // U @ ([𝑓] (U @ [𝑔] (K @ 𝐸1))) @ (Y @ (U @ ([𝑓] (U @ [𝑔] (K @ [[𝑥] 𝐸2,[𝑦] 𝐸3])))))
        val fNames = fs.map(x => x._1)

        val bodyList = fs.foldRight(ReductionTreeUnoptimized.Const(Constant.Nil)) {
          case ((name, (pt, args)), current) =>
            val names = argNames ++ args ++ fNames
            val compiledBody = compileLoose(pt, scopes, envMap, names) match {
              case Right(rt) => rt
              case Left(e) => return Left(e)
            }
            val abstracted = args.foldRight(compiledBody) { (arg, acc) => abstractVarOwnership(arg, acc) }
            ReductionTreeUnoptimized.Application(
              Strong(ReductionTreeUnoptimized.Application(
                Strong(ReductionTreeUnoptimized.Cons),
                abstracted
              )),
              Strong(current)
            )
        }
        val bodyExpr = fs.map(f => f._1).foldRight(
          ReductionTreeUnoptimized.Application(
            Strong(ReductionTreeUnoptimized.K),
            Strong(bodyList)
          )
        ) {
          case (name, current) =>
            ReductionTreeUnoptimized.Application(
              Strong(ReductionTreeUnoptimized.U),
              Strong(abstractVar(name, current))
            )
        }
        val mainExpr = fs.map(f => f._1).foldRight(
          ReductionTreeUnoptimized.Application(
            Strong(ReductionTreeUnoptimized.K),
            compileLoose(pt, scopes, envMap, argNames ++ fNames) match {
              case Right(rt) => rt
              case Left(e) => return Left(e)
            }
          )
        ) {
          case (name, current) =>
            ReductionTreeUnoptimized.Application(
              Strong(ReductionTreeUnoptimized.U),
              Strong(abstractVar(name, current))
            )
        }

        Right(Strong(ReductionTreeUnoptimized.Application(
          Strong(mainExpr),
          Strong(ReductionTreeUnoptimized.Application(
            Strong(ReductionTreeUnoptimized.Y),
            Strong(bodyExpr)
          ))
        )))
    }
  }

  def compileStrict(tree: ParseTreeUnoptimized, scopes: ScopesRes, env: Map[String, ReductionTreeUnoptimized]): Either[CompilerError, ReductionTreeUnoptimized] = {
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
            case Right(rt) => Strong(rt)
            case Left(e) => return Left(e)
          },
          compileStrict(x, scopes, env) match {
            case Right(rt) => Strong(rt)
            case Left(e) => return Left(e)
          }
        ))
      case Where(pt, fs) =>
        // U @ ([𝑓] (U @ [𝑔] (K @ 𝐸1))) @ (Y @ (U @ ([𝑓] (U @ [𝑔] (K @ [[𝑥] 𝐸2,[𝑦] 𝐸3])))))
        val lFNames = fs.map(x => x._1)

        val functionNames = env.keys.toSet
        val bodyList = fs.foldRight(ReductionTreeUnoptimized.Const(Constant.Nil)) {
          case ((name, (pt, args)), current) =>
            val names = functionNames ++ args ++ lFNames
            val compiledBody = compileLoose(pt, scopes, env, names) match {
              case Right(rt) => rt
              case Left(e) => return Left(e)
            }
            val abstracted = args.foldRight(compiledBody) { (arg, acc) => abstractVarOwnership(arg, acc) }
            ReductionTreeUnoptimized.Application(
              Strong(ReductionTreeUnoptimized.Application(
                Strong(ReductionTreeUnoptimized.Cons),
                abstracted
              )),
              Strong(current)
            )
        }
        val bodyExpr = fs.map(f => f._1).foldRight(
          Strong(ReductionTreeUnoptimized.Application(
            Strong(ReductionTreeUnoptimized.K),
            Strong(bodyList)
          ))
        ) {
          case (name, current) =>
            Strong(ReductionTreeUnoptimized.Application(
              Strong(ReductionTreeUnoptimized.U),
              Strong(abstractVar(name, current.inner))
            ))
        }
        val mainExpr = fs.map(f => f._1).foldRight(
          ReductionTreeUnoptimized.Application(
            Strong(ReductionTreeUnoptimized.K),
            compileLoose(pt, scopes, env, functionNames ++ lFNames) match {
              case Right(rt) => rt
              case Left(e) => return Left(e)
            }
          )
        ) {
          case (name, current) =>
            ReductionTreeUnoptimized.Application(
              Strong(ReductionTreeUnoptimized.U),
              Strong(abstractVar(name, current))
            )
        }

        Right(ReductionTreeUnoptimized.Application(
          Strong(mainExpr),
          Strong(ReductionTreeUnoptimized.Application(
            Strong(ReductionTreeUnoptimized.Y),
            bodyExpr
          ))
        ))
    }
  }

  // ToDo: careful, currently can only be used on tree, later perhaps just don't follow weak ownership
  private def usesVar(name: String, tree: ReductionTreeUnoptimized): Boolean = tree match {
    case ReductionTreeUnoptimized.Unresolved(n) => n == name
    case ReductionTreeUnoptimized.Application(f, x) =>
      usesVar(name, f.inner) || usesVar(name, x.inner)
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
         | ReductionTreeUnoptimized.And
         | ReductionTreeUnoptimized.Or
         | ReductionTreeUnoptimized.Const(_)
         | ReductionTreeUnoptimized.Cond
         | ReductionTreeUnoptimized.Cons
         | ReductionTreeUnoptimized.Hd
         | ReductionTreeUnoptimized.Tl
    =>
      ReductionTreeUnoptimized.Application(
        Strong(ReductionTreeUnoptimized.K),
        Strong(body)
      )
    case ReductionTreeUnoptimized.Application(f, x) =>
      // only continue abstraction wherever the reference is not weak
      val fa = f match {
        case Weak(_) => Strong(ReductionTreeUnoptimized.Application(
          Strong(ReductionTreeUnoptimized.K),
          f
        ))
        case Strong(i) => Strong(abstractVar(name, i))
      }
      val xa = x match {
        case Weak(_) => Strong(ReductionTreeUnoptimized.Application(
          Strong(ReductionTreeUnoptimized.K),
          x
        ))
        case Strong(i) => Strong(abstractVar(name, i))
      }
      ReductionTreeUnoptimized.Application(
        Strong(ReductionTreeUnoptimized.Application(
          Strong(ReductionTreeUnoptimized.S),
          fa
        )),
        xa
      )
    case u@ReductionTreeUnoptimized.Unresolved(n) =>
      if (n == name) {
        ReductionTreeUnoptimized.I
      } else {
        ReductionTreeUnoptimized.Application(
          Strong(ReductionTreeUnoptimized.K),
          Strong(u)
        )
      }
    case ReductionTreeUnoptimized.Pair(_, _) =>
      // Should never occur, Pair should only exist after reduction
      assert(false)
      body
  }

  private def abstractVarInPlace(name: String, body: ReductionTreeUnoptimized.Application): Unit = {
    /*body.operator match {
      case f @ ReductionTreeUnoptimized.Application => abstractVarInPlace(name, body)
      case _ => body.operator =
    }
    // ToDo: check if we might need to carry weak through somewhere
    body.operator = Strong(abstractVar(name, body.operator.inner))
    body.operand = Strong(abstractVar(name, body.operand.inner))*/
    body.operand match {
      case Weak(_) =>
      case Strong(i) => body.operand = Strong(abstractVar(name, i))
    }
    body.operator = Strong(ReductionTreeUnoptimized.Application(
      Strong(ReductionTreeUnoptimized.S),
      body.operator match {
        case Weak(_) => body.operator
        case Strong(i) => Strong(abstractVar(name, i))
      }
    ))
  }

  private def abstractVarOwnership(name: String, body: Ownership[ReductionTreeUnoptimized]): Ownership[ReductionTreeUnoptimized] = {
    body match {
      case Weak(_) => body
      case Strong(i) => Strong(abstractVar(name, i))
    }
  }

  def compileProgram(
                      mainTree: ParseTreeUnoptimized,
                      scopes: ScopesRes,
                      varMap: VariableMapUnoptimizedRes // Assuming VariableMapUnoptimizedRes is Map[String, (ParseTreeUnoptimized, List[String])]
                    ): IO[Either[CompilerError, ReductionTreeUnoptimized]] = {
    val compiledDefs: Map[String, ReductionTreeUnoptimized.Application] = varMap.map {
      case (name, _) =>
        val rt: ReductionTreeUnoptimized.Application = ReductionTreeUnoptimized.Application(
          Strong(ReductionTreeUnoptimized.I),
          Strong(ReductionTreeUnoptimized.I),
        )
        name -> rt
    }

    def compileSingleDefinition(
                                 name: String,
                                 pt: ParseTreeUnoptimized,
                                 args: Array[String]
                               ): IO[Either[CompilerError, Unit]] = {
      IO.delay(compileLoose(pt, scopes, compiledDefs, args.toSet)).flatMap {
        case Left(e) => IO.pure(Left(e))
        case Right(compiledBody) =>
          compiledBody match {
            case Strong(ReductionTreeUnoptimized.Application(f, x)) =>
              compiledDefs(name).operator = f
              compiledDefs(name).operand = x
              IO.unit.as(Right(()))
            case x =>
              compiledDefs(name).operand = x
              IO.unit.as(Right(()))
          }
      }.flatMap {
        case Left(e) => IO.pure(Left(e))
        case Right(_) =>
          IO.delay {
              args.reverseIterator.foreach { arg => abstractVarInPlace(arg, compiledDefs(name)) }
            }.as(Right(()))
      }
    }

    val compileRes: IO[Either[CompilerError, Unit]] =
      varMap.toList
        .parTraverse { case (name, (pt, args)) =>
          compileSingleDefinition(name, pt, args)
        }
        .map { (results: List[Either[CompilerError, Unit]]) =>
          results.collectFirst { case Left(e) => e } match {
            case Some(err) => Left(err)
            case None => Right(())
          }
        }

    (for {
      _ <- EitherT(compileRes)
      mainResult <- EitherT.fromEither[IO](compileStrict(mainTree, scopes, compiledDefs))
        .leftMap(identity)
    } yield mainResult).value
  }
}
