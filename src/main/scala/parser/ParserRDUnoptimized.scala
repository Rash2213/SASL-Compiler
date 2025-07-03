package parser

import lexer.{PeekIterator, Token}
import lexer.Token.*
import SaslData.NonTerminal
import SaslData.NonTerminal.*

import scala.collection.mutable
import scala.annotation.tailrec

enum ParseTreeUnoptimized:
  case Const(c: Constant)
  case Ident(name: String, scope: Option[Int])
  case Application(operator: ParseTreeUnoptimized, operand: ParseTreeUnoptimized)
  case Where(mainTree: ParseTreeUnoptimized, functions: Array[(String, (ParseTreeUnoptimized, Array[String]))])

type VariableMapUnoptimized = mutable.Map[String, (ParseTreeUnoptimized, Array[String])]

import ParseTreeUnoptimized.*
import Constant.*

//  ⟨system⟩ → def id ⟨abstraction⟩ ⟨expr’⟩ ⟨funcdefs’⟩ . ⟨condexpr⟩ ⟨expr’⟩
//    | ⟨condexpr⟩ ⟨expr’⟩
def parserRDUnoptimizedSystem(
                    lexer: PeekIterator[Token],
                    first: ParserRD.gen.FirstMap,
                    variableMap: VariableMapUnoptimized,
                    scopes: Scopes,
                  ): Either[ParseError, ParseTreeUnoptimized] = {
  scopes.addOne(ScopeEntry(0))
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KDef.ordinal) {
        lexer.next()
        lexer.peek() match {
          case Some(Id(s)) =>
            lexer.next()
            scopes.addOne(ScopeEntry(0))
            val scopeA = scopes.length-1
            val pta = parserRDUnoptimizedAbstraction(lexer, first, variableMap, scopes, 0, scopeA) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            //variableMap("0" + s) = pta
            val aFuns = parserRDUnoptimizedExprP(lexer, first, variableMap, scopes, scopeA) match {
              case Right(funs) => funs
              case Left(e) => return Left(e)
            }
            variableMap("0" + s) = aFuns match {
              case None => pta
              case Some(funs) => (ParseTreeUnoptimized.Where(pta._1, funs), pta._2)
            }
            parserRDUnoptimizedFuncDefsP(lexer, first, variableMap, scopes) match {
              case Right(_) =>
              case Left(e) => return Left(e)
            }
            lexer.peek() match {
              case Some(KDot) => lexer.next()
              case Some(_) => return Left(ParseError.ToDo)
              case None => return Left(ParseError.ToDo)
            }
            scopes.addOne(ScopeEntry(0))
            val scopeM = scopes.length-1
            val ptc = parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, scopeM) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            val lFuns = parserRDUnoptimizedExprP(lexer, first, variableMap, scopes, scopeM) match {
              case Right(funs) => funs
              case Left(e) => return Left(e)
            }
            Right(lFuns match {
              case None => ptc
              case Some(funs) => ParseTreeUnoptimized.Where(ptc, funs)
            })
          case Some(_) => Left(ParseError.ToDo)
          case None => Left(ParseError.ToDo)
        }
      } else if (first(CondExpr.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        scopes.addOne(ScopeEntry(0))
        val scopeM = scopes.length - 1
        val ptc = parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, scopeM) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val lFuns = parserRDUnoptimizedExprP(lexer, first, variableMap, scopes, scopeM) match {
          case Right(funs) => funs
          case Left(e) => return Left(e)
        }
        Right(lFuns match {
          case None => ptc
          case Some(funs) => ParseTreeUnoptimized.Where(ptc, funs)
        })
      } else {
        Left(ParseError.ToDo)
      }
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨funcdefs’⟩ → def id ⟨abstraction⟩ ⟨expr’⟩ ⟨funcdefs’⟩
//    | ε
@tailrec
def parserRDUnoptimizedFuncDefsP(
                       lexer: PeekIterator[Token],
                       first: ParserRD.gen.FirstMap,
                       variableMap: VariableMapUnoptimized,
                       scopes: Scopes,
                     ): Either[ParseError, Unit] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KDef.ordinal) {
        lexer.next()
      } else {
        return Right(())
      }
      lexer.peek() match {
        case Some(Id(s)) =>
          lexer.next()
          scopes.addOne(ScopeEntry(0))
          val scopeA = scopes.length-1
          val pta = parserRDUnoptimizedAbstraction(lexer, first, variableMap, scopes, 0, scopeA) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          //variableMap("0" + s) = pta
          val lFuns = parserRDUnoptimizedExprP(lexer, first, variableMap, scopes, scopeA) match {
            case Right(opt) => opt
            case Left(e) => return Left(e)
          }
          variableMap("0" + s) = lFuns match {
            case None => pta
            case Some(funs) => (ParseTreeUnoptimized.Where(pta._1, funs), pta._2)
          }
          parserRDUnoptimizedFuncDefsP(lexer, first, variableMap, scopes)
        case _ => Left(ParseError.ToDo)
      }
    case None => Right(())
  }
}
//  ⟨expr’⟩ → where id ⟨abstraction⟩ ⟨defs’⟩
//    | ε
def parserRDUnoptimizedExprP(
                   lexer: PeekIterator[Token],
                   first: ParserRD.gen.FirstMap,
                   variableMap: VariableMapUnoptimized,
                   scopes: Scopes,
                   parentScope: Int,
                 ): Either[ParseError, Option[Array[(String, (ParseTreeUnoptimized, Array[String]))]]] = {
  /*scopes.addOne(ScopeEntry(parentScope))
  val scope = scopes.length-1*/
  val scope = parentScope
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KWhere.ordinal) {
        lexer.next()
      } else {
        return Right(None)
      }
      lexer.peek() match {
        case Some(Id(s)) =>
          lexer.next()
          scopes.addOne(ScopeEntry(scope))
          val scopeA = scopes.length-1
          val pta = parserRDUnoptimizedAbstraction(lexer, first, variableMap, scopes, parentScope, scopeA) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          //variableMap(scope.toString + s) = pta
          val lFuns: Array[(String, (ParseTreeUnoptimized, Array[String]))] = Array((scope.toString + s, pta))
          parserRDUnoptimizedDefsP(lexer, first, variableMap, scopes, scope, lFuns) match {
            case Right(funs) => Right(Some(funs))
            case Left(e) => return Left(e)
          }
        case _ => Left(ParseError.ToDo)
      }
    case None => Right(None)
  }
}
//  ⟨abstraction⟩ → = ⟨condexpr⟩
//    | id ⟨abstraction⟩
@tailrec
def parserRDUnoptimizedAbstraction(
                         lexer: PeekIterator[Token],
                         first: ParserRD.gen.FirstMap,
                         variableMap: VariableMapUnoptimized,
                         scopes: Scopes,
                       // ToDo: unnecessary?
                         parentScope: Int,
                         abstractionScope: Int,
                         al: Array[String] = Array(),
                       ): Either[ParseError, (ParseTreeUnoptimized, Array[String])] = {
  var argList: Array[String] = al
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SEqual =>
          lexer.next()
          parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, abstractionScope) match {
            case Right(pt) => Right(pt, argList)
            case Left(e) => Left(e)
          }
        case Id(s) =>
          argList = argList.appended(abstractionScope.toString + s)
          lexer.next()
          parserRDUnoptimizedAbstraction(lexer, first, variableMap, scopes, parentScope, abstractionScope, argList)
        case _ => Left(ParseError.ToDo)
      }
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨defs’⟩ → ; id ⟨abstraction⟩ ⟨defs’⟩
//    | ε
@tailrec
def parserRDUnoptimizedDefsP(
                   lexer: PeekIterator[Token],
                   first: ParserRD.gen.FirstMap,
                   variableMap: VariableMapUnoptimized,
                   scopes: Scopes,
                   scope: Int,
                   funs: Array[(String, (ParseTreeUnoptimized, Array[String]))],
                 ): Either[ParseError, Array[(String, (ParseTreeUnoptimized, Array[String]))]] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KSemicolon.ordinal) {
        lexer.next()
        lexer.peek() match {
          case Some(Id(s)) =>
            lexer.next()
            scopes.addOne(ScopeEntry(scope))
            val scopeA = scopes.length-1
            val pta = parserRDUnoptimizedAbstraction(lexer, first, variableMap, scopes, scope, scopeA) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            //variableMap(scope.toString + s) = pta
            val lFuns = funs :+ (scope.toString + s, pta)
            parserRDUnoptimizedDefsP(lexer, first, variableMap, scopes, scope, lFuns)
          case _ => Left(ParseError.ToDo)
        }
      } else {
        Right(funs)
      }
    case None => Right(funs)
  }
}
//  ⟨condexpr⟩ → if ⟨condexpr⟩ ⟨expr’⟩ then ⟨condexpr⟩ else ⟨condexpr⟩
//  | ⟨listexpr⟩
def parserRDUnoptimizedCondExpr(
                      lexer: PeekIterator[Token],
                      first: ParserRD.gen.FirstMap,
                      variableMap: VariableMapUnoptimized,
                      scopes: Scopes,
                      parentScope: Int,
                    ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KIf.ordinal) {
        lexer.next()
        scopes.addOne(ScopeEntry(parentScope))
        val s = scopes.length - 1
        val ptc = parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, s) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val lFuns = parserRDUnoptimizedExprP(lexer, first, variableMap, scopes, s) match {
          case Right(ofuns) => ofuns
          case Left(e) => return Left(e)
        }
        lexer.peek() match {
          case Some(KThen) => lexer.next()
          case Some(_) => return Left(ParseError.ToDo)
          case None => return Left(ParseError.ToDo)
        }
        val ptt = parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, parentScope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        lexer.peek() match {
          case Some(KElse) => lexer.next()
          case Some(_) => return Left(ParseError.ToDo)
          case None => return Left(ParseError.ToDo)
        }
        val pte = parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, parentScope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        Right(
          Application(
            Application(
              Application(
                Ident("cond", None),
                lFuns match {
                  case None => ptc
                  case Some(funs) => ParseTreeUnoptimized.Where(ptc, funs)
                }
              ),
              ptt
            ),
            pte
          )
        )
      } else if (first(ListExpr.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        parserRDUnoptimizedListExpr(lexer, first, variableMap, scopes, parentScope)
      } else {
        Left(ParseError.ToDo)
      }
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨listexpr⟩ → ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩ ⟨conjunct’⟩ ⟨opexpr’⟩ ⟨listexpr’⟩
def parserRDUnoptimizedListExpr(
                      lexer: PeekIterator[Token],
                      first: ParserRD.gen.FirstMap,
                      variableMap: VariableMapUnoptimized,
                      scopes: Scopes,
                      scope: Int,
                    ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      val f = parserRDUnoptimizedFactor(lexer, first, variableMap, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val mpt = parserRDUnoptimizedMulP(lexer, first, variableMap, f, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val apt = parserRDUnoptimizedAddP(lexer, first, variableMap, mpt, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val cpt = parserRDUnoptimizedComparP(lexer, first, variableMap, apt, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val cjpt = parserRDUnoptimizedConjunctP(lexer, first, variableMap, cpt, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val opt = parserRDUnoptimizedOpExprP(lexer, first, variableMap, cjpt, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      parserRDUnoptimizedListExprP(lexer, first, variableMap, opt, scopes, scope)
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨listexpr’⟩ → : ⟨listexpr⟩
//    | ε
def parserRDUnoptimizedListExprP(
                       lexer: PeekIterator[Token],
                       first: ParserRD.gen.FirstMap,
                       variableMap: VariableMapUnoptimized,
                       lhs: ParseTreeUnoptimized,
                       scopes: Scopes,
                       scope: Int,
                     ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KColon.ordinal) {
        lexer.next()
        val rhs = parserRDUnoptimizedListExpr(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val pt = Application(
          Application(
            Ident("cons", None),
            lhs
          ),
          rhs
        )
        Right(pt)
      } else {
        Right(lhs)
      }
    case None => Right(lhs)
  }
}
//  ⟨opexpr’⟩ → or ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩ ⟨conjunct’⟩ ⟨opexpr’⟩
//    | ε
@tailrec
def parserRDUnoptimizedOpExprP(
                     lexer: PeekIterator[Token],
                     first: ParserRD.gen.FirstMap,
                     variableMap: VariableMapUnoptimized,
                     lhs: ParseTreeUnoptimized,
                     scopes: Scopes,
                     scope: Int,
                   ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == SOr.ordinal) {
        lexer.next()
        val f = parserRDUnoptimizedFactor(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val mpt = parserRDUnoptimizedMulP(lexer, first, variableMap, f, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val apt = parserRDUnoptimizedAddP(lexer, first, variableMap, mpt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cpt = parserRDUnoptimizedComparP(lexer, first, variableMap, apt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cjpt = parserRDUnoptimizedConjunctP(lexer, first, variableMap, cpt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val pt = Application(
          Application(
            Ident("or", None),
            lhs
          ),
          cjpt
        )
        parserRDUnoptimizedOpExprP(lexer, first, variableMap, pt, scopes, scope)
      } else {
        Right(lhs)
      }
    case None =>
      Right(lhs)
  }
}
//  ⟨conjunct’⟩ → and ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩ ⟨conjunct’⟩
//    | ε
@tailrec
def parserRDUnoptimizedConjunctP(
                       lexer: PeekIterator[Token],
                       first: ParserRD.gen.FirstMap,
                       variableMap: VariableMapUnoptimized,
                       lhs: ParseTreeUnoptimized,
                       scopes: Scopes,
                       scope: Int,
                     ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == SAnd.ordinal) {
        lexer.next()
        val f = parserRDUnoptimizedFactor(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val mpt = parserRDUnoptimizedMulP(lexer, first, variableMap, f, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val apt = parserRDUnoptimizedAddP(lexer, first, variableMap, mpt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cpt = parserRDUnoptimizedComparP(lexer, first, variableMap, apt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val pt = Application(
          Application(
            Ident("and", None),
            lhs
          ),
          cpt
        )
        parserRDUnoptimizedConjunctP(lexer, first, variableMap, pt, scopes, scope)
      } else {
        Right(lhs)
      }
    case None => Right(lhs)
  }
}
//  ⟨compar’⟩ → = ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
//    | ~= ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
//    | < ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
//    | > ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
//    | <= ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
//    | >= ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩
//    | ε
@tailrec
def parserRDUnoptimizedComparP(
                     lexer: PeekIterator[Token],
                     first: ParserRD.gen.FirstMap,
                     variableMap: VariableMapUnoptimized,
                     lhs: ParseTreeUnoptimized,
                     scopes: Scopes,
                     scope: Int,
                   ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case t @ (SEqual | SNotEqual | SLess | SGreater | SLessEqual | SGreaterEqual) =>
          lexer.next()
          val f = parserRDUnoptimizedFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserRDUnoptimizedMulP(lexer, first, variableMap, f, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val apt = parserRDUnoptimizedAddP(lexer, first, variableMap, mpt, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              t match {
                case SEqual =>
                  Ident("eq", None)
                case SNotEqual =>
                  Ident("neq", None)
                case SLess =>
                  Ident("lt", None)
                case SGreater =>
                  Ident("gt", None)
                case SLessEqual =>
                  Ident("leq", None)
                case SGreaterEqual =>
                  Ident("geq", None)
              },
              lhs
            ),
            apt
          )
          parserRDUnoptimizedComparP(lexer, first, variableMap, pt, scopes, scope)
        case _ =>
          Right(lhs)
      }
    case None =>
      Right(lhs)
  }
}
//  ⟨add’⟩ → + ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩
//    | - ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩
//    | ε
@tailrec
def parserRDUnoptimizedAddP(
                  lexer: PeekIterator[Token],
                  first: ParserRD.gen.FirstMap,
                  variableMap: VariableMapUnoptimized,
                  lhs: ParseTreeUnoptimized,
                  scopes: Scopes,
                  scope: Int,
                ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SPlus =>
          lexer.next()
          val f = parserRDUnoptimizedFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserRDUnoptimizedMulP(lexer, first, variableMap, f, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              Ident("plus", None),
              lhs,
            ),
            mpt
          )
          parserRDUnoptimizedAddP(lexer, first, variableMap, pt, scopes, scope)
        case SMinus =>
          lexer.next()
          val f = parserRDUnoptimizedFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserRDUnoptimizedMulP(lexer, first, variableMap, f, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              Ident("minus", None),
              lhs,
            ),
            mpt
          )
          parserRDUnoptimizedAddP(lexer, first, variableMap, pt, scopes, scope)
        case _ =>
          Right(lhs)
      }
    case None =>
      Right(lhs)
  }
}
//  ⟨mul’⟩ → * ⟨factor⟩ ⟨mul’⟩
//    | / ⟨factor⟩ ⟨mul’⟩
//    | ε
@tailrec
def parserRDUnoptimizedMulP(
                  lexer: PeekIterator[Token],
                  first: ParserRD.gen.FirstMap,
                  variableMap: VariableMapUnoptimized,
                  lhs: ParseTreeUnoptimized,
                  scopes: Scopes,
                  scope: Int,
                ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SMul =>
          lexer.next()
          val f = parserRDUnoptimizedFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              Ident("mul", None),
              lhs
            ),
            f
          )
          parserRDUnoptimizedMulP(lexer, first, variableMap, pt, scopes, scope)
        case SDiv =>
          lexer.next()
          val f = parserRDUnoptimizedFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              Ident("div", None),
              lhs
            ),
            f
          )
          parserRDUnoptimizedMulP(lexer, first, variableMap, pt, scopes, scope)
        case _ =>
          Right(lhs)
      }
    case None =>
      Right(lhs)
  }
}
//  ⟨factor⟩ → + ⟨simple⟩ ⟨comb’⟩
//    | - ⟨simple⟩ ⟨comb’⟩
//    | not ⟨simple⟩ ⟨comb’⟩
//    | ⟨simple⟩ ⟨comb’⟩
def parserRDUnoptimizedFactor(
                    lexer: PeekIterator[Token],
                    first: ParserRD.gen.FirstMap,
                    variableMap: VariableMapUnoptimized,
                    scopes: Scopes,
                    scope: Int,
                  ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SPlus =>
          lexer.next()
          val s = parserRDUnoptimizedSimple(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserRDUnoptimizedCombP(lexer, first, variableMap, scopes, scope, s) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          // prefix + does nothing
          return Right(cp)
        case SMinus =>
          lexer.next()
          val s = parserRDUnoptimizedSimple(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserRDUnoptimizedCombP(lexer, first, variableMap, scopes, scope, s) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          return Right(
            Application(
              Ident("inv", None),
              cp,
            )
          )
        case SNot =>
          lexer.next()
          val s = parserRDUnoptimizedSimple(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserRDUnoptimizedCombP(lexer, first, variableMap, scopes, scope, s) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          return Right(
            Application(
              Ident("not", None),
              cp,
            )
          )
        case _ =>
      }
      if(first(Simple.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val s = parserRDUnoptimizedSimple(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cp = parserRDUnoptimizedCombP(lexer, first, variableMap, scopes, scope, s) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        Right(cp)
      } else {
        Left(ParseError.ToDo)
      }
    case None =>
      Left(ParseError.ToDo)
  }
}

//  ⟨comb’⟩ → ⟨simple⟩ ⟨comb’⟩
//    | ε
@tailrec
def parserRDUnoptimizedCombP(
                   lexer: PeekIterator[Token],
                   first: ParserRD.gen.FirstMap,
                   variableMap: VariableMapUnoptimized,
                   scopes: Scopes,
                   scope: Int,
                   ptSoFar: ParseTreeUnoptimized,
                 ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      if (first(Simple.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val s = parserRDUnoptimizedSimple(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserRDUnoptimizedCombP(
          lexer,
          first,
          variableMap,
          scopes,
          scope,
          Application(
            ptSoFar,
            s
          )
        )
      } else {
        Right(ptSoFar)
      }
    case None =>
      Right(ptSoFar)
  }
}

//  ⟨simple⟩ → id
//    | num
//    | bool
//    | string
//    | nil
//    | [ ⟨list’⟩
//    | ( ⟨condexpr⟩ ⟨expr’⟩ )
def parserRDUnoptimizedSimple(
                    lexer: PeekIterator[Token],
                    first: ParserRD.gen.FirstMap,
                    variableMap: VariableMapUnoptimized,
                    scopes: Scopes,
                    scope: Int,
                  ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case Id(s) =>
          lexer.next()
          Right(Ident(s, Some(scope)))
        case CNum(n) =>
          lexer.next()
          Right(Const(Num(n)))
        case CBool(b) =>
          lexer.next()
          Right(Const(Bool(b)))
        case CString(s) =>
          lexer.next()
          Right(Const(Str(s)))
        case CNil =>
          lexer.next()
          Right(Const(Nil))
        case KOpenBracket =>
          lexer.next()
          parserRDUnoptimizedListP(lexer, first, variableMap, scopes, scope)
        case KOpenParen =>
          scopes.addOne(ScopeEntry(scope))
          val s = scopes.length - 1
          lexer.next()
          val ptc = parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, s) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val lFuns = parserRDUnoptimizedExprP(lexer, first, variableMap, scopes, s) match {
            case Right(funs) => funs
            case Left(e) => return Left(e)
          }
          lexer.peek() match {
            case Some(KCloseParen) => lexer.next()
            case Some(_) => return Left(ParseError.ToDo)
            case None => return Left(ParseError.ToDo)
          }
          Right(lFuns match {
            case None => ptc
            case Some(funs) => ParseTreeUnoptimized.Where(ptc, funs)
          })
        case _ =>
          Left(ParseError.ToDo)
      }
    case None =>
      Left(ParseError.ToDo)
  }
}
//  ⟨list’⟩ → ]
//    | ⟨condexpr⟩ ⟨expr’⟩ ⟨listelems’⟩ ]
def parserRDUnoptimizedListP(
                   lexer: PeekIterator[Token],
                   first: ParserRD.gen.FirstMap,
                   variableMap: VariableMapUnoptimized,
                   scopes: Scopes,
                   scope: Int,
                 ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KCloseBracket.ordinal) {
        lexer.next()
        Right(Const(Nil))
      } else if (first(CondExpr.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        scopes.addOne(ScopeEntry(scope))
        val s = scopes.length - 1
        val ptc = parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, s) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val lFuns = parserRDUnoptimizedExprP(lexer, first, variableMap, scopes, s) match {
          case Right(funs) => funs
          case Left(e) => return Left(e)
        }
        val pt = parserRDUnoptimizedListElemsP(lexer, first, variableMap, scopes, scope) match {
          case Left(e) => return Left(e)
          case Right(pt) => Application(
            Application(
              Ident("cons", None),
              ptc
            ),
            pt
          )
        }
        lexer.peek() match {
          case Some(KCloseBracket) =>
            lexer.next()
            Right(lFuns match {
              case None => pt
              case Some(funs) => ParseTreeUnoptimized.Where(pt, funs)
            })
          case Some(_) => Left(ParseError.ToDo)
          case None => Left(ParseError.ToDo)
        }
      } else {
        Left(ParseError.ToDo)
      }
    case None =>
      Left(ParseError.ToDo)
  }
}
//  ⟨listelems’⟩ → , ⟨condexpr⟩ ⟨expr’⟩ ⟨listelems’⟩
//    | ε
def parserRDUnoptimizedListElemsP(
                        lexer: PeekIterator[Token],
                        first: ParserRD.gen.FirstMap,
                        variableMap: VariableMapUnoptimized,
                        scopes: Scopes,
                        scope: Int
                      ): Either[ParseError, ParseTreeUnoptimized] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KComma.ordinal) {
        lexer.next()
        scopes.addOne(ScopeEntry(scope))
        val s = scopes.length - 1
        val ptc = parserRDUnoptimizedCondExpr(lexer, first, variableMap, scopes, s) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val lFuns = parserRDUnoptimizedExprP(lexer, first, variableMap, scopes, s) match {
          case Right(funs) => funs
          case Left(e) => return Left(e)
        }
        parserRDUnoptimizedListElemsP(lexer, first, variableMap, scopes, scope) match {
          case Left(e) => Left(e)
          case Right(pt) => Right(Application(
            Application(
              Ident("cons", None),
              lFuns match {
                case None => ptc
                case Some(funs) => ParseTreeUnoptimized.Where(ptc, funs)
              }
            ),
            pt
          ))
        }
      } else {
        Right(Const(Constant.Nil))
      }
    case None =>
      Left(ParseError.ToDo)
  }
}