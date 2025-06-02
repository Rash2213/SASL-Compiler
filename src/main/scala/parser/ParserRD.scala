package parser

import lexer.{PeekIterator, Token}
import lexer.Token.*
import SaslData.NonTerminal
import SaslData.NonTerminal.*

import scala.annotation.tailrec
import scala.collection.mutable

enum Constant:
  case Num(n: Long)
  case Bool(b: Boolean)
  case Str(s: String)
  case Nil

enum ParseTree:
  case Const(c: Constant)
  case Ident(name: String)
  case Application(operator: ParseTree, operand: ParseTree)

enum ParseError:
  case ToDo

object ParserRD:
  val gen: ParserGenerator[Token, NonTerminal] = ParserGenerator()

type VariableMap = mutable.Map[String, (ParseTree, Array[String])]
case class ScopeEntry(parent: Int)
type Scopes = mutable.ArrayBuffer[ScopeEntry]

import ParseTree.*
import Constant.*

// variableMap is a map from a name to a ParseTree, we prepend the scope to the variable name
// case class ParserResult(variableMap: Map[String, ParseTree], pTree: ParseTree)

//  ⟨system⟩ → def id ⟨abstraction⟩ ⟨expr’⟩ ⟨funcdefs’⟩ . ⟨condexpr⟩ ⟨expr’⟩
//    | ⟨condexpr⟩ ⟨expr’⟩
def parserRDSystem(
                    lexer: PeekIterator[Token],
                    first: ParserRD.gen.FirstMap,
                    variableMap: VariableMap,
                    scopes: Scopes,
                    ): Either[ParseError, ParseTree] = {
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
            val pta = parserRDAbstraction(lexer, first, variableMap, scopes, 0, scopeA) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            variableMap("0" + s) = pta
            parserRDExprP(lexer, first, variableMap, scopes, 0) match {
              case Right(_) =>
              case Left(e) => return Left(e)
            }
            parserRDFuncDefsP(lexer, first, variableMap, scopes) match {
              case Right(_) =>
              case Left(e) => return Left(e)
            }
            lexer.peek() match {
              case Some(KDot) => lexer.next()
              case Some(_) => return Left(ParseError.ToDo)
              case None => return Left(ParseError.ToDo)
            }
            val ptc = parserRDCondExpr(lexer, first, variableMap, scopes, 0) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            parserRDExprP(lexer, first, variableMap, scopes, 0) match {
              case Right(_) =>
              case Left(e) => return Left(e)
            }
            Right(ptc)
          case Some(_) => Left(ParseError.ToDo)
          case None => Left(ParseError.ToDo)
        }
      } else if (first(CondExpr.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val ptc = parserRDCondExpr(lexer, first, variableMap, scopes, 0) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserRDExprP(lexer, first, variableMap, scopes, 0) match {
          case Right(_) =>
          case Left(e) => return Left(e)
        }
        Right(ptc)
      } else {
        Left(ParseError.ToDo)
      }
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨funcdefs’⟩ → def id ⟨abstraction⟩ ⟨expr’⟩ ⟨funcdefs’⟩
//    | ε
@tailrec
def parserRDFuncDefsP(
                       lexer: PeekIterator[Token],
                       first: ParserRD.gen.FirstMap,
                       variableMap: VariableMap,
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
          val pta = parserRDAbstraction(lexer, first, variableMap, scopes, 0, scopeA) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          variableMap("0" + s) = pta
          parserRDExprP(lexer, first, variableMap, scopes, 0) match {
            case Right(_) =>
            case Left(e) => return Left(e)
          }
          parserRDFuncDefsP(lexer, first, variableMap, scopes)
        case _ => Left(ParseError.ToDo)
      }
    case None => Right(())
  }
}
//  ⟨expr’⟩ → where id ⟨abstraction⟩ ⟨defs’⟩
//    | ε
def parserRDExprP(
                   lexer: PeekIterator[Token],
                   first: ParserRD.gen.FirstMap,
                   variableMap: VariableMap,
                   scopes: Scopes,
                   parentScope: Int,
                   ): Either[ParseError, Unit] = {
  scopes.addOne(ScopeEntry(parentScope))
  val scope = scopes.length-1
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KWhere.ordinal) {
        lexer.next()
      } else {
        return Right(())
      }
      lexer.peek() match {
        case Some(Id(s)) =>
          lexer.next()
          scopes.addOne(ScopeEntry(scope))
          val scopeA = scopes.length-1
          val pta = parserRDAbstraction(lexer, first, variableMap, scopes, parentScope, scopeA) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          variableMap(scope.toString + s) = pta
          parserRDDefsP(lexer, first, variableMap, scopes, scope)
        case _ => Left(ParseError.ToDo)
      }
    case None => Right(())
  }
}
//  ⟨abstraction⟩ → = ⟨condexpr⟩
//    | id ⟨abstraction⟩
@tailrec
def parserRDAbstraction(
                         lexer: PeekIterator[Token],
                         first: ParserRD.gen.FirstMap,
                         variableMap: VariableMap,
                         scopes: Scopes,
                         parentScope: Int,
                         abstractionScope: Int,
                         al: Array[String] = Array(),
                         ): Either[ParseError, (ParseTree, Array[String])] = {
  var argList: Array[String] = al
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SEqual =>
          lexer.next()
          parserRDCondExpr(lexer, first, variableMap, scopes, parentScope) match {
            case Right(pt) => Right(pt, argList)
            case Left(e) => Left(e)
          }
        case Id(s) =>
          argList = argList.appended(abstractionScope.toString + s)
          lexer.next()
          parserRDAbstraction(lexer, first, variableMap, scopes, parentScope, abstractionScope, argList)
        case _ => Left(ParseError.ToDo)
      }
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨defs’⟩ → ; id ⟨abstraction⟩ ⟨defs’⟩
//    | ε
@tailrec
def parserRDDefsP(
                   lexer: PeekIterator[Token],
                   first: ParserRD.gen.FirstMap,
                   variableMap: VariableMap,
                   scopes: Scopes,
                   scope: Int,
                   ): Either[ParseError, Unit] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KSemicolon.ordinal) {
        lexer.next()
        lexer.peek() match {
          case Some(Id(s)) =>
            lexer.next()
            scopes.addOne(ScopeEntry(scope))
            val scopeA = scopes.length-1
            val pta = parserRDAbstraction(lexer, first, variableMap, scopes, scope, scopeA) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            variableMap(scope.toString + s) = pta
            parserRDDefsP(lexer, first, variableMap, scopes, scope)
          case _ => Left(ParseError.ToDo)
        }
      } else {
        Right(())
      }
    case None => Right(())
  }
}
//  ⟨condexpr⟩ → if ⟨condexpr⟩ ⟨expr’⟩ then ⟨condexpr⟩ else ⟨condexpr⟩
//  | ⟨listexpr⟩
def parserRDCondExpr(
                      lexer: PeekIterator[Token],
                      first: ParserRD.gen.FirstMap,
                      variableMap: VariableMap,
                      scopes: Scopes,
                      parentScope: Int,
                      ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KIf.ordinal) {
        lexer.next()
        val ptc = parserRDCondExpr(lexer, first, variableMap, scopes, parentScope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserRDExprP(lexer, first, variableMap, scopes, parentScope) match {
          case Right(_) =>
          case Left(e) => return Left(e)
        }
        lexer.peek() match {
          case Some(KThen) => lexer.next()
          case Some(_) => return Left(ParseError.ToDo)
          case None => return Left(ParseError.ToDo)
        }
        val ptt = parserRDCondExpr(lexer, first, variableMap, scopes, parentScope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        lexer.peek() match {
          case Some(KElse) => lexer.next()
          case Some(_) => return Left(ParseError.ToDo)
          case None => return Left(ParseError.ToDo)
        }
        val pte = parserRDCondExpr(lexer, first, variableMap, scopes, parentScope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        Right(
          Application(
            Application(
              Application(
                Ident("cond"),
                ptc
              ),
              ptt
            ),
            pte
          )
        )
      } else if (first(ListExpr.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        parserRDListExpr(lexer, first, variableMap, scopes, parentScope)
      } else {
        Left(ParseError.ToDo)
      }
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨listexpr⟩ → ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩ ⟨conjunct’⟩ ⟨opexpr’⟩ ⟨listexpr’⟩
def parserRDListExpr(
                      lexer: PeekIterator[Token],
                      first: ParserRD.gen.FirstMap,
                      variableMap: VariableMap,
                      scopes: Scopes,
                      scope: Int,
                      ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      val f = parserRDFactor(lexer, first, variableMap, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val mpt = parserRDMulP(lexer, first, variableMap, f, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val apt = parserRDAddP(lexer, first, variableMap, mpt, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val cpt = parserRDComparP(lexer, first, variableMap, apt, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val cjpt = parserRDConjunctP(lexer, first, variableMap, cpt, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val opt = parserRDOpExprP(lexer, first, variableMap, cjpt, scopes, scope) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      parserRDListExprP(lexer, first, variableMap, opt, scopes, scope)
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨listexpr’⟩ → : ⟨listexpr⟩
//    | ε
def parserRDListExprP(
                       lexer: PeekIterator[Token],
                       first: ParserRD.gen.FirstMap,
                       variableMap: VariableMap,
                       lhs: ParseTree,
                       scopes: Scopes,
                       scope: Int,
                       ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KColon.ordinal) {
        lexer.next()
        val rhs = parserRDListExpr(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val pt = Application(
          Application(
            Ident("cons"),
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
def parserRDOpExprP(
                     lexer: PeekIterator[Token],
                     first: ParserRD.gen.FirstMap,
                     variableMap: VariableMap,
                     lhs: ParseTree,
                     scopes: Scopes,
                     scope: Int,
                     ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == SOr.ordinal) {
        lexer.next()
        val f = parserRDFactor(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val mpt = parserRDMulP(lexer, first, variableMap, f, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val apt = parserRDAddP(lexer, first, variableMap, mpt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cpt = parserRDComparP(lexer, first, variableMap, apt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cjpt = parserRDConjunctP(lexer, first, variableMap, cpt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val pt = Application(
          Application(
            Ident("or"),
            lhs
          ),
          cjpt
        )
        parserRDOpExprP(lexer, first, variableMap, pt, scopes, scope)
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
def parserRDConjunctP(
                       lexer: PeekIterator[Token],
                       first: ParserRD.gen.FirstMap,
                       variableMap: VariableMap,
                       lhs: ParseTree,
                       scopes: Scopes,
                       scope: Int,
                       ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == SAnd.ordinal) {
        lexer.next()
        val f = parserRDFactor(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val mpt = parserRDMulP(lexer, first, variableMap, f, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val apt = parserRDAddP(lexer, first, variableMap, mpt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cpt = parserRDComparP(lexer, first, variableMap, apt, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val pt = Application(
          Application(
            Ident("and"),
            lhs
          ),
          cpt
        )
        parserRDConjunctP(lexer, first, variableMap, pt, scopes, scope)
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
def parserRDComparP(
                     lexer: PeekIterator[Token],
                     first: ParserRD.gen.FirstMap,
                     variableMap: VariableMap,
                     lhs: ParseTree,
                     scopes: Scopes,
                     scope: Int,
                     ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case t @ (SEqual | SNotEqual | SLess | SGreater | SLessEqual | SGreaterEqual) =>
          lexer.next()
          val f = parserRDFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserRDMulP(lexer, first, variableMap, f, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val apt = parserRDAddP(lexer, first, variableMap, mpt, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              t match {
                case SEqual =>
                  Ident("eq")
                case SNotEqual =>
                  Ident("neq")
                case SLess =>
                  Ident("lt")
                case SGreater =>
                  Ident("gt")
                case SLessEqual =>
                  Ident("leq")
                case SGreaterEqual =>
                  Ident("geq")
              },
              lhs
            ),
            apt
          )
          parserRDComparP(lexer, first, variableMap, pt, scopes, scope)
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
def parserRDAddP(
                  lexer: PeekIterator[Token],
                  first: ParserRD.gen.FirstMap,
                  variableMap: VariableMap,
                  lhs: ParseTree,
                  scopes: Scopes,
                  scope: Int,
                  ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SPlus =>
          lexer.next()
          val f = parserRDFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserRDMulP(lexer, first, variableMap, f, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              Ident("plus"),
              lhs,
            ),
            mpt
          )
          parserRDAddP(lexer, first, variableMap, pt, scopes, scope)
        case SMinus =>
          lexer.next()
          val f = parserRDFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserRDMulP(lexer, first, variableMap, f, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              Ident("minus"),
              lhs,
            ),
            mpt
          )
          parserRDAddP(lexer, first, variableMap, pt, scopes, scope)
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
def parserRDMulP(
                  lexer: PeekIterator[Token],
                  first: ParserRD.gen.FirstMap,
                  variableMap: VariableMap,
                  lhs: ParseTree,
                  scopes: Scopes,
                  scope: Int,
                  ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SMul =>
          lexer.next()
          val f = parserRDFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              Ident("mul"),
              lhs
            ),
            f
          )
          parserRDMulP(lexer, first, variableMap, pt, scopes, scope)
        case SDiv =>
          lexer.next()
          val f = parserRDFactor(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val pt = Application(
            Application(
              Ident("div"),
              lhs
            ),
            f
          )
          parserRDMulP(lexer, first, variableMap, pt, scopes, scope)
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
def parserRDFactor(
                    lexer: PeekIterator[Token],
                    first: ParserRD.gen.FirstMap,
                    variableMap: VariableMap,
                    scopes: Scopes,
                    scope: Int,
                    ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SPlus =>
          lexer.next()
          val s = parserRDSimple(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserRDCombP(lexer, first, variableMap, scopes, scope, s) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          // prefix + does nothing
          return Right(cp)
        case SMinus =>
          lexer.next()
          val s = parserRDSimple(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserRDCombP(lexer, first, variableMap, scopes, scope, s) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          return Right(
            Application(
              Ident("inv"),
              cp,
            )
          )
        case SNot =>
          lexer.next()
          val s = parserRDSimple(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserRDCombP(lexer, first, variableMap, scopes, scope, s) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          return Right(
            Application(
              Ident("not"),
              cp,
            )
          )
        case _ =>
      }
      if(first(Simple.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val s = parserRDSimple(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cp = parserRDCombP(lexer, first, variableMap, scopes, scope, s) match {
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
def parserRDCombP(
                   lexer: PeekIterator[Token],
                   first: ParserRD.gen.FirstMap,
                   variableMap: VariableMap,
                   scopes: Scopes,
                   scope: Int,
                   ptSoFar: ParseTree,
                   ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (first(Simple.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val s = parserRDSimple(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserRDCombP(
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
def parserRDSimple(
                    lexer: PeekIterator[Token],
                    first: ParserRD.gen.FirstMap,
                    variableMap: VariableMap,
                    scopes: Scopes,
                    scope: Int,
                    ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case Id(s) =>
          lexer.next()
          Right(Ident(scope.toString + s))
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
          parserRDListP(lexer, first, variableMap, scopes, scope)
        case KOpenParen =>
          lexer.next()
          val ptc = parserRDCondExpr(lexer, first, variableMap, scopes, scope) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          parserRDExprP(lexer, first, variableMap, scopes, scope) match {
            case Right(_) =>
            case Left(e) => return Left(e)
          }
          lexer.peek() match {
            case Some(KCloseParen) => lexer.next()
            case Some(_) => return Left(ParseError.ToDo)
            case None => return Left(ParseError.ToDo)
          }
          Right(ptc)
        case _ =>
          Left(ParseError.ToDo)
      }
    case None =>
      Left(ParseError.ToDo)
  }
}
//  ⟨list’⟩ → ]
//    | ⟨condexpr⟩ ⟨expr’⟩ ⟨listelems’⟩ ]
def parserRDListP(
                   lexer: PeekIterator[Token],
                   first: ParserRD.gen.FirstMap,
                   variableMap: VariableMap,
                   scopes: Scopes,
                   scope: Int,
                   ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KCloseBracket.ordinal) {
        lexer.next()
        Right(Const(Nil))
      } else if (first(CondExpr.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val ptc = parserRDCondExpr(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserRDExprP(lexer, first, variableMap, scopes, scope) match {
          case Right(_) =>
          case Left(e) => return Left(e)
        }
        /*val pt = parserRDListElemsP(lexer, first, variableMap, ptc) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val pt2 = Application(
          Application(
            Ident("cons"),
            ptc
          ),
          parserRDListElemsP(lexer, first, variableMap)
        )*/
        val pt = parserRDListElemsP(lexer, first, variableMap, scopes, scope) match {
          case Left(e) => return Left(e)
          case Right(pt) => Application(
            Application(
              Ident("cons"),
              ptc
            ),
            pt
          )
        }
        lexer.peek() match {
          case Some(KCloseBracket) =>
            lexer.next()
            Right(pt)
          case Some(_) => Left(ParseError.ToDo)
          case None => Left(ParseError.ToDo)
        }
        /*val elems = parserRDListElemsP(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        lexer.peek() match {
          case Some(t) =>
            if (t.ordinal == KCloseBracket.ordinal) {
              lexer.next()
            } else {
              return Left(ParseError.ToDo)
            }
          case None => return Left(ParseError.ToDo)
        }
        Right(Application(
          Application(
            Ident("cons"),
            // ToDo
            Const(Nil)
          ),
          elems
        ))*/
      } else {
        Left(ParseError.ToDo)
      }
    case None =>
      Left(ParseError.ToDo)
  }
}
//  ⟨listelems’⟩ → , ⟨condexpr⟩ ⟨expr’⟩ ⟨listelems’⟩
//    | ε
def parserRDListElemsP(
                        lexer: PeekIterator[Token],
                        first: ParserRD.gen.FirstMap,
                        variableMap: VariableMap,
                        scopes: Scopes,
                        scope: Int
                        //lhs: ParseTree,
                        ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KComma.ordinal) {
        lexer.next()
        val ptc = parserRDCondExpr(lexer, first, variableMap, scopes, scope) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserRDExprP(lexer, first, variableMap, scopes, scope) match {
          case Right(_) =>
          case Left(e) => return Left(e)
        }
        parserRDListElemsP(lexer, first, variableMap, scopes, scope) match {
          case Left(e) => Left(e)
          case Right(pt) => Right(Application(
            Application(
              Ident("cons"),
              ptc
            ),
            pt
          ))
        }
        /*val pt = Application(
          Application(
            Ident("cons"),
            rhs
          ),
          lhs
        )
        parserRDListElemsP(lexer, first, variableMap, pt)*/
        /*val elems = parserRDListElemsP(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        Right(Application(
          Application(
            Ident("cons"),
            ptc
          ),
          elems
        ))*/
      } else {
        /*Right(Application(
          Application(
            Ident("cons"),
            lhs
          ),
          Const(Constant.Nil)
        ))*/
        Right(Const(Constant.Nil))
      }
    case None =>
      Left(ParseError.ToDo)
  }
}