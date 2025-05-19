package parser

import lexer.{PeekIterator, Token}
import lexer.Token.*
import SaslData.NonTerminal
import SaslData.NonTerminal.*

import scala.annotation.tailrec
import scala.collection.mutable

enum Constant:
  case Num(n: Number)
  case Bool(b: Boolean)
  case Str(s: String)
  case Nil

enum ParseTree:
  case Const(c: Constant)
  case Ident(name: String)
  case Application(operator: ParseTree, operand: ParseTree)

enum ParseError:
  case ToDo

object ParserShit:
  val gen: ParserGenerator[Token, NonTerminal] = ParserGenerator()

import ParseTree.*
import Constant.*

// variableMap is a map from a name to a ParseTree, we prepend the scope to the variable name
case class ParserResult(variableMap: Map[String, ParseTree], pTree: ParseTree)

//  ⟨system⟩ → def id ⟨abstraction⟩ ⟨expr’⟩ ⟨funcdefs’⟩ . ⟨condexpr⟩ ⟨expr’⟩
//    | ⟨condexpr⟩ ⟨expr’⟩
def parserShitSystem(
                      lexer: PeekIterator[Token],
                      first: ParserShit.gen.FirstMap,
                      variableMap: mutable.Map[String, ParseTree]
                    ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KDef.ordinal) {
        lexer.next()
        lexer.peek() match {
          case Some(Id(s)) =>
            lexer.next()
            val pta = parserShitAbstraction(lexer, first, variableMap) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            variableMap(s) = pta._1
            parserShitExprP(lexer, first, variableMap) match {
              case Right(_) =>
              case Left(e) => return Left(e)
            }
            parserShitFuncDefsP(lexer, first, variableMap) match {
              case Right(_) =>
              case Left(e) => return Left(e)
            }
            lexer.peek() match {
              case Some(KDot) => lexer.next()
              case Some(_) => return Left(ParseError.ToDo)
              case None => return Left(ParseError.ToDo)
            }
            val ptc = parserShitCondExpr(lexer, first, variableMap) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            parserShitExprP(lexer, first, variableMap) match {
              case Right(_) =>
              case Left(e) => return Left(e)
            }
            Right(ptc)
          case Some(_) => Left(ParseError.ToDo)
          case None => Left(ParseError.ToDo)
        }
      } else if (first(CondExpr.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val ptc = parserShitCondExpr(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserShitExprP(lexer, first, variableMap) match {
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
def parserShitFuncDefsP(
                         lexer: PeekIterator[Token],
                         first: ParserShit.gen.FirstMap,
                         variableMap: mutable.Map[String, ParseTree],
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
          val pta = parserShitAbstraction(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          variableMap(s) = pta._1
          parserShitExprP(lexer, first, variableMap) match {
            case Right(_) =>
            case Left(e) => return Left(e)
          }
          parserShitFuncDefsP(lexer, first, variableMap)
        case _ => Left(ParseError.ToDo)
      }
    case None => Right(())
  }
}
//  ⟨expr’⟩ → where id ⟨abstraction⟩ ⟨defs’⟩
//    | ε
def parserShitExprP(
                     lexer: PeekIterator[Token],
                     first: ParserShit.gen.FirstMap,
                     variableMap: mutable.Map[String, ParseTree],
                   ): Either[ParseError, Unit] = {
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
          val pta = parserShitAbstraction(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          // ToDo: add scope to name
          variableMap(s) = pta._1
          parserShitDefsP(lexer, first, variableMap)
        case _ => Left(ParseError.ToDo)
      }
    case None => Right(())
  }
}
//  ⟨abstraction⟩ → = ⟨condexpr⟩
//    | id ⟨abstraction⟩
@tailrec
def parserShitAbstraction(
                           lexer: PeekIterator[Token],
                           first: ParserShit.gen.FirstMap,
                           variableMap: mutable.Map[String, ParseTree],
                           al: Array[String] = Array(),
                         ): Either[ParseError, (ParseTree, Array[String])] = {
  var argList: Array[String] = al
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SEqual =>
          lexer.next()
          parserShitCondExpr(lexer, first, variableMap) match {
            case Right(pt) => Right(pt, argList)
            case Left(e) => Left(e)
          }
        case Id(s) =>
          // ToDo: add scope prefix
          argList = argList.appended(s)
          lexer.next()
          parserShitAbstraction(lexer, first, variableMap, argList)
        case _ => Left(ParseError.ToDo)
      }
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨defs’⟩ → ; id ⟨abstraction⟩ ⟨defs’⟩
//    | ε
def parserShitDefsP(
                     lexer: PeekIterator[Token],
                     first: ParserShit.gen.FirstMap,
                     variableMap: mutable.Map[String, ParseTree]
                   ): Either[ParseError, Unit] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KSemicolon.ordinal) {
        lexer.next()
        lexer.peek() match {
          case Some(Id(s)) =>
            val pta = parserShitAbstraction(lexer, first, variableMap) match {
              case Right(pt) => pt
              case Left(e) => return Left(e)
            }
            // ToDo: add scope to name
            variableMap(s) = pta._1
            Right(())
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
def parserShitCondExpr(
                        lexer: PeekIterator[Token],
                        first: ParserShit.gen.FirstMap,
                        variableMap: mutable.Map[String, ParseTree],
                      ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KIf.ordinal) {
        lexer.next()
        val ptc = parserShitCondExpr(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserShitExprP(lexer, first, variableMap) match {
          case Right(_) =>
          case Left(e) => return Left(e)
        }
        lexer.peek() match {
          case Some(KThen) => lexer.next()
          case Some(_) => return Left(ParseError.ToDo)
          case None => return Left(ParseError.ToDo)
        }
        val ptt = parserShitCondExpr(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        lexer.peek() match {
          case Some(KElse) => lexer.next()
          case Some(_) => return Left(ParseError.ToDo)
          case None => return Left(ParseError.ToDo)
        }
        val pte = parserShitCondExpr(lexer, first, variableMap) match {
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
        parserShitListExpr(lexer, first, variableMap)
      } else {
        Left(ParseError.ToDo)
      }
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨listexpr⟩ → ⟨factor⟩ ⟨mul’⟩ ⟨add’⟩ ⟨compar’⟩ ⟨conjunct’⟩ ⟨opexpr’⟩ ⟨listexpr’⟩
def parserShitListExpr(
                        lexer: PeekIterator[Token],
                        first: ParserShit.gen.FirstMap,
                        variableMap: mutable.Map[String, ParseTree],
                      ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      val f = parserShitFactor(lexer, first, variableMap) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val mpt = parserShitMulP(lexer, first, variableMap, f) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val apt = parserShitAddP(lexer, first, variableMap, mpt) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val cpt = parserShitComparP(lexer, first, variableMap, apt) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val cjpt = parserShitConjunctP(lexer, first, variableMap, cpt) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      val opt = parserShitOpExprP(lexer, first, variableMap, cjpt) match {
        case Right(pt) => pt
        case Left(e) => return Left(e)
      }
      parserShitListExprP(lexer, first, variableMap, opt)
    case None => Left(ParseError.ToDo)
  }
}
//  ⟨listexpr’⟩ → : ⟨listexpr⟩
//    | ε
def parserShitListExprP(
                         lexer: PeekIterator[Token],
                         first: ParserShit.gen.FirstMap,
                         variableMap: mutable.Map[String, ParseTree],
                         lhs: ParseTree,
                       ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KColon.ordinal) {
        lexer.next()
        val rhs = parserShitListExpr(lexer, first, variableMap) match {
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
def parserShitOpExprP(
                       lexer: PeekIterator[Token],
                       first: ParserShit.gen.FirstMap,
                       variableMap: mutable.Map[String, ParseTree],
                       lhs: ParseTree,
                     ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == SOr.ordinal) {
        lexer.next()
        val f = parserShitFactor(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val mpt = parserShitMulP(lexer, first, variableMap, f) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val apt = parserShitAddP(lexer, first, variableMap, mpt) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cpt = parserShitComparP(lexer, first, variableMap, apt) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cjpt = parserShitConjunctP(lexer, first, variableMap, cpt) match {
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
        parserShitOpExprP(lexer, first, variableMap, pt)
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
def parserShitConjunctP(
                         lexer: PeekIterator[Token],
                         first: ParserShit.gen.FirstMap,
                         variableMap: mutable.Map[String, ParseTree],
                         lhs: ParseTree,
                       ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == SAnd.ordinal) {
        lexer.next()
        val f = parserShitFactor(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val mpt = parserShitMulP(lexer, first, variableMap, f) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val apt = parserShitAddP(lexer, first, variableMap, mpt) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cpt = parserShitComparP(lexer, first, variableMap, apt) match {
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
        parserShitConjunctP(lexer, first, variableMap, pt)
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
def parserShitComparP(
                       lexer: PeekIterator[Token],
                       first: ParserShit.gen.FirstMap,
                       variableMap: mutable.Map[String, ParseTree],
                       lhs: ParseTree,
                     ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case t @ (SEqual | SNotEqual | SLess | SGreater | SLessEqual | SGreaterEqual) =>
          lexer.next()
          val f = parserShitFactor(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserShitMulP(lexer, first, variableMap, f) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val apt = parserShitAddP(lexer, first, variableMap, mpt) match {
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
          parserShitComparP(lexer, first, variableMap, pt)
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
def parserShitAddP(
                    lexer: PeekIterator[Token],
                    first: ParserShit.gen.FirstMap,
                    variableMap: mutable.Map[String, ParseTree],
                    lhs: ParseTree,
                  ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SPlus =>
          lexer.next()
          val f = parserShitFactor(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserShitMulP(lexer, first, variableMap, f) match {
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
          parserShitAddP(lexer, first, variableMap, pt)
        case SMinus =>
          lexer.next()
          val f = parserShitFactor(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val mpt = parserShitMulP(lexer, first, variableMap, f) match {
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
          parserShitAddP(lexer, first, variableMap, pt)
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
def parserShitMulP(
                    lexer: PeekIterator[Token],
                    first: ParserShit.gen.FirstMap,
                    variableMap: mutable.Map[String, ParseTree],
                    lhs: ParseTree,
                  ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SMul =>
          lexer.next()
          val f = parserShitFactor(lexer, first, variableMap) match {
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
          parserShitMulP(lexer, first, variableMap, pt)
        case SDiv =>
          lexer.next()
          val f = parserShitFactor(lexer, first, variableMap) match {
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
          parserShitMulP(lexer, first, variableMap, pt)
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
def parserShitFactor(
                      lexer: PeekIterator[Token],
                      first: ParserShit.gen.FirstMap,
                      variableMap: mutable.Map[String, ParseTree]
                    ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case SPlus =>
          lexer.next()
          val s = parserShitSimple(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserShitCombP(lexer, first, variableMap, s) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          // prefix + does nothing
          return Right(cp)
        case SMinus =>
          lexer.next()
          val s = parserShitSimple(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserShitCombP(lexer, first, variableMap, s) match {
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
          val s = parserShitSimple(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          val cp = parserShitCombP(lexer, first, variableMap, s) match {
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
        val s = parserShitSimple(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val cp = parserShitCombP(lexer, first, variableMap, s) match {
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
def parserShitCombP(
                     lexer: PeekIterator[Token],
                     first: ParserShit.gen.FirstMap,
                     variableMap: mutable.Map[String, ParseTree],
                     ptSoFar: ParseTree,
                   ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (first(Simple.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val s = parserShitSimple(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserShitCombP(
          lexer,
          first,
          variableMap,
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
def parserShitSimple(
                      lexer: PeekIterator[Token],
                      first: ParserShit.gen.FirstMap,
                      variableMap: mutable.Map[String, ParseTree]
                    ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      t match {
        case Id(s) =>
          // ToDo: Scoping
          lexer.next()
          Right(Ident(s))
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
          parserShitListP(lexer, first, variableMap)
        case KOpenParen =>
          lexer.next()
          val ptc = parserShitCondExpr(lexer, first, variableMap) match {
            case Right(pt) => pt
            case Left(e) => return Left(e)
          }
          parserShitExprP(lexer, first, variableMap) match {
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
def parserShitListP(
                     lexer: PeekIterator[Token],
                     first: ParserShit.gen.FirstMap,
                     variableMap: mutable.Map[String, ParseTree]
                   ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KCloseBracket.ordinal) {
        lexer.next()
        Right(Const(Nil))
      } else if (first(CondExpr.ordinal).map(v => v.ordinal).contains(t.ordinal)) {
        val ptc = parserShitCondExpr(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserShitExprP(lexer, first, variableMap) match {
          case Right(_) =>
          case Left(e) => return Left(e)
        }
        /*val pt = parserShitListElemsP(lexer, first, variableMap, ptc) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        val pt2 = Application(
          Application(
            Ident("cons"),
            ptc
          ),
          parserShitListElemsP(lexer, first, variableMap)
        )*/
        val pt = parserShitListElemsP(lexer, first, variableMap) match {
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
        /*val elems = parserShitListElemsP(lexer, first, variableMap) match {
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
def parserShitListElemsP(
                          lexer: PeekIterator[Token],
                          first: ParserShit.gen.FirstMap,
                          variableMap: mutable.Map[String, ParseTree],
                          //lhs: ParseTree,
                        ): Either[ParseError, ParseTree] = {
  lexer.peek() match {
    case Some(t) =>
      if (t.ordinal == KComma.ordinal) {
        lexer.next()
        val ptc = parserShitCondExpr(lexer, first, variableMap) match {
          case Right(pt) => pt
          case Left(e) => return Left(e)
        }
        parserShitExprP(lexer, first, variableMap) match {
          case Right(_) =>
          case Left(e) => return Left(e)
        }
        parserShitListElemsP(lexer, first, variableMap) match {
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
        parserShitListElemsP(lexer, first, variableMap, pt)*/
        /*val elems = parserShitListElemsP(lexer, first, variableMap) match {
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