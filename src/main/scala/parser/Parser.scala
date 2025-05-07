package parser

import lexer.PeekIterator

import scala.collection.mutable

class Parser[Token <: scala.reflect.Enum, NonTerminal <: scala.reflect.Enum]:
  val gen: ParserGenerator[Token, NonTerminal] = ParserGenerator()

  def check(lexer: PeekIterator[Token], start: gen.GrammarSymbol, tbl: gen.ParseTable, em: gen.EmptyMap): Boolean =
    val s: mutable.Stack[gen.GrammarSymbol] = mutable.Stack()
    s.push(start)
    while (s.nonEmpty) {
      val gs = s.pop()
      gs match {
        case GrammarSymbolGeneric.T(t) =>
          lexer.peek() match {
            case Some(v) =>
              if (v.ordinal == t.ordinal) {
                lexer.next()
              } else {
                return false
              }
            case None =>
              return false
          }
        case GrammarSymbolGeneric.NT(nt) =>
          lexer.peek() match {
            case Some(t) =>
              tbl(nt.ordinal)(t.ordinal) match {
                case Some(d) =>
                  s.pushAll(d.reverseIterator)
                case None =>
                  return false
              }
            case None =>
              if (!em(nt.ordinal)) {
                return false
              }
          }
      }
    }

    if (!lexer.hasNext) {
      true
    } else {
      false
    }
