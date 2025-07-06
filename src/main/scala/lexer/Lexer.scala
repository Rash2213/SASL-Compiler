package lexer

import java.nio.charset.StandardCharsets

enum Token:
  case KDef
  case KIf
  case KThen
  case KElse
  case KWhere
  case KColon
  case KOpenParen
  case KCloseParen
  case KOpenBracket
  case KCloseBracket
  case KSemicolon
  case KDot
  case KComma
  case Id(str: String)
  case CNum(num: Long)
  case CBool(bool: Boolean)
  case CString(str: String)
  case CNil
  case SPlus
  case SMinus
  case SNot
  case SMul
  case SDiv
  case SEqual
  case SNotEqual
  case SLess
  case SGreater
  case SLessEqual
  case SGreaterEqual
  case SAnd
  case SOr

def charNum(char:Byte): Boolean =
  '0' <= char && char <= '9'

def charIdStart(char: Byte): Boolean =
  'a' <= char && char <= 'z' ||
    'A' <= char && char <= 'Z' ||
    char == '_'

def charIdRest(char: Byte): Boolean =
  charIdStart(char) || charNum(char)

def charWhitespace(char: Byte): Boolean =
  char == ' '
    || char == '\t'
    || char == '\n'
    || char == '\r'
    || char == '\f'

def replaceEscapeSequences(input: String): String = {
  val sb = new StringBuilder()
  var i = 0
  while (i < input.length) {
    if (input.charAt(i) == '\\') {
      if (i + 1 < input.length) {
        input.charAt(i + 1) match {
          case 'a' => sb.append('\u0008'); i += 2
          case 'b' => sb.append('\b'); i += 2
          case 'e' => sb.append('\u001B'); i += 2
          case 'f' => sb.append('\f'); i += 2
          case 'n' => sb.append('\n'); i += 2
          case 'r' => sb.append('\r'); i += 2
          case 't' => sb.append('\t'); i += 2
          case 'v' => sb.append('\u000B'); i += 2
          case '\\' => sb.append('\\'); i += 2
          case '\'' => sb.append('\''); i += 2
          case '"' => sb.append('"'); i += 2
          case '?' => sb.append('?'); i += 2
          case 'u' =>
            if (i + 5 < input.length) {
              try {
                sb.append(String(Character.toChars(Integer.parseUnsignedInt(input.substring(i+2, i+6), 16))))
                i += 6
              } catch {
                case _: NumberFormatException =>
                  sb.append('\\').append('u')
                  i += 2
                case _: IllegalArgumentException =>
                  sb.append('\\').append('u')
                  i += 2
              }
            } else {
              sb.append('\\').append('u')
              i += 2
            }
          case 'U' =>
            if (i + 9 < input.length) {
              try {
                sb.append(String(Character.toChars(Integer.parseUnsignedInt(input.substring(i+2, i+10), 16))))
                i += 10
              } catch {
                case _: NumberFormatException =>
                  sb.append('\\').append('U')
                  i += 2
                case _: IllegalArgumentException =>
                  sb.append('\\').append('U')
                  i += 2
              }
            } else {
              sb.append('\\').append('U')
              i += 2
            }
          case other =>
            // backslash and a char not equalling a escape sequence
            // append both characters
            sb.append('\\').append(other)
            i += 2
        }
      } else {
        // Backslash at the end of the string, append it literally, should not happen from lexer
        sb.append('\\')
        i += 1
      }
    } else {
      sb.append(input.charAt(i))
      i += 1
    }
  }
  sb.toString()
}

trait PeekIterator[A] extends Iterator[A] {
  /** Method to peek at the next element without consuming it */
  def peek(): Option[A]
}

// ToDo: Input should be immutable
// ToDo: Proper Error-Handling
// ToDo: UTF8-Support in the marked places
class Lexer(private val raw: Array[Byte]) extends PeekIterator[Token]:
  private var idx = 0
  private var preview = previewIterate()

  private def consumeWhitespace(): Unit =
    while (true) {
      while (idx < raw.length && charWhitespace(raw(idx))) {
        idx += 1;
      }
      if (idx < raw.length - 1 && raw(idx) == '|' && raw(idx + 1) == '|') {
        idx += 2;
        // ToDo: will lead to issues for utf-8 multi byte chars which might accidentally contain the same byte
        while (idx < raw.length && !(raw(idx) == '\n')) {
          idx += 1;
        }
      } else {
        return
      }
    }

  private def previewIterate(): Option[Token] =
    consumeWhitespace()

    if idx >= raw.length then
      return None

    val start = idx

    raw(idx) match {
      case '(' => idx += 1; return Some(Token.KOpenParen)
      case ')' => idx += 1; return Some(Token.KCloseParen)
      case '[' => idx += 1; return Some(Token.KOpenBracket)
      case ']' => idx += 1; return Some(Token.KCloseBracket)
      case ':' => idx += 1; return Some(Token.KColon)
      case ';' => idx += 1; return Some(Token.KSemicolon)
      case '.' => idx += 1; return Some(Token.KDot)
      case ',' => idx += 1; return Some(Token.KComma)
      case '+' => idx += 1; return Some(Token.SPlus)
      case '-' => idx += 1; return Some(Token.SMinus)
      case '*' => idx += 1; return Some(Token.SMul)
      case '/' => idx += 1; return Some(Token.SDiv)
      case '=' => idx += 1; return Some(Token.SEqual)
      case '~' =>
        if (idx + 1 < raw.length && raw(idx + 1) == '=') {
          idx += 2; return Some(Token.SNotEqual)
        } else {
          return None
        }
      case '<' =>
        if (idx + 1 < raw.length && raw(idx + 1) == '=') {
          idx += 2; return Some(Token.SLessEqual)
        } else {
          idx += 1; return Some(Token.SLess)
        }
      case '>' =>
        if (idx + 1 < raw.length && raw(idx + 1) == '=') {
          idx += 2; return Some(Token.SGreaterEqual)
        } else {
          idx += 1; return Some(Token.SGreater)
        }
      case '"' =>
        // ToDo: will lead to issues for utf-8 multi byte chars which might accidentally contain the same byte
        // ToDo: also support escape sequences
        idx += 1
        if idx >= raw.length then {
          return None
        }
        var escaped = false
        while (raw(idx) != '"' || escaped) {
          if (raw(idx) == '\\') {
            escaped = !escaped
          } else {
            escaped = false
          }
          idx += 1
          if idx >= raw.length then {
            return None
          }
        }
        idx += 1
        return Some(Token.CString(replaceEscapeSequences(String(raw.slice(start+1, idx-1), StandardCharsets.UTF_8))))
      case _ => // Necessary, because Scala's otherwise assumes we always return
    }

    if charNum(raw(idx)) then {
      idx += 1
      while (idx < raw.length && charNum(raw(idx))) {
        idx += 1
      }
      Some(Token.CNum(String(raw.slice(start, idx), StandardCharsets.UTF_8).toInt))
    } else if charIdStart(raw(idx)) then {
      // prefer the longest match
      idx += 1
      while(idx < raw.length && charIdRest(raw(idx))) {
        idx += 1
      }
      val len = idx - start
      if len > 5 then {
        return Some(Token.Id(String(raw.slice(start, idx), StandardCharsets.UTF_8)))
      }
      val slice = raw.slice(start, idx)
      if len > 5 || len == 1 then {
        Some(Token.Id(String(slice, StandardCharsets.UTF_8)))
      } else if len == 5 then {
        if slice.sameElements("where") then {
          Some(Token.KWhere)
        } else if slice.sameElements("false") then {
          Some(Token.CBool(false))
        } else {
          Some(Token.Id(String(slice, StandardCharsets.UTF_8)))
        }
      } else if len == 4 then {
        if slice.sameElements("then") then {
          Some(Token.KThen)
        } else if slice.sameElements("else") then {
          Some(Token.KElse)
        } else if slice.sameElements("true") then {
          Some(Token.CBool(true))
        } else {
          Some(Token.Id(String(slice, StandardCharsets.UTF_8)))
        }
      } else if len == 3 then {
        if slice.sameElements("def") then {
          Some(Token.KDef)
        } else if slice.sameElements("nil") then {
          Some(Token.CNil)
        } else if slice.sameElements("not") then {
          Some(Token.SNot)
        } else if slice.sameElements("and") then {
          Some(Token.SAnd)
        } else {
          Some(Token.Id(String(slice, StandardCharsets.UTF_8)))
        }
      } else {
        if slice.sameElements("if") then {
          Some(Token.KIf)
        } else if slice.sameElements("or") then {
          Some(Token.SOr)
        } else {
          Some(Token.Id(String(slice, StandardCharsets.UTF_8)))
        }
      }
    } else {
      None
    }

  def hasNext: Boolean = preview.isDefined

  def peek(): Option[Token] =
    preview

  def next(): Token =
    preview match
      case Some(value) =>
        preview = previewIterate()
        value
      case None => throw new NoSuchElementException

class TestLexer[Token](vals: Array[Token]) extends PeekIterator[Token]:
  private var current = 0;

  def hasNext: Boolean = current < vals.length
  def peek(): Option[Token] =
    if (current < vals.length) {
      Some(vals(current))
    } else {
      None
    }

  def next(): Token =
    if (current < vals.length) {
      current += 1
      vals(current - 1)
    } else {
      throw new NoSuchElementException
    }
