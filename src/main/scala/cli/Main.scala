package cli

import lexer.{Lexer, Token}
import parser.{ParserGenerator, VariableMap, Scopes, parserRDSystem, Visualizer}
import parser.SaslData.{NonTerminal, derMap, emMap}

import java.io.{FileInputStream, File}

import cats.effect.*
import cats.syntax.all.*

import com.monovore.decline.*
import com.monovore.decline.effect.*

import scala.collection.mutable

case class VisualizeParseTree(file: String)
case class Execute(file: String)

val fileOpts: Opts[String] = Opts.argument[String](metavar = "file")

val optsVisualizeParseTree: Opts[VisualizeParseTree] = Opts.subcommand("vis-pt", "Visualize the parse tree of the program.") {
  fileOpts.map(VisualizeParseTree.apply)
}
val optsExecute: Opts[Execute] = Opts.subcommand("ex", "Execute the given program.") {
  fileOpts.map(Execute.apply)
}

def readFileBin(filePath: String): Array[Byte] = {
  val fileInputStream = new FileInputStream(filePath)
  val byteLength = fileInputStream.available()
  val bytesArray = new Array[Byte](byteLength)
  fileInputStream.read(bytesArray)
  fileInputStream.close()
  bytesArray
}

def getFilename(filePath: String): String = {
  File(filePath).getName.stripSuffix(".sasl")
}

object SaslCompilerApp extends CommandIOApp(
  name = "saslc",
  header = "SASL Compiler written in Scala.",
  version = "0.0.x"
) {

  override def main: Opts[IO[ExitCode]] =
    (optsVisualizeParseTree.orElse(optsExecute)).map {
      case VisualizeParseTree(file) =>
        IO {
          val raw = readFileBin(file)
          val lexer = Lexer(raw)
          val gen: ParserGenerator[Token, NonTerminal] = ParserGenerator()
          val fr = gen.first(NonTerminal.values.length, derMap, emMap)
          val firstSet = fr._1
          val varMap: VariableMap = mutable.Map()
          val scopes: Scopes = mutable.ArrayBuffer()
          parserRDSystem(lexer, firstSet, varMap, scopes) match {
            case Right(pt) =>
              val visualizer = Visualizer()
              val d = visualizer.generateDot(pt, varMap)
              val fn = getFilename(file)
              visualizer.saveDotToFile(d, s"visualizations/$fn.dot")
              println(s"Visualization saved to visualizations/$fn.dot")
              ExitCode.Success
            case Left(e) =>
              println("Encountered error during parsing!")
              ExitCode.Error
          }
        }
      case Execute(file) =>
        IO {
          val raw = readFileBin(file)
          val lexer = Lexer(raw)
          lexer.foreach(t => println(t))
          ExitCode.Success
        }
    }
}
