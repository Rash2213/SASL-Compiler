package cli

import cats.data.EitherT
import lexer.Lexer
import parser.{ParseError, ParseTreeUnoptimized, ScopesRes, VariableMapUnoptimizedRes, parserRDUnoptimized}
import visualizer.{VisualizerParseTreeUnoptimized, VisualizerReductionTreeUnoptimized}
import compiler.{CompilerError, CompilerUnoptimized}
import vm.{ReductionTreeUnoptimized, evalAndPrintUnoptimized}
import serializer.{DeserializationError, SerializerUnoptimized}

import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import cats.effect.*
import cats.syntax.all.*
import com.monovore.decline.*
import com.monovore.decline.effect.*

case class VisualizeParseTree(file: String, libs: List[String])
case class VisualizeCompileTree(file: String, libs: List[String])
case class Execute(file: String, libs: List[String], optimizer: Boolean)
case class Compile(file: String, libs: List[String])
case class ExecuteB(file: String, optimizer: Boolean)

val fileOpts: Opts[String] = Opts.argument[String](metavar = "file")
val optimizerOpts: Opts[Boolean] = Opts.flag("no-optimizer", "Disables the optimizer.").orFalse
val lFunOpts: Opts[Boolean] = Opts.flag("no-fast-local-funs", "Disables the fast local functions optimization.").orFalse
val libOpts: Opts[List[String]] = Opts.options[String]("lib", "Includes the given sasl/sasllib file as a static library.").orEmpty

val optsVisualizeParseTree: Opts[VisualizeParseTree] = Opts.subcommand("vis-pt", "Visualize the parse tree of the program.") {
  (fileOpts, libOpts).mapN((f, l) => VisualizeParseTree(f, l))
}
val optsVisualizeCompileTree: Opts[VisualizeCompileTree] = Opts.subcommand("vis-ct", "Visualize the compile tree of the program.") {
  (fileOpts, libOpts).mapN((f, l) => VisualizeCompileTree(f, l))
}
val optsExecute: Opts[Execute] = Opts.subcommand("ex", "Execute the given program.") {
  (fileOpts, libOpts, optimizerOpts).mapN((f, l, b) => Execute(f, l, !b))
}
val optsCompile: Opts[Compile] = Opts.subcommand("comp", "Compile the given program to saslbin.") {
  (fileOpts, libOpts).mapN(Compile.apply)
}
val optsExecuteB: Opts[ExecuteB] = Opts.subcommand("exb", "Execute the given saslbin program binary.") {
  (fileOpts, optimizerOpts).mapN((f, b) => ExecuteB(f, !b))
}

def printWriter(filePath: String): Resource[IO, PrintWriter] =
  Resource.make {
    IO.blocking(new PrintWriter(filePath))
  } { pw =>
    IO.blocking(pw.close()).handleErrorWith(_ => IO.unit)
  }

def readFileBinImmediate(filePath: String): Array[Byte] =
  Files.readAllBytes(Paths.get(filePath))

def readFileBin(filePath: String): IO[Array[Byte]] =
  IO.blocking(Files.readAllBytes(Paths.get(filePath)))

def readFileBinLib(filePath: String): IO[Array[Byte]] =
  readFileBin(filePath) map { bytes =>
    val dotByte: Byte = 0x2E
    val dotIndex = bytes.indexOf(dotByte)

    val processedBytes = dotIndex match {
      case -1 => bytes
      case index => bytes.slice(0, index)
    }
    processedBytes
  }

def writeFile(filePath: String, content: String): IO[Unit] =
  printWriter(filePath).use { pw =>
    IO.blocking(pw.write(content))
  }

def writeFileBin(filePath: String, content: Array[Byte]): IO[Unit] =
  IO.blocking(Files.write(Paths.get(filePath), content))

def getFilename(filePath: String): String = {
  File(filePath).getName.stripSuffix(".sasl")
}

def parseUnoptimized(
                      file: String,
                      libs: List[String],
                    ): IO[Either[ParseError, (ParseTreeUnoptimized, ScopesRes, VariableMapUnoptimizedRes)]] = {
  val libCons: IO[Array[Byte]] =
    libs.traverse(readFileBinLib).map { results =>
      val separatorByte: Byte = 0x0A
      val concatenatedBytes = results.reduceOption(_ ++ Array(separatorByte) ++ _).getOrElse(Array.emptyByteArray)
      concatenatedBytes
    }

  for {
    raw <- readFileBin(file)
    full <- libCons.map(_ ++ Array[Byte](0x0A) ++ raw)
    lexer <- IO.delay(Lexer(full))
    res <- IO.delay(parserRDUnoptimized(lexer))
  } yield res
}

enum CommandCompileError:
  case P(pErr: ParseError)
  case C(cErr: CompilerError)

def compileUnoptimized(
                        file: String,
                        libs: List[String],
                      ): IO[Either[CommandCompileError, ReductionTreeUnoptimized]] = {
  (for {
    parseResult <- EitherT(parseUnoptimized(file, libs)).leftMap(CommandCompileError.P.apply)
    (pt, scopes, varMap) = parseResult
    c = CompilerUnoptimized()
    compiledResult <- EitherT(c.compileProgram(pt, scopes, varMap)).leftMap(CommandCompileError.C.apply)
  } yield compiledResult).value
}

object SaslCompilerApp extends CommandIOApp(
  name = "saslc",
  header = "SASL Compiler written in Scala.",
  version = "0.0.x"
) {
  private def handleCompileError(error: CommandCompileError): IO[ExitCode] = error match {
    case CommandCompileError.P(ParseError.WrongToken(e, a)) =>
      IO.println(s"Encountered error during parsing: expected token ${e.productPrefix} but received ${a.productPrefix}!") >> IO.pure(ExitCode.Error)
    case CommandCompileError.P(ParseError.WrongFirst(nt, t)) =>
      IO.println(s"Encountered error during parsing: expected some ${nt.productPrefix} but received ${t.productPrefix}!") >> IO.pure(ExitCode.Error)
    case CommandCompileError.P(ParseError.UnexpectedEnding(t)) =>
      IO.println(s"Encountered error during parsing: expected token ${t.productPrefix} but file ended!") >> IO.pure(ExitCode.Error)
    case CommandCompileError.P(ParseError.UnexpectedEmpty(nt)) =>
      IO.println(s"Encountered error druing parsing: expected some ${nt.productPrefix} but file ended!") >> IO.pure(ExitCode.Error)
    case CommandCompileError.P(parseError) =>
      // ToDo: better parser errors
      IO.println(s"Encountered error during parsing!") >> IO.pure(ExitCode.Error)
    case CommandCompileError.C(CompilerError.UnresolvedVariable(n)) =>
        IO.println(s"Encountered error during compilation: Unresolved variable \"$n\"!") >> IO.pure(ExitCode.Error)
  }

  private def handleDeserializationError(error: DeserializationError): IO[ExitCode] = error match {
    case DeserializationError.IdNotFound(id) => IO.println(s"Encountered error during deserialization: id $id not found!") >> IO.pure(ExitCode.Error)
    case DeserializationError.UnknownConstTag(tag) => IO.println(s"Encountered error during deserialization: unknown constant type tag $tag!") >> IO.pure(ExitCode.Error)
    case DeserializationError.UnknownNodeTag(tag) => IO.println(s"Encountered error during deserialization: unknown node type tag $tag!") >> IO.pure(ExitCode.Error)
    case DeserializationError.ExpectedApplication(rt) => IO.println(s"Encountered error during deserialization: expected application got ${rt.productPrefix}!") >> IO.pure(ExitCode.Error)
  }

  override def main: Opts[IO[ExitCode]] =
    optsVisualizeParseTree
      .orElse(optsExecute)
      .orElse(optsVisualizeCompileTree)
      .orElse(optsCompile)
      .orElse(optsExecuteB)
      .map {

      case VisualizeParseTree(file, libs) =>
        parseUnoptimized(file, libs) flatMap {
          case Right(pt, _, varMap) =>
            val vis = VisualizerParseTreeUnoptimized()
            for {
              d <- IO.delay(vis.generateDot(pt, varMap))
              fn = getFilename(file)
              _ <- writeFile(s"visualizations/$fn.dot", d) >> IO.println(s"Visualization saved to visualizations/$fn.dot")
            } yield ExitCode.Success
          case Left(e) => handleCompileError(CommandCompileError.P(e))
        }

      case VisualizeCompileTree(file, libs) =>
        compileUnoptimized(file, libs) flatMap {
          case Right(rt) =>
            val vis = VisualizerReductionTreeUnoptimized()
            for {
              d <- IO.delay(vis.generateDot(rt))
              fn = getFilename(file)
              _ <- writeFile(s"visualizations/rt/$fn.dot", d) >> IO.println(s"Visualization saved to visualizations/rt/$fn.dot")
            } yield ExitCode.Success
          case Left(e) => handleCompileError(e)
        }

      case Execute(file, libs, optimizer) =>
        compileUnoptimized(file, libs) flatMap {
          // ToDo: adapt Exit Code on whether there was a runtime error
          case Right(rt) => evalAndPrintUnoptimized(rt) *> IO.pure(ExitCode.Success)
          case Left(e) => handleCompileError(e)
        }

      case Compile(file, libs) =>
        compileUnoptimized(file, libs) flatMap {
          case Right(rt) =>
            val s = SerializerUnoptimized
            for {
              bin <- IO.delay(s.serialize(rt))
              fn = getFilename(file)
              _ <- writeFileBin(s"bin/${fn}.saslbin", bin) >> IO.println(s"Visualization saved to bin/${fn}.saslbin")
            } yield ExitCode.Success
          case Left(e) => handleCompileError(e)
        }
      case ExecuteB(file, optimizer) =>
        val s = SerializerUnoptimized
        for {
          bin <- readFileBin(file)
          res <- IO.delay(s.deserialize(bin)) flatMap {
            case Right(rt) => evalAndPrintUnoptimized(rt) *> IO.pure(ExitCode.Success)
            case Left(e) => handleDeserializationError(e)
          }
        } yield res
    }
}
