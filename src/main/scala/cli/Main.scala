package cli

import lexer.Lexer

import java.io.{FileInputStream, File}

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

@main def cli(others: String*): Unit = {
  val raw = readFileBin("./examples/qsort.sasl")
  val lexer = Lexer(raw)
  lexer.foreach(t => println(t))
}