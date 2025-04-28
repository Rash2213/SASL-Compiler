package cli

import lexer.Lexer

import java.io.FileInputStream

def readFileBin(fileName: String): Array[Byte] = {
  val fileInputStream = new FileInputStream(fileName)
  val byteLength = fileInputStream.available()
  val bytesArray = new Array[Byte](byteLength)
  fileInputStream.read(bytesArray)
  fileInputStream.close()
  bytesArray
}

@main def cli(others: String*): Unit = {
  val raw = readFileBin("./examples/qsort.sasl")
  val lexer = Lexer(raw)
  lexer.foreach(t => println(t))
}