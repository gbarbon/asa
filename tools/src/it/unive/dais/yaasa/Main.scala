package it.unive.dais.yaasa

import parser._
import scala.io.Source._

/**
 * @author esteffin
 */

object Main {
  def main(args: Array[String]) {
    println("yaasa is growin' up!")

    //val p = TestLoopParser.parse("class c extends o { void pippo() {int a; a = (a + 1); folle(a);}}")

    val dir = "../test_files/"
    val filename = "simple.java"
    val source = fromFile(dir.concat(filename), "utf-8")
    val lines = try source.getLines.mkString finally source.close()
    val test = TestLoopParser.parse(lines)

    //val j = (1, "", false, (1, 2))

    println(test)
  }
}
