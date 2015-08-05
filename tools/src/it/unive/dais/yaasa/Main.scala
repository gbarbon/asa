package it.unive.dais.yaasa

import it.unive.dais.yaasa.parser._

/**
 * @author esteffin
 */

object Main {
  def main(args: Array[String]) {
    println("yaasa is growin' up!")

    val p = TestLoopParser.parse("class c extends o { void pippo() {int a; a = (a + 1); folle(a);}}")

    val j = (1, "", false, (1, 2))

    println(p)
  }
}
