package it.unive.dais.yaasa

import it.unive.dais.yaasa.parser._

/**
 * @author esteffin
 */

object Main {
  def main(args: Array[String]) {
    println("yaasa is growin' up!")

    val p = TestLoopParser.parse("for x in 1 to 42 { for y in 0 to 1 {} }")

    val j = (1, "", false, (1, 2))

    println(p)
  }
}
