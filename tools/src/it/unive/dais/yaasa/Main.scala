package it.unive.dais.yaasa

import scala.io.Source._
import parser._
import evaluator._
import utils.prelude._
import utils.env._

/**
 * @author esteffin
 */

object Main {
  def main(args: Array[String]) {
    try {
      println("yaasa is growin' up!")

      val dir = "main/resources/"
      val filename = "simple.java"
      val source = fromFile(dir.concat(filename), "utf-8")
      val lines = try source.getLines mkString "\n" finally source.close()
      val test = TestFJPPParser.parse(lines)

      val (res, env) = evaluator.evaluateProgram(test)

      println(env.pretty)
      println(res)
    }
    catch {
      case e: MessageException => println(e.message)
    }
  }
}
