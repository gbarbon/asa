package it.unive.dais.yaasa

import scala.io.Source._
import parser._
import absyn._
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
      if (constants.DEBUG)
        config.initialize(List("main/resources/simple.java"))
      else
        config.initialize(args)
      val libs_ast =
        for (lib <- config.value.libs)
          yield qualifiedRename.qualifyProgram(FJPPParser.parse(true, fromFile(lib, "utf-8").getLines().mkString("\n")))
      val srcs_ast =
        for (lib <- config.value.sources)
          yield qualifiedRename.qualifyProgram(FJPPParser.parse(false, fromFile(lib, "utf-8").getLines().mkString("\n")))
      //yield qualifiedRename.qualifyProgram(FJPPParser.parse(true, lines))
      val test =
        {
          val libs: List[Class] = (for (Program(classes) <- libs_ast) yield classes) flatten
          val sources: List[Class] = (for (Program(classes) <- srcs_ast) yield classes) flatten
          val res = Program(libs ++ sources)
          res
        }

      //println(test.pretty)

      val (res, env) = analyzer.evaluateProgram(test)

      println(env.pretty)
      println(res)
    }
    catch {
      case e: MessageException => println(e.message)
    }
  }
}
