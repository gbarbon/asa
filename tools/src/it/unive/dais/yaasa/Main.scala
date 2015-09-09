package it.unive.dais.yaasa

import scala.io.Source._
import parser._
import absyn._
import evaluator._
import utils.prelude._
import utils.env._
import java.io.File

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
      //@FIXME: Fix argument passing: if not defined, choose defaults
      val op_annots = operators.parse(config.value.operators)
      val libs_ast =
        for (lib <- config.value.libs)
          yield qualifiedRename.qualifyProgram(FJPPParser.parse(true, op_annots, fromFile(new File(lib), "utf-8").getLines().mkString("\n"), lib))
      val srcs_ast =
        for (src <- config.value.sources)
          yield qualifiedRename.qualifyProgram(FJPPParser.parse(false, op_annots, fromFile(new File(src), "utf-8").getLines().mkString("\n"), src))
      //yield qualifiedRename.qualifyProgram(FJPPParser.parse(true, lines))
      val test =
        {
          val libs: List[Class] = (for (Program(classes) <- libs_ast) yield classes) flatten
          val sources: List[Class] = (for (Program(classes) <- srcs_ast) yield classes) flatten
          val res = Program(libs ++ sources)
          res
        }

      //println(test.pretty)

      val (res, env) = new analyzer.Analyzer(test).evaluateProgram()

      println(env.pretty)
      println(res)
    }
    catch {
      case e: MessageException => println(e.message)
    }
  }
}
