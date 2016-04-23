package it.unive.dais.dapa

import it.unive.dais.dapa.analyzer.Analyzer

import scala.io.Source._
import parser._
import absyn._
import utils.prelude._
//import utils.prelude._
//import utils.env._
import utils.pretty_doc._
import java.io.File

/**
 * @author esteffin
 */

object Main {

  def initialize(args: Array[String]): Unit = {
    /*if (constants.DEBUG) {
      //val input = "main/resources/whileTestSet/whileComplex.java"
      val input = {
        if (args.isEmpty) {
          val res = "main/resources/DroidBench/ImplicitFlow1.java"
          println("Args are empty... Using %s" format res)
          res
        }
        else args(0)
      }
      val operators = "main/resources/libraries/operators.csv"
      val libs = "main/resources/libraries/stdlib.java,main/resources/libraries/readlib.java"
      config.initialize(List("--operators", operators, "--libs", libs, "--widening-threshold", "15", input))
    }
    else*/
    config.initialize(args)
  }

  def parse = {
    val op_annots = operators.parse(config.value.operators)
    val libs_ast =
      for (lib <- config.value.libs)
        yield qualifiedRename.qualifyProgram(FJPPParser.parse(library = true, op_annots, fromFile(new File(lib), "utf-8").getLines().mkString("\n"), lib))
    val srcs_ast =
      for (src <- config.value.sources)
        yield qualifiedRename.qualifyProgram(FJPPParser.parse(library = false, op_annots, fromFile(new File(src), "utf-8").getLines().mkString("\n"), src))
    val unified = {
      val libs: List[Class] = (for (Program(classes) <- libs_ast) yield classes) flatten
      val sources: List[Class] = (for (Program(classes) <- srcs_ast) yield classes) flatten
      val res = Program(libs ++ sources)
      res
    }
    unified
  }

  def main_body(args: Array[String]): Unit = {
    try {
      initialize(args)
      val program = parse

      val ((core, (res, env)), ms) =
        utils.profiling.execute_time_get_milliseconds { _ =>
          val core = new Analyzer(program)
          (core, core.evaluateProgram()) }

      if (config.value.profile) {
        println("Analysis done in %s milliseconds..." format ms)
      }

      println("\nAnalysis logs:")
      core.logs.reverse foreach { vwa => println(vwa.pretty_doc.pretty) }
    }
    catch {
      case e: MessageException =>
        sys.error(e.message)
        sys.exit(1)
      case e: Exception =>
        sys.error(e.getMessage)
        sys.exit(1)
    }
  }

  def main(args: Array[String]) {
    main_body(args)
  }
}
