package it.unive.dais.dapa

import scala.io.Source._
import parser._
import absyn._
import org.kiama.output.PrettyPrinter._
//import utils.prelude._
//import utils.env._
import utils.pretty_doc._
import java.io.File

/**
 * @author esteffin
 */

object Main {

  def main_body(args: Array[String]): Unit = {
    /*if (true) {
    val a = text("abc")
    val b = text("bcd")
    val c = text("cde")

    println(a.pretty)
    println((a <+> b).pretty)
    println((a <+> (b <%> c)).pretty)

    val jj: Container[List, Int] = Container[List, Int](List(1,2,3,4,5,1))
    println(jj.pretty)

    val jj2: Container[Set, Int] = Container[Set, Int](Set(1,2,3,4,5,1))
    println(jj2.pretty)



    return
    unitTest.unitMain()
  }*/
    try {
      println("dapa is growin' up!")
      if (constants.DEBUG) {
        //val input = "main/resources/whileTestSet/whileComplex.java"
        val input = {
          if (args.isEmpty) {
            val res = "main/resources/DroidBench/ArrayCopy1.java"
            println("Args are empty... Using %s" format res)
            res
          }
          else args(0)
        }
        config.initialize(List("--verbose","--widening-threshold","15",input)) }
      else
        config.initialize(args)
      //@FIXME: Fix argument passing: if not defined, choose defaults
      val op_annots = operators.parse(config.value.operators)
      val libs_ast =
        for (lib <- config.value.libs)
          yield qualifiedRename.qualifyProgram(FJPPParser.parse(library=true,  op_annots, fromFile(new File(lib), "utf-8").getLines().mkString("\n"), lib))
      val srcs_ast =
        for (src <- config.value.sources)
          yield qualifiedRename.qualifyProgram(FJPPParser.parse(library=false, op_annots, fromFile(new File(src), "utf-8").getLines().mkString("\n"), src))
      //yield qualifiedRename.qualifyProgram(FJPPParser.parse(true, lines))
      val test =
      {
        val libs: List[Class] = (for (Program(classes) <- libs_ast) yield classes) flatten
        val sources: List[Class] = (for (Program(classes) <- srcs_ast) yield classes) flatten
        val res = Program(libs ++ sources)
        res
      }

      //println(test.pretty)
      val (core, (res, env)) = utils.profiling.execute_print_time("Analysis") { _ =>
        val core = new analyzer.Analyzer(test)
        (core, core.evaluateProgram()) }

      //println(env.pretty)
      println("\n\nAnalysis logs:")
      core.logs.reverse foreach { vwa => println(vwa.pretty_doc.pretty) }
      //println(core.logs.length)
    }
    catch {
      case e: /*MessageException*/ArrayIndexOutOfBoundsException => println(e/*.message*/)
    }
  }

  def main(args: Array[String]) {
    /* utils.profiling.execute_print_time("Analysis") { _ => */ main_body(args) //}

    //println(("Boolean" <+> ("non top:" <+> value(abstract_types.statistics.boolReg) <%> "top:" <+> value(abstract_types.statistics.boolTop))).pretty)
    //println(("Numbers" <+> ("non top:" <+> value(abstract_types.statistics.numReg) <%> "top:" <+> value(abstract_types.statistics.numTop))).pretty)
    //println(("Strings" <+> ("non top:" <+> value(abstract_types.statistics.stringReg) <%> "top:" <+> value(abstract_types.statistics.stringTop))).pretty)

    //main_body(args)
  }
}
