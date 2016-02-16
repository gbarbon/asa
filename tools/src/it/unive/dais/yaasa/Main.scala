package it.unive.dais.yaasa

import scala.io.Source._
import parser._
import absyn._
//import utils.prelude._
//import utils.env._
import java.io.File

/**
 * @author esteffin
 */

object Main {
  /*
  trait AbsInt[T] extends pretty { protected val cnt: T; def f(other: AbsInt[T]): AbsInt[T] }
  trait AbsIntFactory[T] {def create(x:Int): AbsInt[T]}

  class Conc(val x: Int) extends pretty {
    def f(other: Conc) = new Conc(this.x + other.x)

    override def pretty: String = x.toString
  }

  implicit def toAbst(a: Conc): AbsInt[Conc] = {
    new AbsInt[Conc]{
      val cnt = a
      override def f(other: AbsInt[Conc]): AbsInt[Conc] = toAbst(a.f(other.cnt))
      override def pretty: String = a.pretty
    }
  }
  type Aa = AbsInt[Conc]
  object AaFactory extends AbsIntFactory[Conc] {
    override def create(x: Int): AbsInt[Conc] = new Conc(x)
  }*/

  def main(args: Array[String]) {
//    if (true) {
//      unitTest.unitMain()
//    }
    try {
      println("yaasa is growin' up!")
      if (constants.DEBUG)
        config.initialize(List("--verbose","--widening-threshold","15","main/resources/ifelse.java"))
      else
        config.initialize(args)
      //@FIXME: Fix argument passing: if not defined, choose defaults
      val op_annots = operators.parse(config.value.operators)
      val libs_ast =
        for (lib <- config.value.libs)
          yield qualifiedRename.qualifyProgram(FJPPParser.parse(library=true, op_annots, fromFile(new File(lib), "utf-8").getLines().mkString("\n"), lib))
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
      val core = new analyzer.Analyzer(test)
      val (res, env) = core.evaluateProgram()

      //println(env.pretty)
      println("\n\nAnalysis logs:")
      core.logs.reverse foreach { vwa => println(vwa.adInfo) }
      //println(core.logs.length)
    }
    catch {
      case e: /*MessageException*/ArrayIndexOutOfBoundsException => println(e/*.message*/)
    }
  }
}
