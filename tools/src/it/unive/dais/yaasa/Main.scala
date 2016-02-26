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

  import utils.pretty_doc._
  import org.kiama.output.PrettyPrinter._

  case class Container[A[_]  <: Iterable[_] , B](cnt: A[B]) {

    def pretty: String = cnt.foldLeft(empty)( (acc, e) => acc <+> value(e)).pretty
  }



  def main(args: Array[String]) {
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
      println("yaasa is growin' up!")
      if (constants.DEBUG) {
        //val input = "main/resources/whileTestSet/whileComplex.java"
        val input = "main/resources/ifelse.java"
        config.initialize(List("--verbose","--widening-threshold","15",input)) }
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
      core.logs.reverse foreach { vwa => println(vwa.adInfo.pretty_doc.pretty) }
      //println(core.logs.length)
    }
    catch {
      case e: /*MessageException*/ArrayIndexOutOfBoundsException => println(e/*.message*/)
    }
  }
}
