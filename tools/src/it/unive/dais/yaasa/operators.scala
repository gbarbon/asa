package it.unive.dais.yaasa

import scala.io.Source._
import java.io.File
import abstract_values._
import abstract_values.LMH._
import absyn._

/**
 * @author esteffin
 */
object operators {
  def parse(f: String): Map[String, FunAnnot] = {
    val lines = fromFile(f, "utf-8").getLines().mkString("\n").split('\n')
    val parts = for (line <- lines if (line.trim() != "")) yield (line.split(','))
    val rows = (for (row <- parts) yield (row(0), utils.pretty_print.xvcat(",")(row.tail))).toMap
    rows mapValues { s =>
      annotParser.AnnotationParser.parse(s) match {
        case a @ FunAnnot(_, _, _) => a
        case _                     => throw new utils.prelude.Unexpected("Wrong annotation in operation resource")
      }
    }
  }
}
