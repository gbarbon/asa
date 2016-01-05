package it.unive.dais.yaasa

import scala.io.Source._
import java.io.File
import datatype.ADType._
import absyn._

/**
 * @author esteffin
 */
object operators {
  def parse(f: String): Map[String, FunAnnot] = {
    val lines = fromFile(f, "utf-8").getLines().mkString("\n")
    val rows = annotParser.AnnotationParser.parseAll(lines)
    rows mapValues {
      case a @ FunAnnot(_, _) => a
      case _                  => throw new utils.prelude.Unexpected("Wrong annotation in operation resource")
    }
  }
}
