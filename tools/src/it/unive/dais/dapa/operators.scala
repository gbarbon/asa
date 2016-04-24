package it.unive.dais.dapa

/**
  * @author esteffin
  * @author gbarbon
  */

import it.unive.dais.dapa.datatype.GenTypes._

import scala.io.Source._

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
