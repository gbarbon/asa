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
  def parse(f: File): Map[String, FunAnnot] = {
    val lines = fromFile(f, "utf-8").getLines().mkString("\n").split('\n')
    val parts = for (line <- lines if (line.trim() != "")) yield (line.split(','))
    val rows = ((for (row <- parts; part <- parts) yield parts.split())

        //(row(0), FunAnnot.parse("%s%s%s")) toMap)
    rows
  }
}
