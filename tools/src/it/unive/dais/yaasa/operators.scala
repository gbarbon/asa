package it.unive.dais.yaasa

import scala.io.Source._
import java.io.File
import abstract_values._
import abstract_values.LMH._

/**
 * @author esteffin
 */
object operators {
  def parse(f: File): Map[String, (String, Lattice[_], Int)] = {
    val lines = fromFile(f, "utf-8").getLines().mkString("\n").split('\n')
    val parts = for (line <- lines if (line.trim() != "")) yield (line.split(','))
    val rows = ((for (row <- parts) yield (row(0), (row(1), LMHFactory.parse(row(2)), row(3).toInt))) toMap)
    rows
  }
}
