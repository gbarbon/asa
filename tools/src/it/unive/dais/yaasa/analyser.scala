package it.unive.dais.yaasa

/**
 * @author esteffin
 * @author gbarbon
 */

import utils.prelude._
import utils.pretty_print._
import utils.env._
import it.unive.dais.yaasa.abstract_values._
import absyn._
import scala.collection.breakOut

/**
 *
 */
object analyser {
  /**
   * Must require the insertion of the confidential labels before the execution.
   *
   * 1) The user must manually insert all the confidential label used in the program.
   * 2) Or we must introduce a way to locate them in the code.
   *    But, maybe it is faster to insert the list of all the confidential labels before,
   *    rather than searching them inside the code and enrich the code with something that locate the label.
   * 3) We can think to load both the code file with a label file.
   * 4) Or, we can modify the code and insert at the beginning the list of confidential labels. <---
   * 5) or we can recognize them with a function readLabel <---
   * --> OR GIVE ALL THE OPTIONS TO THE USER <--
   *
   * So at the begin, all the label objects are created.
   */

  /**
   * @param program
   * @return
   */
  def evaluateProgram(program: Program) = {
    program.classes match {
      case List() => throw new Unexpected("Empty class definition.")
      case c :: _ =>
      //evaluateClass(c)
    }
  }

  //def evaluateLabel()
}
