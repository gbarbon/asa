package it.unive.dais.yaasa.datatype

import lattice._
//import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.utils._
import it.unive.dais.yaasa.utils.prelude._
//import it.unive.dais.yaasa.utils.pretty_print._
//import ADType._
//import it.unive.dais.yaasa.utils.collection.list._
//import it.unive.dais.yaasa.utils.collection.map._
//import it.unive.dais.yaasa.functConvert._

/**
 * @author esteffin
 * @author gbarbon
 */
object LMH {

  /**
   * Implementation of the Low Medium High lattice used for the confidentiality
   */
  object CLattice {
    sealed trait LMHV extends pretty

    case object Low extends LMHV { override def pretty = "Low" }
    case object Medium extends LMHV { override def pretty = "Medium" }
    case object High extends LMHV { override def pretty = "High" }

    class LMHVLattice private (content: CLattice.LMHV) extends Lattice[LMHV] with pretty {

      override val cnt: LMHV = content

      override def <==[B >: LMHV](r: Lattice[B]): Boolean = {
        r.cnt match {
          case v: LMHV =>
            (content, v) match {
              case (Low, _) => true
              case (Medium, Low) => false
              case (Medium, _) => true
              case (High, High) => true
              case (High, _) => false
            }
          case _ => throw new MessageException("Argument error: trying to combine LMHV value with something else.")
        }
      }
      override def join[B >: LMHV](r: Lattice[B]): Lattice[B] = {
        r.cnt match {
          case v: LMHV => new LMHVLattice (if (this <== r) v else content)
          case _ => throw new MessageException("Argument error: trying to combine LMHV value with something else.")
        }
      }
      override def meet[B >: LMHV](r: Lattice[B]): Lattice[B] = {
        r.cnt match {
          case v: LMHV => new LMHVLattice (if (this <== r) content else v)
          case _ => throw new MessageException("Argument error: trying to combine LMHV value with something else.")
        }
      }

      override def pretty: String = cnt.pretty
    }
    object LMHVLattice extends LatticeFactory[LMHV] with parsable[LMHVLattice] {
      //FIXME: this direct constructor should not be used...
      def low = new LMHVLattice(Low)
      def medium = new LMHVLattice(Medium)
      def high = new LMHVLattice(High)

      def top: Lattice[LMHV] = new LMHVLattice(High)
      def bottom: Lattice[LMHV] = new LMHVLattice(Low)
      def parse(s: String): LMHVLattice = {
        s match {
          case "L"   => new LMHVLattice(Low)
          case "M"   => new LMHVLattice(Medium)
          case "H"   => new LMHVLattice(High)
          case error => throw parsingUtils.ParseError("Error parsing %s, not a valid HML string." format error)
        }
      }
    }
  }

  //TODO: Find a better implementation
  type ConfLattice = Lattice[CLattice.LMHV]
  val ConfLatticeFactory: LatticeFactory[CLattice.LMHV] with parsable[CLattice.LMHVLattice] = CLattice.LMHVLattice

  type ObfLattice = Lattice[CLattice.LMHV]
  val ObfLatticeFactory: LatticeFactory[CLattice.LMHV] with parsable[CLattice.LMHVLattice] = CLattice.LMHVLattice

}
