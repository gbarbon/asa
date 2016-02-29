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
    trait LMHVLattice extends Lattice with pretty {
      override def <==(r: Lattice): Boolean = {
        r match {
          case v: LMHVLattice =>
            (this, v) match {
              case (Low, _) => true
              case (Medium, Low) => false
              case (Medium, _) => true
              case (High, High) => true
              case (High, _) => false
            }
          case _ => throw new MessageException("Argument error: trying to combine LMHV value with something else.")
        }
      }
      override def join(r: Lattice): Lattice = {
        r match {
          case v: LMHVLattice => if (this <== r) v else this
          case _ => throw new MessageException("Argument error: trying to combine LMHV value with something else.")
        }
      }
      override def meet(r: Lattice): Lattice = {
        r match {
          case v: LMHVLattice => if (this <== r) this else v
          case _ => throw new MessageException("Argument error: trying to combine LMHV value with something else.")
        }
      }
    }

    case object Low extends LMHVLattice { override def pretty = "Low" }
    case object Medium extends LMHVLattice { override def pretty = "Medium" }
    case object High extends LMHVLattice { override def pretty = "High" }


    object LMHVLattice extends LatticeFactory with parsable[LMHVLattice] {
      //FIXME: this direct constructor should not be used...
      def low = Low
      def medium = Medium
      def high = High

      def top: Lattice = High
      def bottom: Lattice = Low
      def parse(s: String): LMHVLattice = {
        s match {
          case "L"   => Low
          case "M"   => Medium
          case "H"   => High
          case error => throw parsingUtils.ParseError("Error parsing %s, not a valid HML string." format error)
        }
      }
    }
  }

  //TODO: Find a better implementation
  type ConfLattice = Lattice
  val ConfLatticeFactory: LatticeFactory with parsable[CLattice.LMHVLattice] = CLattice.LMHVLattice

  type ObfLattice = Lattice
  val ObfLatticeFactory: LatticeFactory with parsable[CLattice.LMHVLattice] = CLattice.LMHVLattice

}
