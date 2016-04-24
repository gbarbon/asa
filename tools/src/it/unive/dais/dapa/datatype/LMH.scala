package it.unive.dais.dapa.datatype

/**
  * @author esteffin
  * @author gbarbon
  */

import it.unive.dais.dapa.datatype.lattice._
import it.unive.dais.dapa.utils._
import it.unive.dais.dapa.utils.prelude._

object LMH {

  // Implementation of the Low Medium High lattice used for the confidentiality
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

  type ConfLattice = Lattice
  val ConfLatticeFactory: LatticeFactory with parsable[CLattice.LMHVLattice] = CLattice.LMHVLattice

  type ObfLattice = Lattice
  val ObfLatticeFactory: LatticeFactory with parsable[CLattice.LMHVLattice] = CLattice.LMHVLattice

}
