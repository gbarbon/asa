package it.unive.dais.yaasa.datatype

import lattice._
import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.utils._
import it.unive.dais.yaasa.utils.prelude._
import it.unive.dais.yaasa.utils.pretty_print._
import ADType._
import it.unive.dais.yaasa.utils.collection.list._
import it.unive.dais.yaasa.utils.collection.map._
import it.unive.dais.yaasa.functConvert._

/**
 * @author esteffin
 * @author gbarbon
 */
object LMH {

  /**
   * Implementation of the Low Medium High lattice used for the confidentiality
   */
  object CLattice {
    sealed trait LMHV

    case object Low extends LMHV { override def toString() = "Low" }
    case object Medium extends LMHV { override def toString() = "Medium" }
    case object High extends LMHV { override def toString() = "High" }

    class LMHVLattice private (content: CLattice.LMHV) extends Lattice[LMHV] {

      override val cnt: LMHV = content

      def <==(r: Wrapper[LMHV]): Boolean =
        (content, r.cnt) match {
          case (Low, _)      => true
          case (Medium, Low) => false
          case (Medium, _)   => true
          case (High, High)  => true
          case (High, _)     => false
        }
      def join(r: Wrapper[LMHV]): Lattice[LMHV] =
        new LMHVLattice(if (this <== r) r.cnt else content)
      def meet(r: Wrapper[LMHV]): Lattice[LMHV] =
        new LMHVLattice(if (this <== r) content else r.cnt)
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
          case error => throw parsingUtils.ParseError("Error parsing %s, not a valid HML string." format (error))
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
