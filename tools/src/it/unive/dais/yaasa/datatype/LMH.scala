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

    implicit def lattice(l: LMHV): Lattice[LMHV] = new Lattice[LMHV] {

      def <==(r: LMHV): Boolean =
        (l, r) match {
          case (Low, _)      => true
          case (Medium, Low) => false
          case (Medium, _)   => true
          case (High, High)  => true
          case (High, _)     => false
        }
      def join(r: LMHV): LMHV =
        if (this <== r) r else l
      def meet(r: LMHV): LMHV =
        if (this <== r) l else r
      override def toString() = l.toString()
    }

    object Factory extends LatticeFactory[LMHV] {
      def top: Lattice[LMHV] = High
      def bottom: Lattice[LMHV] = Low
      def parse(s: String): Lattice[LMHV] = {
        s match {
          case "L"   => Low
          case "M"   => Medium
          case "H"   => High
          case error => throw parsingUtils.ParseError("Error parsing %s, not a valid HML string." format (error))
        }
      }
    }
  }

  //TODO: Find a better implementation
  type ConfLattice = Lattice[CLattice.LMHV]
  val ConfLatticeFactory: LatticeFactory[CLattice.LMHV] = CLattice.Factory

  type ObfLattice = Lattice[CLattice.LMHV]
  val ObfLatticeFactory: LatticeFactory[CLattice.LMHV] = CLattice.Factory

}
