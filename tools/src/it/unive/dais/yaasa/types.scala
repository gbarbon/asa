package it.unive.dais.yaasa

import datatype.type_definitions._

/**
 * @author esteffin
 */
object types {

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
      def top: ConfLattice = High
      def bottom: ConfLattice = Low
      def parse(s: String): ConfLattice = {
        s match {
          case "L"   => Low
          case "M"   => Medium
          case "H"   => High
          case error => throw utils.parsingUtils.ParseError("Error parsing %s, not a valid HML string." format (error))
        }
      }
    }
  }

  type ConfLattice = Lattice[CLattice.LMHV]
}
