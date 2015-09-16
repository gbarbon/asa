package it.unive.dais.yaasa.datatype

import type_definitions._
import it.unive.dais.yaasa.utils._
import ADType._

/**
 * @author esteffin
 * @author gbarbon
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
          case error => throw parsingUtils.ParseError("Error parsing %s, not a valid HML string." format (error))
        }
      }
    }
  }

  type ConfLattice = Lattice[CLattice.LMHV]

  object CADInfo {

    // an entry of the ADExp map
    /**
     * @constructor create a new atomic data expression of a certain label.
     * @param oExpStm Over approximation of the statements applied to the label (explicit flow, not used at this time)
     * @param uExpStm Under approximation of the statements applied to the label (explicit flow, not used at this time)
     * @param oImplStm Over approximation of the statements applied to the label (implicit flow)
     * @param uImplStm Under approximation of the statements applied to the label (implicit flow)
     */
    private case class Entry(
        oExpStm: List[EStatement] = List(),
        uExpStm: List[EStatement] = List(),
        oImplStm: List[EStatement] = List(),
        uImplStm: List[EStatement] = List(),
        explQuant: BitQuantity = BitQuantity(),
        implQuant: BitQuantity = BitQuantity()) {

      /**
       * "add" methods for statements lists
       * @param stm a statement
       */

      def addOExpStm(stm: EStatement) = this.copy(oExpStm = stm :: oExpStm)
      def addUExpStm(stm: EStatement) = this.copy(uExpStm = stm :: uExpStm)
      def addOImplStm(stm: EStatement) = this.copy(oImplStm = stm :: oImplStm)
      def addUImpltm(stm: EStatement) = this.copy(uImplStm = stm :: uImplStm)
      def addExpStm(stm: EStatement) = this.copy(oExpStm = stm :: oExpStm).copy(uExpStm = stm :: uExpStm)
      def addImplStm(stm: EStatement) = this.copy(oImplStm = stm :: oImplStm).copy(uImplStm = stm :: uImplStm)
      def updateImplQuant(qnt: BitQuantity) = implQuant.update(qnt)

    }

    private object Entry {
      def empty = Entry()
    }

    //a map Label -> Entry
    class SetADInfo private (private val theMap: Map[Label, Entry] = Map()) extends ADInfo {

      private[CADInfo] def this() = this(Map.empty[Label, Entry])

      private[CADInfo] def this(labels: List[Label]) =
        this((for (label <- labels) yield (label, Entry.empty)).toMap)

      def newExplStm(aLabel: Label, aStm: EStatement) = {
        if (theMap contains aLabel) {
          val value = theMap(aLabel)
          theMap.updated(aLabel, value.addExpStm(aStm))
        }
        else {
          val newEntry = new Entry(oExpStm = List(aStm), uExpStm = List(aStm))
          val tempMap = (List(aLabel) zip List(newEntry)).toMap
          new SetADInfo(tempMap ++ theMap)
        }
      }

      def newImplStm(aLabel: Label, aStm: EStatement) = {
        if (theMap contains aLabel) {
          val value = theMap(aLabel)
          theMap.updated(aLabel, value.addImplStm(aStm))
        }
        else {
          val newEntry = new Entry(oImplStm = List(aStm), uImplStm = List(aStm))
          val tempMap = (List(aLabel) zip List(newEntry)).toMap
          new SetADInfo(tempMap ++ theMap)
        }
      }

      //def updExplQnt(aLabel: Label, aQnt: BitQuantity)

      def updImplQnt(aLabel: Label, aQnt: BitQuantity) = {
        if (theMap contains aLabel) {
          val value = theMap(aLabel)
          theMap.updated(aLabel, value.updateImplQuant(aQnt))
        }
        else {
          val newEntry = new Entry(implQuant = aQnt)
          val tempMap = (List(aLabel) zip List(newEntry)).toMap
          new SetADInfo(tempMap ++ theMap)
        }
      }

      def returnExplStms(aLabel: Label): (List[EStatement], List[EStatement]) = (theMap(aLabel).oExpStm, theMap(aLabel).uExpStm)
      def returnImplStms(aLabel: Label): (List[EStatement], List[EStatement]) = (theMap(aLabel).oImplStm, theMap(aLabel).uImplStm)
      def returnExplQnt(aLabel: Label): BitQuantity = theMap(aLabel).explQuant
      def returnImplQnt(aLabel: Label): BitQuantity = theMap(aLabel).implQuant

      /**
       * override def toString() = {
       * def print_stmts(l: List[EStatement]) = utils.pretty_print.xhcat(",")(l map { _.toString() })
       * "<(%s), %s, %s, %s>" format (label.toString(), print_stmts(oExpStm), print_stmts(oImplStm), implQuant.toString())
       * }
       */

    }

    object Factory extends ADInfoFactory {
      def newInfo(labels: List[Label]): SetADInfo = {
        new SetADInfo(labels)
      }
    }
  }

  type CADInfo = CADInfo.SetADInfo
}
