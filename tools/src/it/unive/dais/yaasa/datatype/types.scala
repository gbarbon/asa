package it.unive.dais.yaasa.datatype

import type_definitions._
import it.unive.dais.yaasa.utils._
import it.unive.dais.yaasa.utils.prelude._
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

  /**
   * object CFElement {
   * class SetFlowElement(name: String, obf: Obfuscation, implq: BitQuantity) extends FlowElement {
   * def pretty: String = name
   * }
   *
   * object Factory extends FlowElementFactory {
   * def newElem(name: String, obf: Obfuscation, implq: BitQuantity): SetFlowElement = {
   * new SetFlowElement(name, obf, implq)
   * }
   * }
   * }
   *
   * type CFElement = CFElement.SetFlowElement
   *
   */

  object CADInfo {

    private case class EStatement(name: String, obf: Obfuscation, implq: BitQuantity, aLabel: Label) /** extends FlowElement */ {

      //It prints the Statement operator or function, with the associated label
      def print = "<" + name + ", " + aLabel + ">" //"(" + name + ", " + aLabel + ")"
      override def toString() = print
    }

    private object EStatement {
      def sCreator(aLabel: Label, annot: FunAnnot) = EStatement(annot.name, annot.obfuscation, annot.quantity, aLabel)
    }

    // an entry of the ADExp map
    /**
     * @constructor create a new atomic data expression of a certain label.
     * @param oExpStm Over approximation of the statements applied to the label (explicit flow, not used at this time)
     * @param uExpStm Under approximation of the statements applied to the label (explicit flow, not used at this time)
     * @param oImplStm Over approximation of the statements applied to the label (implicit flow)
     * @param uImplStm Under approximation of the statements applied to the label (implicit flow)
     */
    private case class Entry(
        oExpStm: Set[EStatement] = Set.empty,
        uExpStm: Set[EStatement] = Set.empty,
        oImplStm: Set[EStatement] = Set.empty,
        uImplStm: Set[EStatement] = Set.empty,
        explQuant: BitQuantity = BitQuantity(),
        implQuant: BitQuantity = BitQuantity()) {

      /**
       * "add" methods for statements lists
       * @param stm a statement
       */

      def addOExpStm(stm: EStatement) = this.copy(oExpStm = oExpStm + stm)
      def addUExpStm(stm: EStatement) = this.copy(uExpStm = uExpStm + stm)
      def addOImplStm(stm: EStatement) = this.copy(oImplStm = oImplStm + stm)
      def addUImpltm(stm: EStatement) = this.copy(uImplStm = uImplStm + stm)
      def addExpStm(stm: EStatement) = this.copy(oExpStm = oExpStm + stm, uExpStm = uExpStm + stm)
      def addImplStm(stm: EStatement) = this.copy(oImplStm = oImplStm + stm, uImplStm = uImplStm + stm)
      def updateImplQuant(qnt: BitQuantity) = implQuant.update(qnt)
      def join(other: Entry): Entry = Entry.empty //FIXME: Implement here

    }

    private object Entry {
      def empty = Entry()
    }

    //a map Label -> Entry
    class SetADInfo private (private val theMap: Map[Label, Entry] = Map()) extends ADInfo {

      private[CADInfo] def this() = this(Map.empty[Label, Entry])

      private[CADInfo] def this(labels: List[Label]) =
        this((for (label <- labels) yield (label, Entry.empty)).toMap)

      //def update(elem: FlowElement) = Factory.newInfo(Label.star) //@FIXME: temporary solution
      def update(ann: FunAnnot): ADInfo = Factory.newInfo(Label.star) //@FIXME: temporary solution
      /**
       * check if label in B exist in A
       * if true
       *    update with statement (op, label) all label of set A
       *    update with statement (op, label) all label of set B
       * else
       *    retrieve all label names in A
       *    retrieve all label names in B
       *    create new adexp A+B: join
       *    update all A with stm (op, Li) for every i that belongs to B
       *    update all B with stm (op, Lj) for every J that belongs to A
       */

      //def update(anADExp: ADInfo, elem: FlowElement) = Factory.newInfo(Label.star) //@FIXME: temporary solution
      //def update(ADExps: List[ADInfo], elem: FlowElement) = Factory.newInfo(Label.star) //@FIXME: temporary solution
      def update(anADExp: ADInfo, ann: FunAnnot): ADInfo = Factory.newInfo(Label.star) //@FIXME: temporary solution
      def update(ADExps: List[ADInfo], ann: FunAnnot): ADInfo = {
        val adexps = (this :: ADExps) map { case s: SetADInfo => s case _ => throw new Unexpected("Wrong type implementation") }
        val keys = adexps.foldLeft(Set.empty[Label])((s, l) => s ++ l.theMap.keys)
        val joined =
          for (label <- keys)
            yield (label -> ((adexps map { _.getRowSafe(label) }).foldLeft(Entry.empty) { (acc, entry) => acc join entry }))
        //FIXME: qua devi aggiungere ad ogni elemento di joined la combinazione con ... Probabilmente BROKEN...
        Factory.newInfo(Label.star) //@FIXME: temporary solution
      }

      private def getRowSafe(lab: Label) =
        if (theMap contains lab)
          theMap(lab)
        else
          Entry.empty
      /**
       * def newExplStm(aLabel: Label, aStm: EStatement) = {
       * if (theMap contains aLabel) {
       * val value = theMap(aLabel)
       * theMap.updated(aLabel, value.addExpStm(aStm))
       * }
       * else {
       * val newEntry = new Entry(oExpStm = List(aStm), uExpStm = List(aStm))
       * val tempMap = (List(aLabel) zip List(newEntry)).toMap
       * new SetADInfo(tempMap ++ theMap)
       * }
       * }
       *
       * def newImplStm(aLabel: Label, aStm: EStatement) = {
       * if (theMap contains aLabel) {
       * val value = theMap(aLabel)
       * theMap.updated(aLabel, value.addImplStm(aStm))
       * }
       * else {
       * val newEntry = new Entry(oImplStm = List(aStm), uImplStm = List(aStm))
       * val tempMap = (List(aLabel) zip List(newEntry)).toMap
       * new SetADInfo(tempMap ++ theMap)
       * }
       * }
       *
       * //def updExplQnt(aLabel: Label, aQnt: BitQuantity)
       *
       * def updImplQnt(aLabel: Label, aQnt: BitQuantity) = {
       * if (theMap contains aLabel) {
       * val value = theMap(aLabel)
       * theMap.updated(aLabel, value.updateImplQuant(aQnt))
       * }
       * else {
       * val newEntry = new Entry(implQuant = aQnt)
       * val tempMap = (List(aLabel) zip List(newEntry)).toMap
       * new SetADInfo(tempMap ++ theMap)
       * }
       * }
       *
       * def returnExplStms(aLabel: Label): (List[EStatement], List[EStatement]) = (theMap(aLabel).oExpStm, theMap(aLabel).uExpStm)
       * def returnImplStms(aLabel: Label): (List[EStatement], List[EStatement]) = (theMap(aLabel).oImplStm, theMap(aLabel).uImplStm)
       * def returnExplQnt(aLabel: Label): BitQuantity = theMap(aLabel).explQuant
       * def returnImplQnt(aLabel: Label): BitQuantity = theMap(aLabel).implQuant
       */

      /**
       * override def toString() = {
       * def print_stmts(l: List[EStatement]) = utils.pretty_print.xhcat(",")(l map { _.toString() })
       * "<(%s), %s, %s, %s>" format (label.toString(), print_stmts(oExpStm), print_stmts(oImplStm), implQuant.toString())
       * }
       */

    }

    object Factory extends ADInfoFactory {
      def newInfo(labels: List[Label]): ADInfo = {
        new SetADInfo(labels)
      }
      def fromLabelAnnot(ann: LabelAnnot): ADInfo = {
        new SetADInfo(Label.newLabel(ann))
      }
    }
  }

  type CADInfo = CADInfo.SetADInfo
}
