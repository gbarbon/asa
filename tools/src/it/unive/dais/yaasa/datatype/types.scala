package it.unive.dais.yaasa.datatype

import type_definitions._
import it.unive.dais.yaasa.utils._
import it.unive.dais.yaasa.utils.prelude._
import ADType._
import it.unive.dais.yaasa.utils.list._

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
   */

  object CADInfo {

    /**
     * An entry of the ADExp map
     * @constructor create a new atomic data expression of a certain label.
     * @param oExpStm Over approximation of the statements applied to the label (explicit flow, not used at this time)
     * @param uExpStm Under approximation of the statements applied to the label (explicit flow, not used at this time)
     * @param oImplStm Over approximation of the statements applied to the label (implicit flow)
     * @param uImplStm Under approximation of the statements applied to the label (implicit flow)
     */
    private case class Entry(
        oExpStm: Set[FlowElement] = Set.empty,
        uExpStm: Set[FlowElement] = Set.empty,
        oImplStm: Set[FlowElement] = Set.empty,
        uImplStm: Set[FlowElement] = Set.empty,
        explQuant: BitQuantity = BitQuantity(),
        implQuant: BitQuantity = BitQuantity()) {

      // "add" methods for statements lists
      def addOExpStm(stm: FlowElement) = this.copy(oExpStm = oExpStm + stm)
      def addUExpStm(stm: FlowElement) = this.copy(uExpStm = uExpStm + stm)
      def addOImplStm(stm: FlowElement) = this.copy(oImplStm = oImplStm + stm)
      def addUImpltm(stm: FlowElement) = this.copy(uImplStm = uImplStm + stm)
      def addExpStm(stm: FlowElement) = this.copy(oExpStm = oExpStm + stm, uExpStm = uExpStm + stm)
      def addImplStm(stm: FlowElement) = this.copy(oImplStm = oImplStm + stm, uImplStm = uImplStm + stm)
      def updateImplQuant(qnt: BitQuantity) = this.copy(implQuant = implQuant.update(qnt))
      def updateExplQuant(qnt: BitQuantity) = this.copy(explQuant = explQuant.update(qnt))
      def join(other: Entry): Entry = Entry.empty //@FIXME: Implement here

      def pretty: String = {
        var res = "E:{"
        oExpStm.foreach { x => res = res + x.toString() }
        res = res + "}:{"
        uExpStm.foreach { x => res = res + x.toString() }
        res = res + "} I:{"
        oImplStm.foreach { x => res = res + x.toString() }
        res = res + "}:{"
        uImplStm.foreach { x => res = res + x.toString() }
        res + "} Q:" + explQuant.toString() + ":" + implQuant.toString()
      }
    }

    private object Entry {
      def empty = Entry()
    }

    // theMap: a map Label -> Entry
    class SetADInfo private (private val theMap: Map[Label, Entry] = Map()) extends ADInfo {

      sealed trait UPDOP

      case class ExplUpd() extends UPDOP { override def toString() = "updateExpl" }
      case class ImplUpd() extends UPDOP { override def toString() = "updateImpl" }
      case class EQuantUpd() extends UPDOP { override def toString() = "updateExplQuant" }
      case class IQuantUpd() extends UPDOP { override def toString() = "updateImplQuant" }

      private[CADInfo] def this() = this(Map.empty[Label, Entry])

      private[CADInfo] def this(labels: List[Label]) =
        this((for (label <- labels) yield (label, Entry.empty)).toMap)

      def specUpdate(ann: FunAnnot, op: UPDOP) = {
        var newMap = Map[Label, Entry]()
        theMap.foreach {
          case (key, entry) => {
            val updatedEntry = op match {
              case x: ExplUpd => entry.addExpStm(FlowElement(ann, key))
              case x: ImplUpd => entry.addImplStm(FlowElement(ann, key))
              case _          => throw new Unexpected("Unexpected update operation type" + ann.toString())
            }
            newMap = newMap updated (key, updatedEntry)
          }
        }
        new SetADInfo(newMap)
      }

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
      def specUpdate(anADExp: ADInfo, ann: FunAnnot, op: UPDOP): ADInfo = {
        var newMap = Map[Label, Entry]()
        val otherADInfo = anADExp match {
          case x: SetADInfo => x
          case _            => throw new ClassCastException
        }
        theMap.foreach {
          case (key, entry) => {
            otherADInfo.getLabels.foreach(lab => {
              val updatedEntry = op match {
                case x: ExplUpd => entry.addExpStm(FlowElement(ann, lab))
                case x: ImplUpd => entry.addImplStm(FlowElement(ann, lab))
                case _          => throw new Unexpected("Unexpected update operation type" + ann.toString())
              }
              newMap = newMap updated (key, updatedEntry)
            })
          }
        }
        otherADInfo.getLabels.foreach {
          lab =>
            {
              val entry = Entry(otherADInfo.getExplFlow(lab)._1, otherADInfo.getExplFlow(lab)._2, otherADInfo.getImplFlow(lab)._1, otherADInfo.getImplFlow(lab)._2, otherADInfo.getExplQuant(lab), otherADInfo.getImplQuant(lab))
              theMap.foreach {
                case (key, _) => {
                  val updatedEntry = op match {
                    case x: ExplUpd => entry.addExpStm(FlowElement(ann, key))
                    case x: ImplUpd => entry.addImplStm(FlowElement(ann, key))
                    case _          => throw new Unexpected("Unexpected update operation type" + ann.toString())
                  }
                  newMap = newMap updated (lab, updatedEntry)
                }
              }
            }
        }
        new SetADInfo(newMap)
      }
      //@TODO:
      def specUpdate(ADExps: List[ADInfo], ann: FunAnnot, op: UPDOP): ADInfo = {
        val args = ADExps.cast[SetADInfo]
        val adexps = (this :: ADExps) map {
          case s: SetADInfo => s
          case _            => throw new Unexpected("Wrong type implementation")
        }
        val keys = adexps.foldLeft(Set.empty[Label])((s, l) => s ++ l.theMap.keys)
        val joined =
          for (label <- keys)
            yield (label -> ((adexps map { _.getRowSafe(label) }).foldLeft(Entry.empty) { (acc, entry) => acc join entry }))
        //@FIXME: qua devi aggiungere ad ogni elemento di joined la combinazione con ... Probabilmente BROKEN...
        Factory.newInfo(Label.star) //@FIXME: temporary solution
      }

      def specQUpdate(qnt: BitQuantity, op: UPDOP): ADInfo = {
        var newMap = Map[Label, Entry]()
        theMap.foreach {
          case (key, entry) => {
            val updatedEntry = op match {
              case o: EQuantUpd => entry.updateExplQuant(qnt)
              case o: IQuantUpd => entry.updateImplQuant(qnt)
              case _            => throw new Unexpected("Unexpected update operation type: " + qnt.toString())
            }
            newMap = newMap updated (key, updatedEntry)
          }
        }
        new SetADInfo(newMap)
      }

      //def specQUpdate(qnt: BitQuantity, ADExps: List[ADInfo], op: UPDOP): ADInfo = {}

      def update(ann: FunAnnot): ADInfo = specUpdate(ann, ExplUpd())
      def update(anADExp: ADInfo, ann: FunAnnot): ADInfo = specUpdate(anADExp, ann, ExplUpd())
      def update(ADExps: List[ADInfo], ann: FunAnnot): ADInfo = specUpdate(ADExps, ann, ExplUpd())
      //def updateImpl(ann: FunAnnot): ADInfo = specUpdate(ann, ImplUpd())
      //def updateImpl(anADExp: ADInfo, ann: FunAnnot): ADInfo = specUpdate(anADExp, ann, ImplUpd())
      def updateIQnt(qnt: BitQuantity): ADInfo = specQUpdate(qnt, IQuantUpd())
      //def updateIQnt(qnt: BitQuantity, ADExps: List[ADInfo]): ADInfo = Factory.newInfo(Label.star) //@FIXME: temporary solution

      def updateImpl(implInfo: Option[ADInfo]): ADInfo = {
        var newMap = Map[Label, Entry]()
        //@TODO: the join between the "this" explicit adexp and the implicit adexp
        val temp = new SetADInfo(newMap)
        this
      }

      def asImplicit: ADInfo = {
        var newMap = Map[Label, Entry]()
        theMap.foreach {
          case (key, entry) => {
            val newEntry = Entry(oImplStm = entry.oExpStm ++ entry.oImplStm, uImplStm = entry.uExpStm ++ entry.uImplStm, implQuant = BitQuantity.join(entry.explQuant, entry.implQuant))
            newMap = newMap updated (key, newEntry)
          }
        }
        new SetADInfo(newMap)
      }

      //@TODO: implement me!!!
      def join(anADInfo: ADInfo): ADInfo = {
        var newMap = Map[Label, Entry]()

        //new SetADInfo(newMap)
        this
      }

      private def getLabels: List[Label] = theMap.keys.toList

      private def getExplFlow(lab: Label): (Set[FlowElement], Set[FlowElement]) =
        if (theMap contains lab)
          (theMap(lab).oExpStm, theMap(lab).uExpStm)
        else
          (Set[FlowElement](), Set[FlowElement]())

      private def getImplFlow(lab: Label): (Set[FlowElement], Set[FlowElement]) =
        if (theMap contains lab)
          (theMap(lab).oImplStm, theMap(lab).uImplStm)
        else
          (Set[FlowElement](), Set[FlowElement]())

      private def getExplQuant(lab: Label): BitQuantity =
        if (theMap contains lab)
          theMap(lab).explQuant
        else
          BitQuantity()

      private def getImplQuant(lab: Label): BitQuantity =
        if (theMap contains lab)
          theMap(lab).implQuant
        else
          BitQuantity()

      private def getRowSafe(lab: Label) =
        if (theMap contains lab)
          theMap(lab)
        else
          Entry.empty

      /**
       * override def toString() = {
       * def print_stmts(l: List[EStatement]) = utils.pretty_print.xhcat(",")(l map { _.toString() })
       * "<(%s), %s, %s, %s>" format (label.toString(), print_stmts(oExpStm), print_stmts(oImplStm), implQuant.toString())
       * }
       */
      def pretty: String = {
        var res: String = ""
        theMap.foreach { case (key, entry) => res = res + key.name + " : " + entry.pretty + "\n" }
        res
      }
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
