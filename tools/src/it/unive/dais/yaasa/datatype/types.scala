package it.unive.dais.yaasa.datatype

import type_definitions._
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
        oExplDegr: Set[DegrElement] = Set.empty,
        uExplDegr: Set[DegrElement] = Set.empty,
        oImplDegr: Set[DegrElement] = Set.empty,
        uImplDegr: Set[DegrElement] = Set.empty,
        //explQuant: BitQuantity = BitQuantity(),
        //implQuant: BitQuantity = BitQuantity())
        size: BitQuantity = BitQuantity()) {

      // "add" methods for statements lists
      def addOExpStm(stm: FlowElement) = this.copy(oExpStm = oExpStm + stm)
      def addUExpStm(stm: FlowElement) = this.copy(uExpStm = uExpStm + stm)
      //def addOImplStm(stm: FlowElement) = this.copy(oImplStm = oImplStm + stm)
      //def addUImpltm(stm: FlowElement) = this.copy(uImplStm = uImplStm + stm)

      def addOExplDegr(stm: DegrElement) = this.copy(oExplDegr = oExplDegr + stm)
      def addUExplDegr(stm: DegrElement) = this.copy(uExplDegr = uExplDegr + stm)
      // def addOImplDegr(stm: DegrElement) = this.copy(oImplDegr = oImplDegr + stm)
      // def addUImplDegr(stm: DegrElement) = this.copy(uImplDegr = uImplDegr + stm)

      def addExpStm(stm: FlowElement) = this.copy(oExpStm = oExpStm + stm, uExpStm = uExpStm + stm)
      // def addImplStm(stm: FlowElement) = this.copy(oImplStm = oImplStm + stm, uImplStm = uImplStm + stm)
      def addExpDegr(stm: DegrElement) = this.copy(oExplDegr = oExplDegr + stm, uExplDegr = uExplDegr + stm)
      // def addImplDegr(stm: DegrElement) = this.copy(oImplDegr = oImplDegr + stm, uImplDegr = uImplDegr + stm)

      // @TODO: remove the following two functions, the size is never modified
      //def updateImplQuant(qnt: BitQuantity) = this.copy(implQuant = implQuant.update(qnt))
      //def updateExplQuant(qnt: BitQuantity) = this.copy(explQuant = explQuant.update(qnt))
      def join(other: Entry): Entry = {
        Entry(
          oExpStm ++ other.oExpStm,
          uExpStm ++ other.uExpStm,
          oImplStm ++ other.oImplStm,
          uImplStm ++ other.uImplStm,
          oExplDegr ++ other.oExplDegr,
          uExplDegr ++ other.uExplDegr,
          oImplDegr ++ other.oImplDegr,
          uImplDegr ++ other.uImplDegr,
          //explQuant join other.explQuant,
          //implQuant join other.implQuant
          size join other.size)
      }

      //@FIXME: quantities commented
      // Temporary conversion of FunAnnot to BitQuantity operations
      //@TODO: find a better way to implement this!
      /*
      def newExplQuant(ann: FunAnnot) = {
        val res = ann.name match {
          case "BOPlusPlus" => BitQuantity.BOPlusPlus(explQuant)
          case "BOPlus"     => BitQuantity.BOPlus(explQuant)
          case "BOMinus"    => BitQuantity.BOMinus(explQuant)
          case "BOMul"      => BitQuantity.BOMul(explQuant)
          case "BODiv"      => BitQuantity.BODiv(explQuant)
          case "BOAnd"      => BitQuantity.BOAnd
          case "BOOr"       => BitQuantity.BOOr
          case "BOMod"      => BitQuantity.BOMod(explQuant)
          case "BOLt"       => BitQuantity.BOLt
          case "BOLeq"      => BitQuantity.BOLeq(explQuant)
          case "BOEq"       => BitQuantity.BOEq(explQuant)
          case "BOGt"       => BitQuantity.BOGt
          case "BOGeq"      => BitQuantity.BOGeq(explQuant)
          case "BONeq"      => BitQuantity.BONeq(explQuant)
          case "UNot"       => BitQuantity.UNot
          case "UNeg"       => BitQuantity.UNeg(explQuant)
          case _            => returnQuant(ann.name, explQuant) //All functions from stdlib
        }
        this.copy(explQuant = res)
      }*/

      // used when new label is created
      //def newExplQuant(ann: LabelAnnot) = this.copy(explQuant = ann.dimension)
      def createSize(ann: LabelAnnot) = this.copy(size = ann.dimension)

      def pretty: String = {
        "E:[%s:%s] I:[%s:%s] Q:%s:%s".
          format(
            prettySet(oExpStm map { _.toString() }),
            prettySet(uExpStm map { _.toString() }),
            prettySet(oImplStm map { _.toString() }),
            prettySet(uImplStm map { _.toString() }),
            prettySet(oExplDegr map { _.toString() }),
            prettySet(uExplDegr map { _.toString() }),
            prettySet(oImplDegr map { _.toString() }),
            prettySet(uImplDegr map { _.toString() }),
            size.toString())
        /*explQuant.toString(),
            implQuant.toString())*/
        /*oExpStm.foldLeft(res) { (res, x) => res + x.toString() }
        res = res + "}:{"
        uExpStm.foreach { x => res = res + x.toString() }
        res = res + "} I:{"
        oImplStm.foreach { x => res = res + x.toString() }
        res = res + "}:{"
        uImplStm.foreach { x => res = res + x.toString() }
        res + "} Q:" + explQuant.toString() + ":" + implQuant.toString()*/
      }
    }

    private object Entry {
      def empty = Entry()
    }

    // theMap: a map Label -> Entry
    class SetADInfo private (private val theMap: Map[Label, Entry] = Map()) extends ADInfo {

      private[CADInfo] def this() = this(Map.empty[Label, Entry])

      private[CADInfo] def this(labels: List[Label]) =
        this((for (label <- labels) yield (label, Entry.empty)).toMap)

      def update(ann: FunAnnot): ADInfo = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => {
              //val updatedEntry = entry.addExpStm(FlowElement(ann, key)).newExplQuant(ann) //@FIXME: remove temporary comments
              val updatedEntry = entry.addExpStm(FlowElement(ann, key).addExplDegr(DegrElement(ann, , , )))
              acc updated (key, updatedEntry)
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
      def update(anADExp: ADInfo, ann: FunAnnot): ADInfo = {
        var newMap = Map[Label, Entry]()
        val otherADInfo = anADExp match {
          case x: SetADInfo => x
          case _            => throw new ClassCastException
        }
        theMap.foreach {
          case (key, entry) => {
            otherADInfo.getLabels.foreach(lab => {
              //val updatedEntry = entry.addExpStm(FlowElement(ann, lab)).newExplQuant(ann) //@FIXME: remove temporary comments
              val updatedEntry = entry.addExpStm(FlowElement(ann, lab).addExplDegr(ann, , , ))
              newMap = newMap updated (key, updatedEntry)
            })
          }
        }
        otherADInfo.getLabels.foreach {
          lab =>
            {
              // val entry = Entry(otherADInfo.getExplFlow(lab)._1, otherADInfo.getExplFlow(lab)._2, otherADInfo.getImplFlow(lab)._1, otherADInfo.getImplFlow(lab)._2, otherADInfo.getExplQuant(lab), otherADInfo.getImplQuant(lab))
              val entry = Entry(otherADInfo.getExplFlow(lab)._1, otherADInfo.getExplFlow(lab)._2, otherADInfo.getImplFlow(lab)._1, otherADInfo.getImplFlow(lab)._2)
              theMap.foreach {
                case (key, _) => {
                  // val updatedEntry = entry.addExpStm(FlowElement(ann, key)).newExplQuant(ann)  //@FIXME: remove temporary comments
                  val updatedEntry = entry.addExpStm(FlowElement(ann, key).addExplDegr(DegrElement(ann, , , )))
                  newMap = newMap updated (lab, updatedEntry)
                }
              }
            }
        }
        new SetADInfo(newMap)
      }
      //@TODO: still incomplete function!
      def update(ADExps: List[ADInfo], ann: FunAnnot): ADInfo = {
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
        Factory.newInfo(Label.star) //@FIXME: temporary WRONG solution
      }

      // @TODO: old quantity function, to remove
      //needed to create explicit quantity when a Label is read!
      /*def newExplQuant(ann: LabelAnnot) = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => acc updated (key, entry.newExplQuant(ann))
          }
        new SetADInfo(newMap)
      }*/
      def newSize(ann: LabelAnnot) = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => acc updated (key, entry.createSize(ann))
          }
        new SetADInfo(newMap)
      }


      def asImplicit: ADInfo = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => {
              // val newEntry = Entry(oImplStm = entry.oExpStm ++ entry.oImplStm, uImplStm = entry.uExpStm ++ entry.uImplStm, implQuant = (entry.explQuant join entry.implQuant))
              val newEntry = Entry(oImplStm = entry.oExpStm ++ entry.oImplStm, uImplStm = entry.uExpStm ++ entry.uImplStm, oImplDegr = entry.oExplDegr ++ entry.oImplDegr, uImplDegr = entry.uExplDegr ++ entry.uImplDegr)
              acc updated (key, newEntry)
            }
          }
        new SetADInfo(newMap)
      }

      def join(anADInfo: ADInfo): ADInfo = {
        val m = join_map[Label, Entry]({ case (l, r) => l join r }, theMap, anADInfo.asInstanceOf[SetADInfo].theMap)
        new SetADInfo(m)
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

      private def getExplDegr(lab: Label): (Set[DegrElement], Set[DegrElement]) =
        if (theMap contains lab)
          (theMap(lab).oExplDegr, theMap(lab).uExplDegr)
        else
          (Set[DegrElement](), Set[DegrElement]())

      private def getImplDegr(lab: Label): (Set[DegrElement], Set[DegrElement]) =
        if (theMap contains lab)
          (theMap(lab).oImplDegr, theMap(lab).uImplDegr)
        else
          (Set[DegrElement](), Set[DegrElement]())

      /*private def getExplQuant(lab: Label): BitQuantity =
        if (theMap contains lab)
          theMap(lab).explQuant
        else
          BitQuantity()

      private def getImplQuant(lab: Label): BitQuantity =
        if (theMap contains lab)
          theMap(lab).implQuant
        else
          BitQuantity()*/

      private def getSize(lab: Label): BitQuantity =
        if (theMap contains lab)
          theMap(lab).size
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
        val rows = for ((k, v) <- theMap) yield ("%s: %s" format (k.name, v.pretty))
        vcat(rows)
      }
    }

    object Factory extends ADInfoFactory {
      def newInfo(labels: List[Label]): ADInfo = {
        new SetADInfo(labels)
      }
      def fromLabelAnnot(ann: LabelAnnot): ADInfo = {
        val res = new SetADInfo(Label.newLabel(ann))
        // res.newExplQuant(ann)  //@TODO: to remove
        res.newSize(ann)
      }
    }
  }

  type CADInfo = CADInfo.SetADInfo
}
