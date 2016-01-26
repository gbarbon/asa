package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.datatype.ABSValue._
import it.unive.dais.yaasa.datatype.ADType.UpdateType.UpdateType
import it.unive.dais.yaasa.datatype.FortyTwo._
import it.unive.dais.yaasa.datatype.LMH._
import it.unive.dais.yaasa.exception.AbsValuesMismatch
import it.unive.dais.yaasa.utils.pretty_print._
import it.unive.dais.yaasa.datatype.ADType._
import it.unive.dais.yaasa.utils.collection.map._
import it.unive.dais.yaasa.utils.prelude._

/**
 * @author esteffin
 * @author gbarbon
 */
object CADInfo {

  object CADInfoImpl {

    // Degradation element definition
    private case class DegrElement(
        aFunAnnot: FunAnnot,
        position: Uid) {
      def pretty = "(%s, %s)" format (aFunAnnot.name, position.toString)
    }

    // Flow Element definition
    private case class FlowElement(
        aFunAnnot: FunAnnot,
        aLabel: Label) {
      def pretty = "(%s, %s)" format (aFunAnnot.name, aLabel.name)
    }

    /**
     * An entry of the ADExp map
      *
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
        oExplDegr: Map[DegrElement, (AbstractValue, Iterations)] = Map.empty,
        uExplDegr: Map[DegrElement, (AbstractValue, Iterations)] = Map.empty,
        oImplDegr: Map[DegrElement, (AbstractValue, Iterations)] = Map.empty,
        uImplDegr: Map[DegrElement, (AbstractValue, Iterations)] = Map.empty,
        size: BitQuantity = BitQuantity()) {

      // "add" methods for statements lists
      def addOExpStm(stm: FlowElement) = this.copy(oExpStm = oExpStm + stm)
      def addUExpStm(stm: FlowElement) = this.copy(uExpStm = uExpStm + stm)

      def addOExplDegr(stm: DegrElement, theVal: AbstractValue) = {
        if (oExplDegr contains stm) {
          val prev_el: (AbstractValue, Iterations) = oExplDegr(stm)
          // @FIXME: type mismatch, the second element should match the Wrapper!!!
          /**(theVal, prev_el._1) match {
            * case a: (AbsBoolean[_,_,_] , AbsBoolean[_,_,_] ) =>
            * this.copy(oExplDegr = oExplDegr updated (stm , (a._1.join(a._2), prev_el._2.join(Iterations.oneIter))))
            * case a: (AbsNum[_,_,_], AbsNum[_,_,_]) =>
            * this.copy(oExplDegr = oExplDegr updated (stm , (a._1.join(a._2), prev_el._2.join(Iterations.oneIter))))
            * case a: (AbsString[_,_,_], AbsString[_,_,_]) =>
            * this.copy(oExplDegr = oExplDegr updated (stm , (a._1.join(a._2), prev_el._2.join(Iterations.oneIter))))
            * case a: (_, _) => throw new AbsValuesMismatch("Abstract values are not compatible")
            * }*/
          this.copy(oExplDegr = oExplDegr) // temporary statement
        }
        else
          this.copy(uExplDegr = oExplDegr + (stm -> (theVal, Iterations.oneIter)))
      }
      def addUExplDegr(stm: DegrElement, theVal: AbstractValue) = {
        if (uExplDegr contains stm) {
          val prev_el: (AbstractValue, Iterations) = uExplDegr(stm)
          // @FIXME: type mismatch, the second element should match the Wrapper!!!
          /**(theVal, prev_el._1) match {
            * case a: (AbsBoolean[_,_,_], AbsBoolean[_,_,_]) =>
            * this.copy(uExplDegr = uExplDegr updated (stm , (a._1.join(a._2), prev_el._2.join(Iterations.oneIter))))
            * case a: (AbsNum[_,_,_], AbsNum[_,_,_]) =>
            * this.copy(uExplDegr = uExplDegr updated (stm , (a._1.join(a._2), prev_el._2.join(Iterations.oneIter))))
            * case a: (AbsString[_,_,_], AbsString[_,_,_]) =>
            * this.copy(uExplDegr = uExplDegr updated (stm , (a._1.join(a._2), prev_el._2.join(Iterations.oneIter))))
            * case a: (_, _) => throw new AbsValuesMismatch("Abstract values are not compatible")
            * }*/
          this.copy(uExplDegr = uExplDegr) // temporary statement
        }
        else
          this.copy(uExplDegr = uExplDegr + (stm -> (theVal, Iterations.oneIter)))
      }

      def addExpStm(stm: FlowElement) = this.copy(oExpStm = oExpStm + stm, uExpStm = uExpStm + stm)
      def addExplDegr(stm: DegrElement, theVal: AbstractValue) = this.addOExplDegr(stm, theVal).addUExplDegr(stm, theVal)

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
          size join other.size)
      }

      // used when new label is created
      def createSize(ann: LabelAnnot) = this.copy(size = ann.dimension)

      def pretty: String = {
        "E:[%s:%s] I:[%s:%s] ED:[%s:%s] ID:[%s.%s] size:[%s]".
          format(
            prettySet(oExpStm map { _.toString() }),
            prettySet(uExpStm map { _.toString() }),
            prettySet(oImplStm map { _.toString() }),
            prettySet(uImplStm map { _.toString() }),
            prettySet((oExplDegr map { case (k,v) => (k.toString(),v.toString())} ).toSet),
            prettySet((uExplDegr map { case (k,v) => (k.toString(),v.toString())} ).toSet),
            prettySet((oImplDegr map { case (k,v) => (k.toString(),v.toString())} ).toSet),
            prettySet((uImplDegr map { case (k,v) => (k.toString(),v.toString())} ).toSet),
            size.toString())
      }
    }

    private object Entry {
      def empty = Entry()
    }

    // theMap: a map Label -> Entry
    class SetADInfo private (private val theMap: Map[Label, Entry] = Map()) extends ADInfo[FunAnnot, Uid, AbstractValue] with pretty {

      private[CADInfo] def this() = this(Map.empty[Label, Entry])
      private[CADInfo] def this(labels: List[Label]) =
        this((for (label <- labels) yield (label, Entry.empty)).toMap)

      def update(updateType: UpdateType, ann: FunAnnot, pos: Uid, aVal: AbstractValue): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => {
              // @FIXME: cast abstracValue to abstractDegradationValue still missing
              // @FIXME: only update both over and under approximation!
              updateType match {
                case UpdateType.All => {
                  val updatedEntry = entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), aVal)
                  acc updated (key, updatedEntry)
                }
                case UpdateType.OverApp => ???
                case UpdateType.UnderApp => ???
                case _ => throw ???
              }
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
      def update(updateType: UpdateType, ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        var newMap = Map[Label, Entry]()
        val otherADInfo = anADExp match {
          case x: SetADInfo => x
          case _            => throw new ClassCastException
        }
        theMap.foreach {
          case (key, entry) => {
            otherADInfo.getLabels.foreach(lab => {
              // @FIXME: check Vals._2 if correct
              // @FIXME: cast abstracValue to abstractDegradationValue still missing
              // @FIXME: only update both over and under approximation!
              updateType match {
                case UpdateType.All => {
                  val updatedEntry = entry.addExpStm(FlowElement(ann, lab)).addExplDegr(DegrElement(ann, pos), Vals._1)
                  newMap = newMap updated (key, updatedEntry)
                }
                case UpdateType.OverApp => ???
                case UpdateType.UnderApp => ???
                case _ => throw ???
              }
              newMap = newMap
            })
          }
        }
        otherADInfo.getLabels.foreach {
          lab =>
            {
              val entry = Entry(otherADInfo.getExplFlow(lab)._1, otherADInfo.getExplFlow(lab)._2, otherADInfo.getImplFlow(lab)._1, otherADInfo.getImplFlow(lab)._2, otherADInfo.getExplDegr(lab)._1, otherADInfo.getExplDegr(lab)._2, otherADInfo.getImplDegr(lab)._1, otherADInfo.getImplDegr(lab)._2)
              theMap.foreach {
                case (key, _) => {
                  // @FIXME: check Vals._2 if correct
                  // @FIXME: cast abstracValue to abstractDegradationValue still missing
                  // @FIXME: only update both over and under approximation!
                  updateType match {
                    case UpdateType.All => {
                      val updatedEntry = entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), Vals._2)
                      newMap = newMap updated (key, updatedEntry)
                    }
                    case UpdateType.OverApp => ???
                    case UpdateType.UnderApp => ???
                    case _ => throw ???
                  }
                  newMap = newMap
                }
              }
            }
        }
        new SetADInfo(newMap)
      }

      def newSize(ann: LabelAnnot) = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => acc updated (key, entry.createSize(ann))
          }
        new SetADInfo(newMap)
      }

      def asImplicit: ADInfo[FunAnnot, Uid, AbstractValue] = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => {
              val newEntry = Entry(oImplStm = entry.oExpStm ++ entry.oImplStm, uImplStm = entry.uExpStm ++ entry.uImplStm, oImplDegr = entry.oExplDegr ++ entry.oImplDegr, uImplDegr = entry.uExplDegr ++ entry.uImplDegr)
              acc updated (key, newEntry)
            }
          }
        new SetADInfo(newMap)
      }

      def join(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val m = join_map[Label, Entry]({ case (l, r) => l join r }, theMap, anADInfo.asInstanceOf[SetADInfo].theMap)
        new SetADInfo(m)
      }

      def widening(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = ???

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

      private def getExplDegr(lab: Label): (Map[DegrElement, (AbstractValue, Iterations)], Map[DegrElement, (AbstractValue, Iterations)]) =
        if (theMap contains lab)
          (theMap(lab).oExplDegr, theMap(lab).uExplDegr)
        else
          (Map[DegrElement, (AbstractValue, Iterations)](), Map[DegrElement, (AbstractValue, Iterations)]())

      private def getImplDegr(lab: Label): (Map[DegrElement, (AbstractValue, Iterations)], Map[DegrElement, (AbstractValue, Iterations)]) =
        if (theMap contains lab)
          (theMap(lab).oImplDegr, theMap(lab).uImplDegr)
        else
          (Map[DegrElement, (AbstractValue, Iterations)](), Map[DegrElement, (AbstractValue, Iterations)]())

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

      def pretty: String = {
        val rows = for ((k, v) <- theMap) yield ("%s: %s" format (k.name, v.pretty))
        vcat(rows)
      }
    }

    object Factory extends ADInfoFactory[FunAnnot, Uid, AbstractValue, Label, LabelAnnot] {
      def newInfo(labels: List[Label]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        new SetADInfo(labels)
      }
      def fromLabelAnnot(ann: LabelAnnot): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val res = new SetADInfo(Label.newLabel(ann))
        res.newSize(ann)
      }
      val star = newInfo(List(Label.star)) //empty adexp, it contains only a star label
      val empty = newInfo(List()) //empty adexp, it contains only a star label
    }
  }

  type CADInfo = ADInfo[FunAnnot, Uid, AbstractValue]
  val CADInfoFactory = CADInfoImpl.Factory
}
