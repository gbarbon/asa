package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.datatype.ABSValue._
import it.unive.dais.yaasa.datatype.FortyTwo._

import scala.util.Success

//import it.unive.dais.yaasa.datatype.LMH._
import it.unive.dais.yaasa.exception._
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
        position: Uid) extends pretty {
      def pretty = "(%s, %s)" format (aFunAnnot.name, position.toString)
    }

    // Flow Element definition
    private case class FlowElement (
        aFunAnnot: FunAnnot,
        aLabel: Label) extends pretty {
      override def pretty = "(%s, %s)" format (aFunAnnot.name, aLabel.name)
    }

    private case class DegrAttrib (
        abstrVal: AbstractValue,
        iters: Iterations) extends pretty {
      override def pretty = "(%s, %s)" format (abstrVal.toString() ,iters.toString())

      def join(r: DegrAttrib): DegrAttrib = DegrAttrib(this.abstrVal join r.abstrVal, this.iters join r.iters)
      def meet(r: DegrAttrib): DegrAttrib = DegrAttrib(this.abstrVal meet r.abstrVal, this.iters meet r.iters)
      def widening(r: DegrAttrib): DegrAttrib = {
        println("DEBUG: Widening between two abstract values "+this.abstrVal +" and "+r.abstrVal)
        println("DEBUG: Widening between two iterations "+this.iters +" and "+r.iters)
        DegrAttrib(this.abstrVal widening r.abstrVal, this.iters widening r.iters)}
    }

    /**
     * An entry of the ADExp map
      *
      * @constructor create a new atomic data expression of a certain label.
     * @param oExplStm Over approximation of the statements applied to the label (explicit flow, not used at this time)
     * @param uExplStm Under approximation of the statements applied to the label (explicit flow, not used at this time)
     * @param oImplStm Over approximation of the statements applied to the label (implicit flow)
     * @param uImplStm Under approximation of the statements applied to the label (implicit flow)
     */
    private case class Entry(
        oExplStm: Set[FlowElement] = Set.empty,
        uExplStm: Set[FlowElement] = Set.empty,
        oImplStm: Set[FlowElement] = Set.empty,
        uImplStm: Set[FlowElement] = Set.empty,
        oExplDegr: Map[DegrElement, DegrAttrib] = Map.empty,
        uExplDegr: Map[DegrElement, DegrAttrib] = Map.empty,
        oImplDegr: Map[DegrElement, DegrAttrib] = Map.empty,
        uImplDegr: Map[DegrElement, DegrAttrib] = Map.empty,
        size: BitQuantity = BitQuantity.empty) extends pretty {

      // "add" methods for statements lists
      // @TODO: now over and under add methods should be supported by union method, check and remove
      def addOExpStm(stm: FlowElement) = this.copy(oExplStm = oExplStm + stm)
      def addUExpStm(stm: FlowElement) = this.copy(uExplStm = uExplStm + stm)
      def addOExplDegr(stm: DegrElement, theVal: AbstractValue) = {
        if (oExplDegr contains stm) {
          val prev_el: DegrAttrib = oExplDegr(stm)
          println("DEBUG: Over iters for stm "+ stm +" is " + prev_el.iters.incr)
          this.copy(oExplDegr = oExplDegr updated(stm, DegrAttrib(theVal join prev_el.abstrVal, prev_el.iters.incr)))

        }
        else {
          println("DEBUG: Over iters for stm " + stm + "not found, creating new")
          this.copy(oExplDegr = oExplDegr + (stm -> DegrAttrib(theVal, Iterations.oneIter)))
        }
      }
      def addUExplDegr(stm: DegrElement, theVal: AbstractValue) = {
        if (uExplDegr contains stm) {
          val prev_el: DegrAttrib = uExplDegr(stm)
          //println("DEBUG: Under iters for stm "+ stm +" is " + prev_el.iters.incr)
          this.copy(uExplDegr = uExplDegr updated(stm, DegrAttrib(theVal join prev_el.abstrVal, prev_el.iters.incr)))
        }
        else {
          //println("DEBUG: Under iters for stm " + stm + "not found, creating new")
          this.copy(uExplDegr = uExplDegr + (stm -> DegrAttrib(theVal, Iterations.oneIter)))
        }
      }
      def addExpStm(stm: FlowElement) = this.copy(oExplStm = oExplStm + stm, uExplStm = uExplStm + stm)
      def addExplDegr(stm: DegrElement, theVal: AbstractValue) = this.addOExplDegr(stm, theVal).addUExplDegr(stm, theVal)

      def join(other: Entry): Entry = {
        Entry(
          oExplStm ++ other.oExplStm,
          uExplStm ++ other.uExplStm,
          oImplStm ++ other.oImplStm,
          uImplStm ++ other.uImplStm,
          join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, oExplDegr , other.oExplDegr),
          join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, uExplDegr , other.uExplDegr),
          join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, oImplDegr , other.oImplDegr),
          join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, uImplDegr , other.uImplDegr),
          size join other.size)
      }

      def meet(other: Entry): Entry = {
        Entry(oExplStm intersect other.oExplStm,
          uExplStm intersect other.uExplStm,
          oImplStm intersect other.oImplStm,
          uImplStm intersect other.uImplStm,
          meet_map[DegrElement, DegrAttrib]({ case (l, r) => l meet r }, oExplDegr , other.oExplDegr),
          meet_map[DegrElement, DegrAttrib]({ case (l, r) => l meet r }, uExplDegr , other.uExplDegr),
          meet_map[DegrElement, DegrAttrib]({ case (l, r) => l meet r }, oImplDegr , other.oImplDegr),
          meet_map[DegrElement, DegrAttrib]({ case (l, r) => l meet r }, uImplDegr , other.uImplDegr),
          size meet other.size
        )
      }

      def union(other: Entry): Entry = {
        var res: Entry = Entry() //@FIXME: not immutable now!! (non sono riuscito!!)
        /**
          * se op appare solo in una delle due mappe,
          * devo tenere quell'operatore con quei valori ma under_pprox a zero e over al valore di quello che c'è già (iterations)

          * nel caso sia presente un operazione fra le due mappe, valore = join fra valori
          * molteplicità min il min dei valori, max il max fra i due valori
        **/
        // for the FlowElements
        // uExpl: if el exist only in one of the two, then add nothing to the uExpl. If it exists in both, add to the uExpl.
        for (stm <- this.uExplStm ++ other.uExplStm) {
          res = (this.uExplStm.contains(stm), other.uExplStm.contains(stm)) match {
            case (false, false) => throw new Unexpected("Cannot exists an element that does not exists in both sets.")
            case (true, true) => res.copy(uExplStm = res.uExplStm + stm)
            case (_, _) => res.copy()
              //res.copy(oExplStm = res.oExplStm + stm)
              // @TODO: check, we add it to the over approx? It should already be present
          }
        }
        // oExpl: if el exists only in one of the two (or in both), then add to the oExpl
        for (stm <- this.oExplStm ++ other.oExplStm) {
          res = (this.oExplStm.contains(stm), other.oExplStm.contains(stm)) match {
            case (false, false) => throw new Unexpected("Cannot exists an element that does not exists in both sets.")
            case (_, _) => res.copy(oExplStm = res.oExplStm + stm)
          }
        }

        // the same for the implicit
        for (stm <- this.uImplStm ++ other.uImplStm) {
          res = (this.uImplStm.contains(stm), other.uImplStm.contains(stm)) match {
            case (false, false) => throw new Unexpected("Cannot exists an element that does not exists in both sets.")
            case (true, true) => res.copy(uImplStm = res.uImplStm + stm)
            case (_, _) => res.copy()
          }
        }
        for (stm <- this.oImplStm ++ other.oImplStm) {
          res = (this.oImplStm.contains(stm), other.oImplStm.contains(stm)) match {
            case (false, false) => throw new Unexpected("Cannot exists an element that does not exists in both sets.")
            case (_, _) => res.copy(oImplStm = res.oImplStm + stm)
          }
        }

        // for the DegradationElements we must also performs the join and the meet
        for (el <- this.uExplDegr.keySet ++ other.uExplDegr.keySet) {
          res = (this.uExplDegr.get(el), other.uExplDegr.get(el)) match {
            case (None, None) => throw new Unexpected("Cannot exists an element that does not exists in both maps.")
            case (Some(l), Some(r)) =>
              res.copy(uExplDegr = uExplDegr updated(el, DegrAttrib(l.abstrVal meet r.abstrVal, l.iters meet r.iters)))
            case (_, _) => res.copy()
          }
        }
        for (el <- this.oExplDegr.keySet ++ other.oExplDegr.keySet) {
          res = (this.oExplDegr.get(el), other.oExplDegr.get(el)) match {
            case (None, None) => throw new Unexpected("Cannot exists an element that does not exists in both maps.")
            case (Some(l), None) => res.copy(oExplDegr = oExplDegr updated(el, l))
            case (None, Some(r)) => res.copy(oExplDegr = oExplDegr updated(el, r))
            case (Some(l), Some(r)) =>
              res.copy(oExplDegr = oExplDegr updated(el, DegrAttrib(l.abstrVal join r.abstrVal, l.iters join r.iters)))
          }
        }
        for (el <- this.uImplDegr.keySet ++ other.uImplDegr.keySet) {
          res = (this.uImplDegr.get(el), other.uImplDegr.get(el)) match {
            case (None, None) => throw new Unexpected("Cannot exists an element that does not exists in both maps.")
            case (Some(l), Some(r)) =>
              res.copy(uImplDegr = uImplDegr updated(el, DegrAttrib(l.abstrVal meet r.abstrVal, l.iters meet r.iters)))
            case (_, _) => res.copy()
          }
        }
        for (el <- this.oImplDegr.keySet ++ other.oImplDegr.keySet) {
          res = (this.oImplDegr.get(el), other.oImplDegr.get(el)) match {
            case (None, None) => throw new Unexpected("Cannot exists an element that does not exists in both maps.")
            case (Some(l), None) => res.copy(oImplDegr = oImplDegr updated(el, l))
            case (None, Some(r)) => res.copy(oImplDegr = oImplDegr updated(el, r))
            case (Some(l), Some(r)) =>
              res.copy(oImplDegr = oImplDegr updated(el, DegrAttrib(l.abstrVal join r.abstrVal, l.iters join r.iters)))
          }
        }
        res.copy(size = this.size join other.size)
      }

      def widening(other: Entry): Entry = {
        Entry(
          //@FIXME: not sure widening is correct this way...
          oExplStm ++ other.oExplStm,
          uExplStm ++ other.uExplStm,
          oImplStm ++ other.oImplStm,
          uImplStm ++ other.uImplStm,
          widening_map[DegrElement, DegrAttrib]({ case (l, r) => l widening r }, oExplDegr , other.oExplDegr/*, DegrAttrib.empty*/),
          widening_map[DegrElement, DegrAttrib]({ case (l, r) => l widening r }, uExplDegr , other.uExplDegr/*, DegrAttrib.empty*/),
          widening_map[DegrElement, DegrAttrib]({ case (l, r) => l widening r }, oImplDegr , other.oImplDegr/*, DegrAttrib.empty*/),
          widening_map[DegrElement, DegrAttrib]({ case (l, r) => l widening r }, uImplDegr , other.uImplDegr/*, DegrAttrib.empty*/),
          size widening other.size)
      }

      // used when new label is created
      def createSize(ann: LabelAnnot) = this.copy(size = ann.dimension)

      def pretty: String = {
        "E:[%s:%s] I:[%s:%s] ED:[%s:%s] ID:[%s.%s] size:[%s]".
          format(
            prettySet(oExplStm map { _.toString }),
            prettySet(uExplStm map { _.toString }),
            prettySet(oImplStm map { _.toString }),
            prettySet(uImplStm map { _.toString }),
            prettySet((oExplDegr map { _.toString }).toSet),
            prettySet((uExplDegr map { _.toString } ).toSet),
            prettySet((oImplDegr map { _.toString } ).toSet),
            prettySet((uImplDegr map { _.toString } ).toSet),
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
        println("DEBUG: *** updt single ADINFO ***")
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) =>
              acc updated (key, entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), aVal))
              // @FIXME: cast abstracValue to abstractDegradationValue still missing

              // @TODO: remove following part when sure
              /**
                * updateType match {
                * case UpdateType.All =>
                * acc updated (key, entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), aVal))
                * case UpdateType.OverApp =>
                * acc updated (key, entry.addOExpStm(FlowElement(ann, key)).addOExplDegr(DegrElement(ann, pos), aVal))
                * case UpdateType.UnderApp =>
                * acc updated (key, entry.addUExpStm(FlowElement(ann, key)).addUExplDegr(DegrElement(ann, pos), aVal))
                * case _ => throw new WrongUpdateClass("Update type is not recognized")
                * }**/
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
        //println("premap: %s" format newMap)
        println("DEBUG: *** upd two ADINFO: 1st ADINFO ***")
        theMap.foreach {
          case (key, entry) =>
            println("DEBUG: updating label " + key)
            otherADInfo.getLabels.foreach(lab => {
              // @FIXME: cast abstracValue to abstractDegradationValue still missing
              println("DEBUG: inserting tuple (" + ann + ", " + lab  + " ")
              newMap = newMap updated (key, entry.addExpStm(FlowElement(ann, lab)).addExplDegr(DegrElement(ann, pos), Vals._1))

              //
              /**updateType match {
                * case UpdateType.All =>
                * println("DEBUG: inserting tuple (" + ann + ", " + lab  + " ")
                * newMap = newMap updated (key, entry.addExpStm(FlowElement(ann, lab)).addExplDegr(DegrElement(ann, pos), Vals._1))
                * case UpdateType.OverApp =>
                * newMap = newMap updated (key, entry.addOExpStm(FlowElement(ann, lab)).addOExplDegr(DegrElement(ann, pos), Vals._1))
                * case UpdateType.UnderApp =>
                * newMap = newMap updated (key, entry.addUExpStm(FlowElement(ann, lab)).addUExplDegr(DegrElement(ann, pos), Vals._1))
                * case _ => throw new WrongUpdateClass("Update type is not recognized")
                * }**/
            })
        }
        //println("midmap: %s" format newMap)
        println("DEBUG: *** upd two ADINFO: 2nd ADINFO ***")
        otherADInfo.getLabels.foreach {
          lab =>
            {
              println("DEBUG: updating label " + lab)
              val entry = otherADInfo.getEntry(lab)
              //val entry = Entry(otherADInfo.getExplFlow(lab)._1, otherADInfo.getExplFlow(lab)._2, otherADInfo.getImplFlow(lab)._1, otherADInfo.getImplFlow(lab)._2, otherADInfo.getExplDegr(lab)._1, otherADInfo.getExplDegr(lab)._2, otherADInfo.getImplDegr(lab)._1, otherADInfo.getImplDegr(lab)._2)
              theMap.foreach {
                case (key, _) =>
                  // @FIXME: cast abstracValue to abstractDegradationValue still missing
                  println("DEBUG: inserting tuple: (" + ann + ", " + key  + ")")
                  val newentry: Entry = entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), Vals._2)

                  //val myentry: (Label, Entry) = (lab, entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), Vals._2))
                    /**updateType match {
                      * case UpdateType.All =>
                      * println("DEBUG: inserting tuple (" + ann + ", " + key  + " ")
                      * (lab, entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), Vals._2))
                      * case UpdateType.OverApp =>
                      * (lab, entry.addOExpStm(FlowElement(ann, key)).addOExplDegr(DegrElement(ann, pos), Vals._2))
                      * case UpdateType.UnderApp =>
                      * (lab, entry.addUExpStm(FlowElement(ann, key)).addUExplDegr(DegrElement(ann, pos), Vals._2))
                      * case _ => throw new WrongUpdateClass("Update type is not recognized")
                      * }**/
                  if (newMap.keys.exists {_ == lab}) {
                    newMap = newMap.updated(lab, newentry join theMap(lab))
                    //newMap = newMap.updated(lab, myentry._2 join theMap(lab))
                  }
                  else
                    newMap = newMap.updated(lab, newentry)
                    //newMap = newMap.updated(myentry._1, myentry._2)
              }
            }
        }
        //println("newmap: %s" format newMap)
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
            case (acc, (key, entry)) =>
              val newEntry = Entry(
                oImplStm = entry.oExplStm ++ entry.oImplStm,
                uImplStm = entry.uExplStm ++ entry.uImplStm,
                oImplDegr = join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, entry.oExplDegr , entry.oImplDegr),
                uImplDegr = join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, entry.uExplDegr , entry.uImplDegr),
                size = entry.size)
              acc updated (key, newEntry)
          }
        new SetADInfo(newMap)
      }

      def join(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val m = join_map[Label, Entry]({ case (l, r) => l join r }, theMap, anADInfo.asInstanceOf[SetADInfo].theMap)
        new SetADInfo(m)
      }

      def meet(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val m = meet_map[Label, Entry]({ case (l, r) => l meet r }, theMap, anADInfo.asInstanceOf[SetADInfo].theMap)
        new SetADInfo(m)
      }

      def union(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val otherMap: Map[Label, Entry] = anADInfo.asInstanceOf[SetADInfo].theMap
        val keys = theMap.keySet ++ otherMap.keySet
        val m =  (for (k <- keys) yield {
            (theMap.get(k), otherMap.get(k)) match {
              case (Some(l), Some(r)) => k -> l.union(r)
            }
          }).toMap
        new SetADInfo(m)
      }

      def widening(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val m = widening_map[Label, Entry]({ case (l, r) => l widening r }, theMap, anADInfo.asInstanceOf[SetADInfo].theMap)
        //@FIXME: not sure widening is correct this way... (see also utils.collection.widening_map)
        new SetADInfo(m)
      }

      private def getLabels: List[Label] = theMap.keys.toList

      private def getEntry(lab: Label): Entry = {
        theMap(lab) match {
          case res: Entry => res
          case _ => {
            println("DEBUG: BIGBIG PROBLEM HERE!")
            Entry()
          }
        }
      }

      // @TODO: remove following methods when sure
      /**
        * private def getExplFlow(lab: Label): (Set[FlowElement], Set[FlowElement]) =
        * if (theMap contains lab)
        * (theMap(lab).oExplStm, theMap(lab).uExplStm)
        * else
        * (Set[FlowElement](), Set[FlowElement]())

        * private def getImplFlow(lab: Label): (Set[FlowElement], Set[FlowElement]) =
        * if (theMap contains lab)
        * (theMap(lab).oImplStm, theMap(lab).uImplStm)
        * else
        * (Set[FlowElement](), Set[FlowElement]())

        * private def getExplDegr(lab: Label): (Map[DegrElement, DegrAttrib], Map[DegrElement, DegrAttrib]) =
        * if (theMap contains lab)
        * (theMap(lab).oExplDegr, theMap(lab).uExplDegr)
        * else
        * (Map[DegrElement, DegrAttrib](), Map[DegrElement, DegrAttrib]())

        * private def getImplDegr(lab: Label): (Map[DegrElement, DegrAttrib], Map[DegrElement, DegrAttrib]) =
        * if (theMap contains lab)
        * (theMap(lab).oImplDegr, theMap(lab).uImplDegr)
        * else
        * (Map[DegrElement, DegrAttrib](), Map[DegrElement, DegrAttrib]())

        * private def getSize(lab: Label): BitQuantity =
        * if (theMap contains lab)
        * theMap(lab).size
        * else
        * BitQuantity.empty

        * private def getRowSafe(lab: Label) =
        * if (theMap contains lab)
        * theMap(lab)
        * else
        * Entry.empty
        **/

      def pretty: String = {
        val rows = for ((k, v) <- theMap) yield "%s: %s" format (k.name, v.pretty)
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
      //@TODO: remove following method
      /**
      def starFromUid(position: Uid): ADInfo[FunAnnot, Uid, AbstractValue] = {
        newInfo(List(Label.starUid(position.toString)))
      }**/
      val star = newInfo(List(Label.star)) //empty adexp, it contains only a star label
      val empty = newInfo(List()) //empty adexp, it contains only a star label
    }
  }

  type CADInfo = ADInfo[FunAnnot, Uid, AbstractValue]
  val CADInfoFactory = CADInfoImpl.Factory
}
