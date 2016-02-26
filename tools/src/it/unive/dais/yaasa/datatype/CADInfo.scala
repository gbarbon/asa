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

      def join(r: DegrAttrib): DegrAttrib = {
        //println("DEBUG: join iters, left is "+ this.iters + ", right is "+ r.iters + " Join is: "+ (this.iters join r.iters))
        DegrAttrib(this.abstrVal join r.abstrVal, this.iters join r.iters)}
      def meet(r: DegrAttrib): DegrAttrib = DegrAttrib(this.abstrVal meet r.abstrVal, this.iters meet r.iters)
      def widening(r: DegrAttrib): DegrAttrib = {
        //println("DEBUG: Widening between two abstract values "+this.abstrVal +" and "+r.abstrVal)
        //println("DEBUG: Widening between two iterations "+this.iters +" and "+r.iters)
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

      // add method for statements lists
      def addExpl(fstm: FlowElement, dstm: DegrElement, theVal: AbstractValue) = {
        var tmpOExplDegr, tmpUExplDegr: Map[DegrElement, DegrAttrib] = Map.empty
        if (oExplDegr contains dstm) {
          val prev_el: DegrAttrib = oExplDegr(dstm)
          tmpOExplDegr = oExplDegr updated(dstm, DegrAttrib(theVal join prev_el.abstrVal, prev_el.iters.incr))
        }
        else {
          tmpOExplDegr = oExplDegr + (dstm -> DegrAttrib(theVal, Iterations.oneIter))
        }
        if (uExplDegr contains dstm) {
          val prev_el: DegrAttrib = uExplDegr(dstm)
          tmpUExplDegr = uExplDegr updated(dstm, DegrAttrib(theVal join prev_el.abstrVal, prev_el.iters.incr))
        }
        else {
          tmpUExplDegr = uExplDegr + (dstm -> DegrAttrib(theVal, Iterations.oneIter))
        }
        this.copy(oExplStm = oExplStm + fstm, uExplStm = uExplStm + fstm,
          oExplDegr = tmpOExplDegr, uExplDegr = tmpUExplDegr)
      }

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
      private[CADInfo] def this(labels: List[Label]) = this((for (label <- labels) yield (label, Entry.empty)).toMap)

      def update(ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo[FunAnnot, Uid, AbstractValue] = null): ADInfo[FunAnnot, Uid, AbstractValue] = {
        var newMap = Map[Label, Entry]()
        anADExp match {
          case null => newMap = // here unop (or single arguments function) update
            theMap.foldLeft(Map.empty[Label, Entry]) {
              case (acc, (key, entry)) =>
                // @TODO: cast abstracValue to abstractDegradationValue still missing
                acc updated(key, entry.addExpl(FlowElement(ann, key), DegrElement(ann, pos), Vals._1))
            }
          case _ => // here binop (or two arguments function) update
            val otherADInfo = anADExp match {
              case x: SetADInfo => x
              case _ => throw new ClassCastException
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
            theMap.foreach {
              case (key, entry) =>
                otherADInfo.getLabels.foreach(lab => {
                  // @TODO: cast abstracValue to abstractDegradationValue still missing
                  newMap = newMap updated(key, entry.addExpl(FlowElement(ann, lab), DegrElement(ann, pos), Vals._1))
                })
            }
            otherADInfo.getLabels.foreach {
              lab => {
                val entry = otherADInfo.getEntry(lab)
                theMap.foreach {
                  case (key, _) =>
                    // @TODO: cast abstracValue to abstractDegradationValue still missing
                    val newentry: Entry = entry.addExpl(FlowElement(ann, key), DegrElement(ann, pos), Vals._2)
                    if (newMap.keys.exists {
                      _ == lab
                    }) {
                      newMap = newMap.updated(lab, newentry join newMap(lab))
                    }
                    else
                      newMap = newMap.updated(lab, newentry)
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
              case (Some(l), None) => k -> l
              case (None, Some(r)) => k -> r
              case (None, None) => throw new Unexpected("Exception in union, file CADInfo, line 338")
            }
          }).toMap
        new SetADInfo(m)
      }

      def widening(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val m = widening_map[Label, Entry]({ case (l, r) => l widening r }, theMap, anADInfo.asInstanceOf[SetADInfo].theMap)
        new SetADInfo(m)
      }

      private def getLabels: List[Label] = theMap.keys.toList

      private def getEntry(lab: Label): Entry = {
        theMap(lab) match {
          case res: Entry => res
          case _ => {
            println("Something wrong here...")
            // @TODO: use exception instead
            Entry()
          }
        }
      }

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
      val star = newInfo(List(Label.star)) //empty adexp, it contains only a star label
      val empty = newInfo(List())
    }
  }

  type CADInfo = ADInfo[FunAnnot, Uid, AbstractValue]
  val CADInfoFactory = CADInfoImpl.Factory
}
