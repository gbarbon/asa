package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.datatype.ABSValue._
import it.unive.dais.yaasa.datatype.ADType._
import it.unive.dais.yaasa.datatype.FortyTwo._
import it.unive.dais.yaasa.exception._
import it.unive.dais.yaasa.utils._
import it.unive.dais.yaasa.utils.collection.map._
import it.unive.dais.yaasa.utils.prelude._
import it.unive.dais.yaasa.utils.pretty_doc.{pretty_doc, prettySet, prettyMap, prettyStrMap}
import org.kiama.output.PrettyPrinter._

/**
 * @author esteffin
 * @author gbarbon
 */
object CADInfo {

  object CADInfoImpl {

    // Degradation element definition
    private case class DegrElement(
        aFunAnnot: FunAnnot,
        position: Uid) extends pretty_doc {
      override def pretty_doc = parens(aFunAnnot.name <> comma <+> position.toString)
      override def pretty = "(%s, %s)" format (aFunAnnot.name, position.toString)
    }

    // Flow Element definition
    private case class FlowElement (
        aFunAnnot: FunAnnot,
        aLabel: Label) extends pretty_doc {
      override def pretty_doc = parens (aFunAnnot.name <> comma <+> aLabel.name)
      override def pretty = "(%s, %s)" format (aFunAnnot.name, aLabel.name)
    }

    // Degradation element attributes, composed of an AbstractValue and the Iterations interval
    private case class DegrAttrib (
        abstrVal: AbstractValue,
        iters: Iterations) extends pretty_doc {
      override def pretty_doc = parens(abstrVal.pretty_doc <> comma <+> iters.pretty)
      override def pretty = "(%s, %s)" format (abstrVal.toString() ,iters.toString())

      def join(r: DegrAttrib): DegrAttrib = {
        DegrAttrib(this.abstrVal join r.abstrVal, this.iters join r.iters)}
      def meet(r: DegrAttrib): DegrAttrib = DegrAttrib(this.abstrVal meet r.abstrVal, this.iters meet r.iters)
      def widening(r: DegrAttrib): DegrAttrib = {
        DegrAttrib(this.abstrVal widening r.abstrVal, this.iters widening r.iters)}
    }

    /**
      * An entry of the ADExp map
      * @constructor create a new atomic data expression of a certain label.
      * @param oStm Over approximation of the statements applied to the label
      * @param uStm Under approximation of the statements applied to the label
      */
    private case class Entry (oStm: Set[FlowElement] = Set.empty,
                              uStm: Set[FlowElement] = Set.empty,
                              oDegr: Map[DegrElement, DegrAttrib] = Map.empty,
                              uDegr: Map[DegrElement, DegrAttrib] = Map.empty,
                              size: BitQuantity = BitQuantity.empty) extends pretty_doc {

      // add method for statements lists
      def addStm(fstm: FlowElement, dstm: DegrElement, theVal: AbstractValue) = {
        var tmpOExplDegr, tmpUExplDegr: Map[DegrElement, DegrAttrib] = Map.empty
        if (oDegr contains dstm) {
          val prev_el: DegrAttrib = oDegr(dstm)
          tmpOExplDegr = oDegr updated(dstm, DegrAttrib(theVal join prev_el.abstrVal, prev_el.iters.incr))
        }
        else {
          tmpOExplDegr = oDegr + (dstm -> DegrAttrib(theVal, Iterations.oneIter))
        }
        if (uDegr contains dstm) {
          val prev_el: DegrAttrib = uDegr(dstm)
          tmpUExplDegr = uDegr updated(dstm, DegrAttrib(theVal join prev_el.abstrVal, prev_el.iters.incr))
        }
        else {
          tmpUExplDegr = uDegr + (dstm -> DegrAttrib(theVal, Iterations.oneIter))
        }
        this.copy(oStm = oStm + fstm, uStm = uStm + fstm,
          oDegr = tmpOExplDegr, uDegr = tmpUExplDegr)
      }

      def join(other: Entry): Entry = {
        Entry(
          oStm ++ other.oStm,
          uStm ++ other.uStm,
          join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, oDegr , other.oDegr),
          join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, uDegr , other.uDegr),
          size join other.size)
      }

      def meet(other: Entry): Entry = {
        Entry(oStm intersect other.oStm,
          uStm intersect other.uStm,
          meet_map[DegrElement, DegrAttrib]({ case (l, r) => l meet r }, oDegr , other.oDegr),
          meet_map[DegrElement, DegrAttrib]({ case (l, r) => l meet r }, uDegr , other.uDegr),
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
        for (stm <- this.uStm ++ other.uStm) {
          res = (this.uStm.contains(stm), other.uStm.contains(stm)) match {
            case (false, false) => throw new Unexpected("Cannot exists an element that does not exists in both sets.")
            case (true, true) => res.copy(uStm = res.uStm + stm)
            case (_, _) => res.copy()
            //res.copy(oExplStm = res.oExplStm + stm)
            // @TODO: check, we add it to the over approx? It should already be present
          }
        }
        // oExpl: if el exists only in one of the two (or in both), then add to the oExpl
        for (stm <- this.oStm ++ other.oStm) {
          res = (this.oStm.contains(stm), other.oStm.contains(stm)) match {
            case (false, false) => throw new Unexpected("Cannot exists an element that does not exists in both sets.")
            case (_, _) => res.copy(oStm = res.oStm + stm)
          }
        }

        // for the DegradationElements we must also performs the join and the meet
        for (el <- this.uDegr.keySet ++ other.uDegr.keySet) {
          res = (this.uDegr.get(el), other.uDegr.get(el)) match {
            case (None, None) => throw new Unexpected("Cannot exists an element that does not exists in both maps.")
            case (Some(l), Some(r)) =>
              res.copy(uDegr = uDegr updated(el, DegrAttrib(l.abstrVal meet r.abstrVal, l.iters meet r.iters)))
            case (_, _) => res.copy()
          }
        }
        for (el <- this.oDegr.keySet ++ other.oDegr.keySet) {
          res = (this.oDegr.get(el), other.oDegr.get(el)) match {
            case (None, None) => throw new Unexpected("Cannot exists an element that does not exists in both maps.")
            case (Some(l), None) => res.copy(oDegr = oDegr updated(el, l))
            case (None, Some(r)) => res.copy(oDegr = oDegr updated(el, r))
            case (Some(l), Some(r)) =>
              res.copy(oDegr = oDegr updated(el, DegrAttrib(l.abstrVal join r.abstrVal, l.iters join r.iters)))
          }
        }
        res.copy(size = this.size join other.size)
      }

      def widening(other: Entry): Entry = {
        Entry(
          //@FIXME: not sure widening is correct this way...
          oStm ++ other.oStm,
          uStm ++ other.uStm,
          widening_map[DegrElement, DegrAttrib]({ case (l, r) => l widening r }, oDegr , other.oDegr/*, DegrAttrib.empty*/),
          widening_map[DegrElement, DegrAttrib]({ case (l, r) => l widening r }, uDegr , other.uDegr/*, DegrAttrib.empty*/),
          size widening other.size)
      }

      // used when new label is created
      def createSize(ann: LabelAnnot) = this.copy(size = ann.dimension)

      override def pretty_doc = {
        val stm =
          if (oStm.isEmpty && uStm.isEmpty) text("S: [{}:{}]")
          else "S:" <+> brackets(prettySet(uStm) <> colon <%> prettySet(oStm))
        val degr =
          if (oDegr.isEmpty && uDegr.isEmpty) text("D: [[]:[]]")
          else "D:" <+> brackets(prettyMap(uDegr) <> colon <%> prettyMap(oDegr))
        //val sz = "size:" <+> brackets(size.toString)
        stm <%> degr

      }

      override def pretty: String = {
        "S:[%s:%s] D:[%s:%s]".
          format(
            pretty_print.prettySet(uStm map { _.toString }),
            pretty_print.prettySet(oStm map { _.toString }),
            pretty_print.prettySet((uDegr map { _.toString }).toSet),
            pretty_print.prettySet((oDegr map { _.toString } ).toSet))
            //size.toString())
      }
    }

    private object Entry {
      def empty = Entry()
    }

    // theMap: a map Label -> Entry
    class SetADInfo private (private val explMap: Map[Label, Entry] = Map(),
                             private val implMap: Map[Label, Entry] = Map()
                            ) extends ADInfo[FunAnnot, Uid, AbstractValue] with pretty_doc {

      private[CADInfo] def this() = this(Map.empty[Label, Entry], Map.empty[Label, Entry])
      private[CADInfo] def this(explLabels: List[Label], implLabels: List[Label]) = this(
        (for (label <- explLabels) yield (label, Entry.empty)).toMap,
        (for (label <- implLabels) yield (label, Entry.empty)).toMap)
      private[CADInfo] def this(labels: List[Label]) = this(
        (for (label <- labels) yield (label, Entry.empty)).toMap,
        Map.empty[Label, Entry])

      def update(ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo[FunAnnot, Uid, AbstractValue] = null): ADInfo[FunAnnot, Uid, AbstractValue] = {
        var newExplMap = Map[Label, Entry]()
        anADExp match {
          case null => newExplMap = // here unop (or single arguments function) update
            explMap.foldLeft(Map.empty[Label, Entry]) {
              case (acc, (key, entry)) =>
                // @TODO: cast abstracValue to abstractDegradationValue still missing
                acc updated(key, entry.addStm(FlowElement(ann, key), DegrElement(ann, pos), Vals._1))
            }
          case otherADInfo: SetADInfo => // here binop (or two arguments function) update
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
            explMap.foreach {
              case (key, entry) =>
                println("Printing 1stMap: " + key)
                otherADInfo.getExplLabels.foreach(lab => {
                  // @TODO: cast abstracValue to abstractDegradationValue still missing
                  val newentry: Entry = entry.addStm(FlowElement(ann, lab), DegrElement(ann, pos), Vals._2)
                  if (newExplMap.keys.exists {_ == key}) {
                    newExplMap = newExplMap.updated(key, newentry join newExplMap(key))
                  }
                  else
                    newExplMap = newExplMap.updated(key, newentry)
                })
            }
            otherADInfo.getExplLabels.foreach {
              lab => {
                val entry = otherADInfo.getExplEntry(lab)
                explMap.foreach {
                  case (key, _) =>
                    // @TODO: cast abstracValue to abstractDegradationValue still missing
                    val newentry: Entry = entry.addStm(FlowElement(ann, key), DegrElement(ann, pos), Vals._1)
                    if (newExplMap.keys.exists {_ == lab}) {
                      newExplMap = newExplMap.updated(lab, newentry join newExplMap(lab))
                    }
                    else
                      newExplMap = newExplMap.updated(lab, newentry)
                }
              }
            }
          case _ => throw new ClassCastException
        }
        new SetADInfo(newExplMap, implMap)
      }

      def newSize(ann: LabelAnnot) = {
        val newMap =
          explMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => acc updated (key, entry.createSize(ann))
          }
        new SetADInfo(newMap)
      }

      def asImplicit: ADInfo[FunAnnot, Uid, AbstractValue] = {
        val newMap = join_map[Label, Entry]({ case (l, r) => l join r }, explMap , implMap)
        new SetADInfo(Map.empty[Label, Entry], newMap)
      }

      def join(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val e = join_map[Label, Entry]({ case (l, r) => l join r }, explMap, anADInfo.asInstanceOf[SetADInfo].explMap)
        val i = join_map[Label, Entry]({ case (l, r) => l join r }, implMap, anADInfo.asInstanceOf[SetADInfo].implMap)
        new SetADInfo(e,i)
      }

      def meet(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val e = meet_map[Label, Entry]({ case (l, r) => l meet r }, explMap, anADInfo.asInstanceOf[SetADInfo].explMap)
        val i = meet_map[Label, Entry]({ case (l, r) => l meet r }, implMap, anADInfo.asInstanceOf[SetADInfo].implMap)
        new SetADInfo(e, i)
      }

      def union(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val otherExplMap: Map[Label, Entry] = anADInfo.asInstanceOf[SetADInfo].explMap
        val otherImplMap: Map[Label, Entry] = anADInfo.asInstanceOf[SetADInfo].implMap
        val explKeys = explMap.keySet ++ otherExplMap.keySet
        val implKeys = implMap.keySet ++ otherImplMap.keySet
        val e =  (for (k <- explKeys) yield {
            (explMap.get(k), otherExplMap.get(k)) match {
              case (Some(l), Some(r)) => k -> l.union(r)
              case (Some(l), None) => k -> l
              case (None, Some(r)) => k -> r
              case (None, None) => throw new Unexpected("Exception in union, file CADInfo, line 338")
            }
          }).toMap
        val i =  (for (k <- implKeys) yield {
          (implMap.get(k), otherImplMap.get(k)) match {
            case (Some(l), Some(r)) => k -> l.union(r)
            case (Some(l), None) => k -> l
            case (None, Some(r)) => k -> r
            case (None, None) => throw new Unexpected("Exception in union, file CADInfo, line 338")
          }
        }).toMap
        new SetADInfo(e,i)
      }

      def widening(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        val e = widening_map[Label, Entry]({ case (l, r) => l widening r }, explMap, anADInfo.asInstanceOf[SetADInfo].explMap)
        val i = widening_map[Label, Entry]({ case (l, r) => l widening r }, implMap, anADInfo.asInstanceOf[SetADInfo].implMap)
        new SetADInfo(e, i)
      }

      private def getExplLabels: List[Label] = explMap.keys.toList

      private def getExplEntry(lab: Label): Entry = {
        explMap(lab) match {
          case res: Entry => res
          case _ =>
            println("Something wrong here...")
            // @TODO: use exception instead
            Entry()
        }
      }

      override def pretty_doc = {
        // @TODO: improve print of Explicit / Implicit
        val expl = "Explicit:" <+> prettyStrMap(explMap map { case (k, v) => (k.name, v) })
        val impl = "Implicit:" <+> prettyStrMap(implMap map { case (k, v) => (k.name, v) })
        expl <%> impl
      }

      override def pretty: String = {
        val erows = for ((k, v) <- explMap) yield "%s: %s" format (k.name, v.pretty)
        val irows = for ((k, v) <- implMap) yield "%s: %s" format (k.name, v.pretty)
        pretty_print.vcat(erows) + pretty_print.vcat(irows)
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
