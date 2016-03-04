package it.unive.dais.dapa.datatype

import it.unive.dais.dapa.absyn._
import it.unive.dais.dapa.datatype.ABSValue._
import it.unive.dais.dapa.datatype.FortyTwo._
import it.unive.dais.dapa.utils.pretty_doc.{pretty_doc, prettySet, prettyMap, prettyStrMap}

import scala.util.Success

//import it.unive.dais.dapa.datatype.LMH._
import org.kiama.output.PrettyPrinter._
import it.unive.dais.dapa.exception._
import it.unive.dais.dapa.utils._
import it.unive.dais.dapa.datatype.ADType._
import it.unive.dais.dapa.utils.collection.map._
import it.unive.dais.dapa.utils.prelude._

/**
 * @author esteffin
 * @author gbarbon
 */
object CADInfo {

  object CADInfoImpl {

    // Degradation element definition
    private case class DegrElement(
        aFunAnnot: FunAnnot,
        position: Uid,
        argPos: Int) extends pretty_doc {
      override def pretty_doc = parens(aFunAnnot.name <> ("_%d" format argPos) <> comma <+> position.toString)
      override def pretty = "(%s, %s)" format (aFunAnnot.name, position.toString)
    }

    // Flow Element definition
    private case class FlowElement (
        aFunAnnot: FunAnnot,
        aLabel: Label,
        argPos: Int) extends pretty_doc {
      override def pretty_doc = parens (aFunAnnot.name <> ("_%d" format argPos) <> comma <+> aLabel.name)
      override def pretty = "(%s, %s)" format (aFunAnnot.name, aLabel.name)
    }

    private case class DegrAttrib (
        abstrVal: AbstractValue,
        iters: Iterations) extends pretty_doc {
      override def pretty_doc = parens(abstrVal.pretty_doc <> comma <+> iters.pretty)
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
      * @param oStm Over approximation of the statements applied to the label (explicit flow, not used at this time)
      * @param uStm Under approximation of the statements applied to the label (explicit flow, not used at this time)
      *  ... Over approximation of the statements applied to the label (implicit flow)
      *  ... Under approximation of the statements applied to the label (implicit flow)
      */

    /*
    private case class Entry (
        oExplStm: Set[FlowElement] = Set.empty,
        uExplStm: Set[FlowElement] = Set.empty,
        oImplStm: Set[FlowElement] = Set.empty,
        uImplStm: Set[FlowElement] = Set.empty,
        oExplDegr: Map[DegrElement, DegrAttrib] = Map.empty,
        uExplDegr: Map[DegrElement, DegrAttrib] = Map.empty,
        oImplDegr: Map[DegrElement, DegrAttrib] = Map.empty,
        uImplDegr: Map[DegrElement, DegrAttrib] = Map.empty,
        size: BitQuantity = BitQuantity.empty) extends pretty_doc {

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

      override def pretty_doc = {
        val expl =
          if (oImplStm.isEmpty && uImplStm.isEmpty) text("E: [{}:{}]")
          else "E:" <+> brackets(prettySet(oExplStm) <> colon <%> prettySet(uExplStm))
        val impl =
          if (oImplStm.isEmpty && uImplStm.isEmpty) text("I: [{}:{}]")
          else "I:" <+> brackets(prettySet(oImplStm) <> colon <%> prettySet(uImplStm))
        val edegr =
          if (oExplDegr.isEmpty && uExplDegr.isEmpty) text("ED: [[]:[]]")
          else "ED:" <+> brackets(prettyMap(oExplDegr) <> colon <%> prettyMap(uExplDegr))
        val idegr =
          if (oImplDegr.isEmpty && uImplDegr.isEmpty) text("ID: [[]:[]]")
          else "ID:" <+> brackets(prettyMap(oImplDegr) <> colon <%> prettyMap(uImplDegr))
        val sz = "size:" <+> brackets(size.toString)

        expl <%> impl <%> edegr <%> idegr <%> sz

      }

      override def pretty: String = {
        "E:[%s:%s] I:[%s:%s] ED:[%s:%s] ID:[%s.%s] size:[%s]".
          format(
            pretty_print.prettySet(oExplStm map { _.toString }),
            pretty_print.prettySet(uExplStm map { _.toString }),
            pretty_print.prettySet(oImplStm map { _.toString }),
            pretty_print.prettySet(uImplStm map { _.toString }),
            pretty_print.prettySet((oExplDegr map { _.toString }).toSet),
            pretty_print.prettySet((uExplDegr map { _.toString } ).toSet),
            pretty_print.prettySet((oImplDegr map { _.toString } ).toSet),
            pretty_print.prettySet((uImplDegr map { _.toString } ).toSet),
            size.toString())
      }
    }
    */

    private case class Entry (
                               oStm: Set[FlowElement] = Set.empty,
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
    class SetADInfo private (
                              private val explMap: Map[Label, Entry] = Map(),
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
                acc updated(key, entry.addStm(FlowElement(ann, key, 1), DegrElement(ann, pos, 1), Vals._1))
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
                //println("Printing 1stMap: " + key)
                otherADInfo.getExplLabels.foreach(lab => {
                  // @TODO: cast abstracValue to abstractDegradationValue still missing
                  val newentry: Entry = entry.addStm(FlowElement(ann, lab, 2), DegrElement(ann, pos, 2), Vals._2)
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
                    val newentry: Entry = entry.addStm(FlowElement(ann, key, 1), DegrElement(ann, pos, 1), Vals._1)
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
        /**val newMap =
          explMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) =>
              val newEntry = Entry(
                oImplStm = entry.oStm ++ entry.oImplStm,
                uImplStm = entry.uStm ++ entry.uImplStm,
                oImplDegr = join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, entry.oDegr , entry.oImplDegr),
                uImplDegr = join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, entry.uDegr , entry.uImplDegr),
                size = entry.size)
              acc updated (key, newEntry)
          }*/
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
      private def getimplLabels: List[Label] = implMap.keys.toList

      private def getExplEntry(lab: Label): Entry = {
        explMap(lab) match {
          case res: Entry => res
          case _ => {
            println("Something wrong here...")
            // @TODO: use exception instead
            Entry()
          }
        }
      }
      private def getImplEntry(lab: Label): Entry = {
        implMap(lab) match {
          case res: Entry => res
          case _ => {
            println("Something wrong here...")
            // @TODO: use exception instead
            Entry()
          }
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
