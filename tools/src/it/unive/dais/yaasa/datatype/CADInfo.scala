package it.unive.dais.yaasa.datatype

//import it.unive.dais.yaasa.datatype.LMH.CLattice.Factory
//import lattice._
import it.unive.dais.yaasa.absyn._
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
object CADInfo {

  //TODO: This should be done as the previous lattice using the opaque interface ADInfo
  object CADInfo {

    // @TODO: temporary DegrElement class, check it
    private case class DegrElement(
        aFunAnnot: FunAnnot,
        position: Uid) {

      override def toString() = "(%s, %s)" format (aFunAnnot.name, position.toString)
    }

    private case class FlowElement(
        aFunAnnot: FunAnnot,
        aLabel: Label) {
      override def toString() = "(%s, %s)" format (aFunAnnot.name, aLabel.name)
    }

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
        oExplDegr: Map[DegrElement, (AbstractValue, Iterations)] = Map.empty,
        uExplDegr: Map[DegrElement, (AbstractValue, Iterations)] = Map.empty,
        oImplDegr: Map[DegrElement, (AbstractValue, Iterations)] = Map.empty,
        uImplDegr: Map[DegrElement, (AbstractValue, Iterations)] = Map.empty,
        size: BitQuantity = BitQuantity()) {

      // "add" methods for statements lists
      def addOExpStm(stm: FlowElement) = this.copy(oExpStm = oExpStm + stm)
      def addUExpStm(stm: FlowElement) = this.copy(uExpStm = uExpStm + stm)
      //def addOImplStm(stm: FlowElement) = this.copy(oImplStm = oImplStm + stm)
      //def addUImpltm(stm: FlowElement) = this.copy(uImplStm = uImplStm + stm)

      def addOExplDegr(stm: DegrElement, theVal: AbstractValue) = {
        if (oExplDegr contains stm) {
          val prev_el: (AbstractValue, Iterations) = oExplDegr(stm)
          this.copy(uExplDegr = oExplDegr + (stm -> (prev_el._1.join(theVal), prev_el._2.join(Iterations.oneIter))))
        }
        else
          this.copy(uExplDegr = oExplDegr + (stm -> (theVal, Iterations.oneIter)))
      }
      def addUExplDegr(stm: DegrElement, theVal: AbstractValue) = {
        if (uExplDegr contains stm) {
          val prev_el: (AbstractValue, Iterations) = uExplDegr(stm)
          this.copy(uExplDegr = uExplDegr + (stm -> (prev_el._1.join(theVal), prev_el._2.join(Iterations.oneIter))))
        }
        else
          this.copy(uExplDegr = uExplDegr + (stm -> (theVal, Iterations.oneIter)))
      }

      // def addOImplDegr(stm: DegrElement) = this.copy(oImplDegr = oImplDegr + stm)
      // def addUImplDegr(stm: DegrElement) = this.copy(uImplDegr = uImplDegr + stm)

      def addExpStm(stm: FlowElement) = this.copy(oExpStm = oExpStm + stm, uExpStm = uExpStm + stm)
      // def addImplStm(stm: FlowElement) = this.copy(oImplStm = oImplStm + stm, uImplStm = uImplStm + stm)
      def addExplDegr(stm: DegrElement, theVal: AbstractValue) = this.addOExplDegr(stm, theVal).addUExplDegr(stm, theVal)
      // def addImplDegr(stm: DegrElement) = this.copy(oImplDegr = oImplDegr + stm, uImplDegr = uImplDegr + stm)

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
            // prettySet(oExplDegr map { _.toString() }), // @FIXME: type mismatch error on maps
            // prettySet(uExplDegr map { _.toString() }), // @FIXME: type mismatch error on maps
            // prettySet(oImplDegr map { _.toString() }), // @FIXME: type mismatch error on maps
            // prettySet(uImplDegr map { _.toString() }), // @FIXME: type mismatch error on maps
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

      def update(ann: FunAnnot, pos: Uid, aVal: AbstractValue): ADInfo = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => {
              val updatedEntry = entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), aVal)
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
      def update(ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo): ADInfo = {
        var newMap = Map[Label, Entry]()
        val otherADInfo = anADExp match {
          case x: SetADInfo => x
          case _            => throw new ClassCastException
        }
        theMap.foreach {
          case (key, entry) => {
            otherADInfo.getLabels.foreach(lab => {
              // @FIXME: check Vals._2 if correct
              val updatedEntry = entry.addExpStm(FlowElement(ann, lab)).addExplDegr(DegrElement(ann, pos), Vals._1)
              newMap = newMap updated (key, updatedEntry)
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
                  val updatedEntry = entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), Vals._2)
                  newMap = newMap updated (lab, updatedEntry)
                }
              }
            }
        }
        new SetADInfo(newMap)
      }
      //@FIXME: still incomplete function!
      def update(ann: FunAnnot, pos: Uid, Vals: List[AbstractValue], ADExps: List[ADInfo]): ADInfo = {
        //@FIXME: old ADExp value is now a couple ADExp, AbstrVAlue cotained in ADExpsWVals
        /*val args = ADExps.cast[SetADInfo]
        val adexps = (this :: ADExps) map {
          case s: SetADInfo => s
          case _            => throw new Unexpected("Wrong type implementation")
        }
        val keys = adexps.foldLeft(Set.empty[Label])((s, l) => s ++ l.theMap.keys)
        val joined =
          for (label <- keys)
            yield (label -> ((adexps map { _.getRowSafe(label) }).foldLeft(Entry.empty) { (acc, entry) => acc join entry }))
        //@FIXME: qua devi aggiungere ad ogni elemento di joined la combinazione con ... Probabilmente BROKEN...
        */ Factory.newInfo(Label.star) //@FIXME: temporary WRONG solution
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
  //type CADInfo = ADType[CADInfo.SetADInfo]
  //val ConfLatticeFactory: LatticeFactory[CLattice.LMHV] = SetADInfo.Factory
}
