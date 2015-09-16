package it.unive.dais.yaasa.datatype

import ADType._

/**
 * @author gbarbon
 */
object adexp {

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

    //a map Label -> Entry
    class SetADInfo(theMap: Map[Label, Entry] = Map()) extends ADInfo {

      def newExplStm(aLabel: Label, aStm: EStatement) = {
        if (theMap.exists(_._1 == aLabel)) {
          val value = theMap(aLabel)
          value.addExpStm(aStm)
          theMap.updated(aLabel, value)
        }
        else {
          val newEntry = new Entry(oExpStm = List(aStm), uExpStm = List(aStm))
          val tempMap = (List(aLabel) zip List(newEntry)).toMap
          new SetADInfo(tempMap ++ theMap)
        }
      }

      def newImplStm(aLabel: Label, aStm: EStatement) = {
        if (theMap.exists(_._1 == aLabel)) {
          val value = theMap(aLabel)
          value.addImplStm(aStm)
          theMap.updated(aLabel, value)
        }
        else {
          val newEntry = new Entry(oImplStm = List(aStm), uImplStm = List(aStm))
          val tempMap = (List(aLabel) zip List(newEntry)).toMap
          new SetADInfo(tempMap ++ theMap)
        }
      }

      //def updExplQnt(aLabel: Label, aQnt: BitQuantity)

      def updImplQnt(aLabel: Label, aQnt: BitQuantity) = {
        if (theMap.exists(_._1 == aLabel)) {
          val value = theMap(aLabel)
          value.updateImplQuant(aQnt)
          theMap.updated(aLabel, value)
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

      object Factory extends ADInfoFactory {
        def newInfo(labels: List[Label]): SetADInfo = {
          new SetADInfo((for (label <- labels) yield (label, List.empty[Entry])).toMap)
        }
      }
    }
  }

  type CADInfo = CADInfo.SetADInfo
}
