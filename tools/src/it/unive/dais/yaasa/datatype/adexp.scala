package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.abstract_values._
import it.unive.dais.yaasa.ADType._

/**
 * @author gbarbon
 */
object adexp {

  // an entry of the ADExp map
  trait Entry {
    val oExpStm: List[EStatement]
    val uExpStm: List[EStatement]
    val oImplStm: List[EStatement]
    val uImplStm: List[EStatement]
    val explQuant: BitQuantity
    val implQuant: BitQuantity

    /**
     * "add" methods for statements lists
     * @param stm a statement
     */
    /**
     * def addOExpStm(stm: EStatement) = this.copy(oExpStm = stm :: oExpStm)
     * def addUExpStm(stm: EStatement) = this.copy(uExpStm = stm :: uExpStm)
     * def addOImplStm(stm: EStatement) = this.copy(oImplStm = stm :: oImplStm)
     * def addUImpltm(stm: EStatement) = this.copy(uImplStm = stm :: uImplStm)
     * def addExpStm(stm: EStatement) = this.copy(oExpStm = stm :: oExpStm).copy(uExpStm = stm :: uExpStm)
     * def addImpltm(stm: EStatement) = this.copy(oImplStm = stm :: oImplStm).copy(uImplStm = stm :: uImplStm) *
     */
  }

  //a map Label -> Entry
  class ADExp(
      theMap: Map[Label, Entry] = Map()) extends ADInfo {

    def newEntry(aLabel: Label) {
      if (theMap.contains(aLabel))
        throw new EvaluationException()
      else
        this.copy()
    }
    def newExplStm(aLabel: Label, aStm: EStatement)
    def newImplStm(aLabel: Label, aStm: EStatement)
    def updExplQnt(aLabel: Label, aQnt: BitQuantity)
    def updImplQnt(aLabel: Label, aQnt: BitQuantity)

    def returnExplStms(aLabel: Label): (List[EStatement], List[EStatement])
    def returnImplStms(aLabel: Label): (List[EStatement], List[EStatement])
    def returnExplQnt(aLabel: Label): BitQuantity
    def returnImplQnt(aLabel: Label): BitQuantity
  }
}
