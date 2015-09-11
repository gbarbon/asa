package it.unive.dais.yaasa

/**
 * @author gbarbon
 */

//@FIXME: remove Label class from abstrac_values ?
import it.unive.dais.yaasa.abstract_values._

//the Atomic Data Interface
object ADType {

  trait ADInfo {

    //def newEntry(aLabel: Label) // to add a label to the ADExp
    def newExplStm(aLabel: Label, aStm: EStatement) // to add a statement to the explicit flow of a given label
    def newImplStm(aLabel: Label, aStm: EStatement) // to add a statement to the implicit flow of a given label
    def updExplQnt(aLabel: Label, aQnt: BitQuantity) // to update the quantity released in the explicit flow for a given label
    def updImplQnt(aLabel: Label, aQnt: BitQuantity) // to update the quantity released in the implicit flow for a given label

    def returnExplStms(aLabel: Label): (List[EStatement], List[EStatement]) // to return the list of statements that belongs to the explicit flow of a given label
    def returnImplStms(aLabel: Label): (List[EStatement], List[EStatement]) // to return the list of statements that belongs to the implicit flow of a given label
    def returnExplQnt(aLabel: Label): BitQuantity // to return the bit quantity released by the explicit flow for a given label
    def returnImplQnt(aLabel: Label): BitQuantity // to return the bit quantity released by the implicit flow for a given label
  }

  trait ADInfoFactory {
    def newInfo(aLabel: Label): ADInfo = newInfo(List(aLabel))
    def newInfo(labels: List[Label]): ADInfo

    def empty = newInfo(List[Label]())
  }
}
