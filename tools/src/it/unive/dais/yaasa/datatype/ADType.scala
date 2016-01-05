package it.unive.dais.yaasa.datatype

/**
 * @author gbarbon
 */

//the Atomic Data Interface
object ADType {

  // The Atomic Data Interface
  trait ADInfo[FunAnnot, Uid, AbstractValue] {
    def update(ann: FunAnnot, pos: Uid, aVal: AbstractValue): ADInfo[FunAnnot, Uid, AbstractValue] // label from this, flow element as parameter, unary operators
    def update(ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] // label from this, flow element as parameter, binary operators
    def update(ann: FunAnnot, pos: Uid, Vals: List[AbstractValue], ADExps: List[ADInfo[FunAnnot, Uid, AbstractValue]]): ADInfo[FunAnnot, Uid, AbstractValue]

    def asImplicit: ADInfo[FunAnnot, Uid, AbstractValue] // convert the current ADInfo to implicit only
    def join(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] //join two ADInfo, this with the argument

    def pretty: String
    override def toString(): String = pretty
  }

  trait ADInfoFactory[FunAnnot, Uid, AbstractValue,Label, LabelAnnot] {
    def fromLabelAnnot(ann: LabelAnnot): ADInfo[FunAnnot, Uid, AbstractValue]
    def newInfo(aLabel: Label): ADInfo[FunAnnot, Uid, AbstractValue] = newInfo(List(aLabel))
    def newInfo(labels: List[Label]): ADInfo[FunAnnot, Uid, AbstractValue]
    val star: ADInfo[FunAnnot, Uid, AbstractValue] //  = newInfo(List(Label.star)) //empty adexp, it contains only a star label
    val empty: ADInfo[FunAnnot, Uid, AbstractValue] // = newInfo(List()) //empty adexp, it contains only a star label
  }
}
