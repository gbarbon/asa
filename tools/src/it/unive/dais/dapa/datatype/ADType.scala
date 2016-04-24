package it.unive.dais.dapa.datatype

/**
  * @author esteffin
  * @author gbarbon
  */

import it.unive.dais.dapa.utils.pretty_doc.pretty_doc

//the Atomic Data Interface
object ADType {

  trait ADInfo[FunAnnot, Uid, AbstractValue] extends pretty_doc {
    def update(ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] // label from this, flow element as parameter, binary operators
    def asImplicit: ADInfo[FunAnnot, Uid, AbstractValue] // convert the current ADInfo to implicit only
    def join(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] //join two ADInfo, this with the argument
    def widening(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] // widening
    def meet(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue]
    def union(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue]
  }

  trait ADInfoFactory[FunAnnot, Uid, AbstractValue,Label, LabelAnnot] {
    def fromLabelAnnot(ann: LabelAnnot): ADInfo[FunAnnot, Uid, AbstractValue]
    def newInfo(aLabel: Label): ADInfo[FunAnnot, Uid, AbstractValue] = newInfo(List(aLabel))
    def newInfo(labels: List[Label]): ADInfo[FunAnnot, Uid, AbstractValue]
    val star: ADInfo[FunAnnot, Uid, AbstractValue] //empty adexp, it contains only a star label
    val empty: ADInfo[FunAnnot, Uid, AbstractValue]
  }
}
