package it.unive.dais.yaasa.datatype

/**
 * @author gbarbon
 */

import lattice._
import types._
import it.unive.dais.yaasa.absyn._

//the Atomic Data Interface
object ADType {

  type Obfuscation = ObfLattice

  trait Annot
  case class LabelAnnot(name: String,
                        confidentiality: ConfLattice,
                        dimension: BitQuantity,
                        molteplicity: Int = 1) extends Annot {
    def pretty = "%s:%s:%s:%s" format (name, confidentiality, dimension.toString(), molteplicity)
    override def toString() = pretty
  }

  object LabelAnnot {
    def parse(strings: Map[String, String]) =
      {
        val name = strings("labelName")
        val conf = ConfLatticeFactory.parse(strings("conf"))
        val dim = new BitQuantity(strings("dim") toInt)
        if (strings contains "molt")
          LabelAnnot(name, conf, dim, strings("molt") toInt)
        else
          LabelAnnot(name, conf, dim)
      }
  }

  case class FunAnnot(name: String,
                      obfuscation: Obfuscation) extends Annot {
    def pretty = "%s:%s" format (name, obfuscation)
    override def toString() = pretty
  }

  object FunAnnot {
    def parse(strings: Map[String, String]) =
      {
        val name = strings("name")
        val init_c = ObfLatticeFactory.parse(strings("obf"))
        FunAnnot(name, init_c)
      }
  }

  /**
   * Quantitative value class
   */
  case class BitQuantity(uQuant: Int = 0, oQuant: Int = 0) {
    def this(quant: Int) = this(quant, quant)
    /**
     * Update of the quantitative value
     */
    def oUpdate() = this.copy(oQuant = oQuant + 1)
    def uUpdate() = this.copy(uQuant = uQuant + 1)
    def update(qnt: BitQuantity) = this.copy(uQuant = uQuant + qnt.uQuant, oQuant = oQuant + qnt.oQuant)

    /**
     * Print of the quantitative value
     */
    def oPrint = oQuant
    def uPrint = uQuant

    def join(r: BitQuantity): BitQuantity = {
      val l = this
      BitQuantity(l.oQuant + r.oQuant, l.uQuant + r.uQuant)
    }

    override def toString() = "[%d-%d]" format (oQuant, uQuant)
  }

  object BitQuantity {
    def empty = BitQuantity()
    def oneBit = BitQuantity(1, 1)
  }

  //@TODO: improve the definition of Iterations, now used with intervals, but can be modular
  /**
   * Iteration class
   */
  case class Iterations(uIter: Int = 0, oIter: Int = 0) {
    def this(iter: Int) = this(iter, iter)
    /**
     * Update of the iterations value
     */
    def oUpdate() = this.copy(oIter = oIter + 1)
    def uUpdate() = this.copy(uIter = uIter + 1)
    def update(iter: Iterations) = this.copy(uIter = uIter + iter.uIter, oIter = oIter + iter.oIter)

    /**
     * Print of the iterations value
     */
    def oPrint = oIter
    def uPrint = uIter

    def join(r: Iterations): Iterations = {
      val l = this
      Iterations(l.oIter + r.oIter, l.uIter + r.uIter)
    }

    override def toString() = "[%d-%d]" format (oIter, uIter)
  }

  object Iterations {
    def empty = Iterations()
    def oneIter = Iterations(1, 1)
  }

  /**
   * @constructor create a new Label with a name, a confidentiality level and a dimension
   * @param name name of the label
   * @param conf the confidentiality value for the label
   * @param dim dimension in bit of the label
   */
  case class Label(
      name: String,
      conf: ConfLattice,
      dim: BitQuantity) {
    override def toString() = "%s:%s:%s" format (name, conf.toString(), dim.toString())
  }

  object Label {
    def star = Label("star", CLattice.Low, BitQuantity())
    def newLabel(ann: LabelAnnot): List[Label] =
      for (i <- List.range(0, ann.molteplicity))
        yield (
        Label("%s_%s" format (ann.name, i), ann.confidentiality, ann.dimension))
  }

  case class FlowElement(
      aFunAnnot: FunAnnot,
      aLabel: Label) {
    override def toString() = "(%s, %s)" format (aFunAnnot.name, aLabel.name)
  }

  // @FIXME: Dummy AbstractValue!!
  trait AbstractValue {
    val value: Any
    val ty: Type

    def join(secondEl: AbstractValue): AbstractValue
    // @TODO: raise exception if types are not compatible

    override def toString() = "[%s]" format (value)
  }

  // @FIXME: temporary, same name of the type defined in the analyzer (with ConcreteValue)!!!
  type ValueWAbstr = (AbstractValue, ADInfo)

  // @TODO: temporary DegrElement class, check it
  case class DegrElement(
      aFunAnnot: FunAnnot,
      position: Uid) {
    override def toString() = "(%s, %s)" format (aFunAnnot.name, position.toString)
  }

  // The Atomic Data Interface
  trait ADInfo {
    def update(ann: FunAnnot, pos: Uid, aVal: AbstractValue): ADInfo // label from this, flow element as parameter, unary operators
    def update(ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo): ADInfo // label from this, flow element as parameter, binary operators
    def update(ann: FunAnnot, pos: Uid, Vals: List[AbstractValue], ADExps: List[ADInfo]): ADInfo

    def asImplicit: ADInfo // convert the current ADInfo to implicit only
    def join(anADInfo: ADInfo): ADInfo //join two ADInfo, this with the argument

    def pretty: String
    override def toString(): String = pretty
  }

  trait ADInfoFactory {
    def fromLabelAnnot(ann: LabelAnnot): ADInfo
    def newInfo(aLabel: Label): ADInfo = newInfo(List(aLabel))
    def newInfo(labels: List[Label]): ADInfo
    val star = newInfo(List(Label.star)) //empty adexp, it contains only a star label
    val empty = newInfo(List()) //empty adexp, it contains only a star label
  }
}
