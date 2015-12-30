package it.unive.dais.yaasa.datatype

/**
 * @author gbarbon
 */

import type_definitions._
import types._
import it.unive.dais.yaasa.absyn._

//the Atomic Data Interface
object ADType {

  type Obfuscation = (List[ConfLattice] => ConfLattice)

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
        val conf = CLattice.Factory.parse(strings("conf"))
        val dim = new BitQuantity(strings("dim") toInt)
        if (strings contains "molt")
          LabelAnnot(name, conf, dim, strings("molt") toInt)
        else
          LabelAnnot(name, conf, dim)
      }
  }

  case class FunAnnot(name: String,
                      obfuscation: Obfuscation,
                      quantity: BitQuantity) extends Annot {
    def pretty = "%s:%s:%s" format (name, obfuscation, quantity.toString())
    override def toString() = pretty
  }

  object FunAnnot {
    def parse(strings: Map[String, String]) =
      {
        val name = strings("name")
        val init_c = CLattice.Factory.parse(strings("obf"))
        val obf = { l: List[ConfLattice] => init_c }
        val dim = new BitQuantity(strings("implq") toInt)
        FunAnnot(name, obf, dim)
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

    //def fromInt(x: Int): BitQuantity
    // @FIXME: size in bit of an integer
    //def fromString(x: String): BitQuantity
    // @FIXME: number of char of the function * number of bits for each char
    // def fromBoolean: BitQuantity = BitQuantity.oneBit
    // def fromEquality(aQuant: BitQuantity): BitQuantity = BitQuantity(1, aQuant.oQuant)

    //@TODO: remove me!!
    //@TODO: halfQuantity is a temporary solution, used for integer operation
    // We can assume that we are loosing quantity of information for each label.
    // We model this loss as the half of the previous quantity (approximation).
    /*def halfQuantity(aQuant: BitQuantity): BitQuantity = {
      val newOquant =
        if (aQuant.oQuant != 0) aQuant.oQuant / 2
        else 0
      val newUquant =
        if (aQuant.uQuant != 0) aQuant.uQuant / 2
        else 0
      BitQuantity(newUquant, newOquant)
    }*/

    //@TODO: remove me!!
    /*
    def BOPlusPlus(aQuant: BitQuantity): BitQuantity = aQuant
    def BOPlus(aQuant: BitQuantity): BitQuantity = halfQuantity(aQuant) //@FIXME: see above note about halfQuantity
    def BOMinus(aQuant: BitQuantity): BitQuantity = halfQuantity(aQuant) //@FIXME: see above note about halfQuantity
    def BOMul(aQuant: BitQuantity): BitQuantity = halfQuantity(aQuant) //@FIXME: see above note about halfQuantity
    def BODiv(aQuant: BitQuantity): BitQuantity = halfQuantity(aQuant) //@FIXME: see above note about halfQuantity
    def BOAnd: BitQuantity = fromBoolean
    def BOOr: BitQuantity = fromBoolean
    def BOMod(aQuant: BitQuantity): BitQuantity = halfQuantity(aQuant) //@FIXME: see above note about halfQuantity
    def BOLt: BitQuantity = fromBoolean
    def BOLeq(aQuant: BitQuantity): BitQuantity = fromEquality(aQuant)
    def BOEq(aQuant: BitQuantity): BitQuantity = fromEquality(aQuant)
    def BOGt: BitQuantity = fromBoolean
    def BOGeq(aQuant: BitQuantity): BitQuantity = fromEquality(aQuant)
    def BONeq(aQuant: BitQuantity): BitQuantity = fromEquality(aQuant)
    def UNot: BitQuantity = oneBit
    def UNeg(aQuant: BitQuantity): BitQuantity = aQuant*/
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

    override def toString() = "[%s]" format (value)
  }

  // @FIXME: same name of the type defined in the analyzer (with ConcreteValue)!!!
  type ValueWAbstr = (AbstractValue, ADInfo)

  // @TODO: temporary DegrElement class, check it!
  case class DegrElement(
      aFunAnnot: FunAnnot,
      position: Uid,
      aVal: AbstractValue,
      iterations: Iterations) {
    override def toString() = "(%s, %s, %s, %s)" format (aFunAnnot.name, position.toString, aVal.toString, iterations.toString)
  }

  // The Atomic Data Interface
  trait ADInfo {
    def update(ann: FunAnnot, pos: Uid, aVal: AbstractValue): ADInfo // label from this, flow element as parameter, unary operators
    def update(ann: FunAnnot, pos: Uid, Vals: List[AbstractValue], anADExp: ADInfo): ADInfo // label from this, flow element as parameter, binary operators
    def update(ann: FunAnnot, pos: Uid, Vals: List[AbstractValue], ADExps: List[ADInfo]): ADInfo

    // @FIXME: add the DegrElement parameters to the update functions!!

    //def getLabels: List[Label] // return all the labels in the adexp
    //def getExplFlow(lab: Label): (Set[FlowElement], Set[FlowElement]) // return the explicit flow in the adexp, over and under approx
    //def getImplFlow(lab: Label): (Set[FlowElement], Set[FlowElement]) // return the implicit flow in the adexp, over and under approx
    //def getExplQuant(lab: Label): BitQuantity
    //def getImplQuant(lab: Label): BitQuantity

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
