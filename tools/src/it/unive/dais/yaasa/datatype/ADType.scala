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
    def pretty = "%s:%s:%s:%s" format (name, confidentiality, confidentiality.toString(), molteplicity)
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
  case class BitQuantity(oQuant: Int = 0, uQuant: Int = 0) {
    def this(quant: Int) = this(quant, quant)
    /**
     * Update of the quantitative value
     */
    def oUpdate() = this.copy(oQuant = oQuant + 1)
    def uUpdate() = this.copy(uQuant = uQuant + 1)
    def update(qnt: BitQuantity) = this.copy(oQuant = oQuant + qnt.oQuant, uQuant = uQuant + qnt.uQuant)

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
    def fromBoolean: BitQuantity = BitQuantity.oneBit
    def fromEquality(aLab: Label): BitQuantity = BitQuantity(aLab.dim.oQuant, 1)
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

  // The Atomic Data Interface
  trait ADInfo {
    def update(ann: FunAnnot): ADInfo // label from this, flow element as parameter, unary operators
    def update(anADExp: ADInfo, ann: FunAnnot): ADInfo // label from this, flow element as parameter, binary operators
    def update(ADExps: List[ADInfo], ann: FunAnnot): ADInfo

    //@FIXME: le due funzioni qui di seguito potrebbero non servire!
    //def updateImpl(ann: FunAnnot): ADInfo
    //def updateImpl(anADExp: ADInfo, ann: FunAnnot): ADInfo

    def updateImpl(implInfo: Option[ADInfo]): ADInfo // FIXME: remove option type added for sake of compilation andjoin the this ADInfo with the implicit info passed as argument

    def updateIQnt(qnt: BitQuantity): ADInfo
    //def updateIQnt(qnt: BitQuantity, ADExps: List[ADInfo]): ADInfo //note: this has no sense! Why do we need to update a list of adexp?

    //@TODO: implicit update and quantities update methods still missing

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
