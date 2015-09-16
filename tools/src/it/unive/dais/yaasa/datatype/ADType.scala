package it.unive.dais.yaasa.datatype

/**
 * @author gbarbon
 */

//@FIXME: remove Label class from abstrac_values ?
//import it.unive.dais.yaasa.abstract_values._
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
    //@FIXME: annotations not printed
    def pretty = ""
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
    //@FIXME: annotations not printed
    def pretty = ""
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

    override def toString() = "[%d-%d]" format (oQuant, uQuant)
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
    def newLabel(ann: LabelAnnot) = Label(ann.name, ann.confidentiality, ann.dimension)
  }

  /**
   * Statement applied to the label. The statement can be a function or an operator.
   * @constructor create a new statement instance with a name, an obfuscation power and a quantity of released bit
   * @param name name of the function or the operator
   * @param obf the obfuscation power of the statement
   * @param implq the quantity of bits released by the statement
   * @param aLabel the associated label @FIXME: is this correct?
   */
  trait FlowElement {
    // val aLabel: Label
    def pretty: String
    override def toString() = pretty
  }

  trait FlowElementFactory {
    def newElem(name: String, obf: Obfuscation, implq: BitQuantity): FlowElement
    def fromFunAnnot(anAnnot: FunAnnot): FlowElement = newElem(anAnnot.name, anAnnot.obfuscation, anAnnot.quantity)
  }

  // The Atomic Data Interface
  trait ADInfo {

    //def newEntry(aLabel: Label) // to add a label to the ADExp
    /**
     * def newExplStm(aLabel: Label, aStm: EStatement) // to add a statement to the explicit flow of a given label
     * def newImplStm(aLabel: Label, aStm: EStatement) // to add a statement to the implicit flow of a given label
     * def updExplQnt(aLabel: Label, aQnt: BitQuantity) // to update the quantity released in the explicit flow for a given label
     * def updImplQnt(aLabel: Label, aQnt: BitQuantity) // to update the quantity released in the implicit flow for a given label
     */

    def update(elem: FlowElement) // label from this, flow element as parameter, unary operators
    def update(anADExp: ADInfo, elem: FlowElement) // label from this, flow element as parameter, binary operators
    def update(ADExps: List[ADInfo], elem: FlowElement) //

    /**
     * def returnExplStms(aLabel: Label): (List[EStatement], List[EStatement]) // to return the list of statements that belongs to the explicit flow of a given label
     * def returnImplStms(aLabel: Label): (List[EStatement], List[EStatement]) // to return the list of statements that belongs to the implicit flow of a given label
     * def returnExplQnt(aLabel: Label): BitQuantity // to return the bit quantity released by the explicit flow for a given label
     * def returnImplQnt(aLabel: Label): BitQuantity // to return the bit quantity released by the implicit flow for a given label
     */
  }

  trait ADInfoFactory {
    def newInfo(aLabel: Label): ADInfo = newInfo(List(aLabel))
    def newInfo(labels: List[Label]): ADInfo
    def star = newInfo(List(Label.star)) //empty adexp, it contains only a star label
  }
}
