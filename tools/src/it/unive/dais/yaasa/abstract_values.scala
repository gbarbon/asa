package it.unive.dais.yaasa

import datatype.type_definitions._
import types._
import it.unive.dais.yaasa.absyn._

/**
 * This object contains all the classes used by the analysis.
 */
object abstract_values {

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
    def empty = Label("star", CLattice.Low, BitQuantity())
    def newLabel(ann: LabelAnnot) = Label(ann.name, ann.confidentiality, ann.dimension)
  }

  /**
   * Statement applied to the label. The statement can be a function or an operator.
   * @FIXME:  Does it must also have an associated label?
   *          If so, it is sufficient the name of the associated label, or we want a link to the other label object instance?
   * @constructor create a new statement instance with a name, an obfuscation power and a quantity of released bit
   * @param name name of the function or the operator
   * @param obf the obfuscation power of the statement
   * @param implq the quantity of bits released by the statement
   * @param aLabel the associated label @FIXME: is this correct?
   */
  trait FlowElement {
    val name: String
    val obf: Obfuscation
    val implq: BitQuantity
    val aLabel: Label
    def pretty: String
    override def toString() = pretty
  }

  trait ADExpr {

    def pretty: String
    override def toString() = pretty
  }

  // changed aLabel from Label to String
  //@FIXME: added List[LMH} => LMH to fix error, but not sure it is correct
  case class EStatement(name: String, obf: Obfuscation, implq: BitQuantity, aLabel: Label) extends FlowElement {
    /**
     * It prints the Statement operator or function, with the associated label, see @FIXME above
     */
    def print = "<" + name + ", " + aLabel + ">" //"(" + name + ", " + aLabel + ")"
    override def toString() = print
  }

  object Statement {

    def sCreator(aLabel: Label, annot: FunAnnot) = EStatement(annot.name, annot.obfuscation, annot.quantity, aLabel)
  }

  /**
   * @constructor create a new atomic data expression of a certain label.
   * @param label the associated label
   * @param oExpStm Over approximation of the statements applied to the label (explicit flow, not used at this time)
   * @param uExpStm Under approximation of the statements applied to the label (explicit flow, not used at this time)
   * @param oImplStm Over approximation of the statements applied to the label (implicit flow)
   * @param uImplStm Under approximation of the statements applied to the label (implicit flow)
   * @param oImplQuant Over approximation of the quantitative value released in the implicit flow
   * @param uImplQuant Under approximation of the quantitative value released in the implicit flow
   * Notice:  We use over-approximation variables only in case of concrete-only analysis
   */
  class ADExp(
      val label: Label,
      //type: String, @FIXME: add also the type of the label???
      val oExpStm: List[Statement] = List[Statement](),
      val uExpStm: List[Statement] = List[Statement](),
      val oImplStm: List[Statement] = List[Statement](),
      val uImplStm: List[Statement] = List[Statement](),
      val implQuant: BitQuantity = new BitQuantity()) {
    val name = label.name

    override def toString() = {
      def print_stmts(l: List[Statement]) = utils.pretty_print.xhcat(",")(l map { _.toString() })
      "<(%s), %s, %s, %s>" format (label.toString(), print_stmts(oExpStm), print_stmts(oImplStm), implQuant.toString())
    }

    /**
     * Print only the name
     */
    def namePrint = name

    /**
     * "add" methods for statements lists
     * @param stm a statement
     */
    def addOExpStm(stm: Statement) = this.copy(oExpStm = stm :: oExpStm)
    def addUExpStm(stm: Statement) = this.copy(uExpStm = stm :: uExpStm)
    def addOImplStm(stm: Statement) = this.copy(oImplStm = stm :: oImplStm)
    def addUImpltm(stm: Statement) = this.copy(uImplStm = stm :: uImplStm)
    def addExpStm(stm: Statement) = this.copy(oExpStm = stm :: oExpStm).copy(uExpStm = stm :: uExpStm)
    def addImpltm(stm: Statement) = this.copy(oImplStm = stm :: oImplStm).copy(uImplStm = stm :: uImplStm)

    /**
     * "update" methods for quantitative values
     */
    def updateImplQuant() = this.copy(implQuant = implQuant.oUpdate())

    /**
     * Concrete Print function.
     * It prints the extended atomic data expression for the current label.
     */
    def concretePrint = "{" + concExplFlowPrint + "}, {" + concImplFlowPrint + "}"
    def concExplFlowPrint = "<" + name + "{" + (oExpStm map print) + "}>" // explicit flow print function (concrete)
    def concImplFlowPrint = "<" + name + "{" + (oImplStm map print) + "}>" // implicit flow print function (concrete)

    // implicit quantitative value print function (concrete)
    def concImplQuantPrint = "<" + name + ", " + implQuant.oPrint + ">"

    /**
     * Abstract print functions.
     */
    def abstractPrint = "{" + abstExplFlowPrint + "}, {" + abstImplFlowPrint + "}"
    def abstExplFlowPrint = "<" + name + "{" + (uExpStm map print) + "}, {" + (oExpStm map print) + "}>" // explicit flow print function (abstract)
    def abstImplFlowPrint = "<" + name + "{" + (uImplStm map print) + "}, {" + (oImplStm map print) + "}>" // implicit flow print function (abstract)

    // implicit quantitative value print function (abstract)
    def abstImplQuantPrint = "<" + name + ", " + implQuant.oPrint + ">"

  }
  object ADExp {
    def empty = ADExp(Label.empty)
    def newADExp(aLabel: Label) = ADExp(aLabel)
  }
}
