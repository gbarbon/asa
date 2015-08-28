package it.unive.dais.yaasa

/**
 * This object contains all the classes used by the analysis.
 */
object abstract_values {

  /**
   * Abstraction class
   */
  class Abstraction() {
    def leastUpperBound() = {}
    def greatestLowerBound() = {}
  }

  /**
   * Obfuscation class
   * @constructor create a new obfuscation instance
   * @param obf obfuscation value
   */
  class Obfuscation(obf: Double) extends Abstraction {

  }
  //Instead, as definded in the paper it would be:
  /*
  class Obfuscation extends Enumeration {
    type Obfuscation = Value
    val L, M, H = Value
  }*/

  /**
   * Confidentiality class
   * @constructor
   * @param conf confidentiality value
   */
  class Confidentiality(conf: Double) extends Abstraction {

  }
  //Instead, as definded in the paper it would be:
  /*
  class Confidentiality extends Enumeration {
    type Confidentiality = Value
    val L, M, H = Value
  }*/

  /**
   * Quantitative value class
   */
  case class BitQuantity(oQuant: Int = 0, uQuant: Int = 0) {

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
  case class Statement(name: String, obf: Obfuscation, implq: BitQuantity, aLabel: Label) {
    /**
     * It prints the Statement operator or function, with the associated label, see @FIXME above
     */
    def print = "(" + name + ", " + aLabel.namePrint + ")"
  }

  /**
   * @constructor create a new label with a name, a confidentiality level and a dimension
   * @param name name of the label
   * @param conf the confidentiality value for the label
   * @param dim dimension in bit of the label
   * @param oExpStm Over approximation of the statements applied to the label (explicit flow, not used at this time)
   * @param uExpStm Under approximation of the statements applied to the label (explicit flow, not used at this time)
   * @param oImplStm Over approximation of the statements applied to the label (implicit flow)
   * @param uImplStm Under approximation of the statements applied to the label (implicit flow)
   * @param oImplQuant Over approximation of the quantitative value released in the implicit flow
   * @param uImplQuant Under approximation of the quantitative value released in the implicit flow
   * Notice:  We use over-approximation variables only in case of concrete-only analysis
   */
  case class Label(
      name: String,
      conf: Confidentiality,
      dim: BitQuantity,
      //type: String, @FIXME: add also the type of the label???
      oExpStm: List[Statement] = List[Statement](),
      uExpStm: List[Statement] = List[Statement](),
      oImplStm: List[Statement] = List[Statement](),
      uImplStm: List[Statement] = List[Statement](),
      oImplQuant: BitQuantity = new BitQuantity(),
      uImplQuant: BitQuantity = new BitQuantity()) {

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

    /**
     * "update" methods for quantitative values
     */
    def updateOImplQuant() = this.copy(oImplQuant = oImplQuant.oUpdate())
    def updateUImplQuant() = this.copy(uImplQuant = uImplQuant.oUpdate())

    /**
     * Concrete Print function.
     * It prints the extended atomic data expression for the current label.
     */
    def concretePrint = "{" + concExplFlowPrint + "}, {" + concImplFlowPrint + "}"
    def concExplFlowPrint = "<" + name + "{" + (oExpStm map print) + "}>" // explicit flow print function (concrete)
    def concImplFlowPrint = "<" + name + "{" + (oImplStm map print) + "}>" // implicit flow print function (concrete)

    // implicit quantitative value print function (concrete)
    def concImplQuantPrint = "<" + name + ", " + oImplQuant.oPrint + ">"

    /**
     * Abstract print functions.
     */
    def abstractPrint = "{" + abstExplFlowPrint + "}, {" + abstImplFlowPrint + "}"
    def abstExplFlowPrint = "<" + name + "{" + (uExpStm map print) + "}, {" + (oExpStm map print) + "}>" // explicit flow print function (abstract)
    def abstImplFlowPrint = "<" + name + "{" + (uImplStm map print) + "}, {" + (oImplStm map print) + "}>" // implicit flow print function (abstract)

    // implicit quantitative value print function (abstract)
    def abstImplQuantPrint = "<" + name + ", " + uImplQuant.uPrint + ", " + oImplQuant.oPrint + ">"
  }
}
