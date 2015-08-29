package it.unive.dais.yaasa

/**
 * This object contains all the classes used by the analysis.
 */
object abstract_values {

  /**
   * Abstraction class
   */
  trait Abstraction {
    def leastUpperBound() = {}
    def greatestLowerBound() = {}
  }

  /**
   * Obfuscation class
   */
  class Obfuscation extends Abstraction

  object Obfuscation {

    case class Low() extends Obfuscation
    case class Medium() extends Obfuscation
    case class High() extends Obfuscation

    def <==(l: Obfuscation, r: Obfuscation) =
      (l, r) match {
        case (Low(), _)        => true
        case (Medium(), Low()) => false
        case (Medium(), _)     => true
        case (High(), High())  => true
        case (High(), _)       => false
      }
  }

  /**
   * Confidentiality class
   */
  class Confidentiality extends Abstraction

  object Confidentiality {

    case class Low() extends Confidentiality
    case class Medium() extends Confidentiality
    case class High() extends Confidentiality

    def <==(l: Confidentiality, r: Confidentiality) =
      (l, r) match {
        case (Low(), _)        => true
        case (Medium(), Low()) => false
        case (Medium(), _)     => true
        case (High(), High())  => true
        case (High(), _)       => false
      }
  }

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
  // changed aLabel from Label to String
  case class Statement(name: String, obf: Obfuscation, implq: BitQuantity, aLabel: String) {
    /**
     * It prints the Statement operator or function, with the associated label, see @FIXME above
     */
    def print = "(" + name + ", " + aLabel + ")"
  }

  //@FIXME: loading from operators.csv missing
  object Statement {
    def BOPlusPlus(aLabel: String) = Statement("++", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOPlusPlus,++,L,0
    def BOPlus(aLabel: String) = Statement("+", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOPlus,+,L,0
    def BOMinus(aLabel: String) = Statement("-", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOMinus,-,L,0
    def BOMul(aLabel: String) = Statement("*", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOMul,*,L,0
    def BODiv(aLabel: String) = Statement("/", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BODiv,/,L,0
    def BOAnd(aLabel: String) = Statement("&&", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOAnd,&&,L,0
    def BOOr(aLabel: String) = Statement("||", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOOr,||,L,0
    def BOMod(aLabel: String) = Statement("%", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOMod,%,L,0
    def BOLt(aLabel: String) = Statement("<", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOLt,<,L,0
    def BOLeq(aLabel: String) = Statement("<=", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOLeq,<=,L,0
    def BOEq(aLabel: String) = Statement("==", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOEq,==,L,0
    def BOGt(aLabel: String) = Statement(">", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOGt,>,L,0
    def BOGeq(aLabel: String) = Statement(">=", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BOGeq,>=,L,0
    def BONeq(aLabel: String) = Statement("!=", Obfuscation.Low(), BitQuantity(0, 0), aLabel) //BONeq,!=,L,0
    def UNeg(aLabel: String) = Statement("-", Obfuscation.Low(), BitQuantity(0, 0), aLabel) // UNot,!,L,0
    def UNot(aLabel: String) = Statement("!", Obfuscation.Low(), BitQuantity(0, 0), aLabel) // UNeg,-,L,0
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
    def addExpStm(stm: Statement) = this.copy(oExpStm = stm :: oExpStm).copy(uExpStm = stm :: uExpStm)
    def addImpltm(stm: Statement) = this.copy(oImplStm = stm :: oImplStm).copy(uImplStm = stm :: uImplStm)

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
  object Label {
    def empty = Label("star", Confidentiality.Low(), BitQuantity())
  }
}
