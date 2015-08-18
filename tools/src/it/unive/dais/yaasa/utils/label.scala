package it.unive.dais.yaasa.utils

/**
 * @author gbarbon
 *
 */

import scala.collection.mutable.ListBuffer

/**
 * This object contains all the classes used by the analysis.
 */
object abstract_values {

  /**
   * Obfuscation class
   * @constructor create a new obfuscation instance
   * @param obf obfuscation value
   */
  class Obfuscation(obf: Double) {

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
  class Confidentiality(conf: Double) {

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
  class BitQuantity(_oQuant: Int = 0, _uQuant: Int = 0) {
    var oQuant = _oQuant
    var uQuant = _uQuant

    /**
     * Update of the quantitative value
     */
    def oUpdate = oQuant += 1
    def uUpdate = uQuant += 1

    /**
     * Print of the quantitative value
     */
    def oPrint = oQuant
    def uPrint = uQuant
  }

  /**
   * @constructor create a new label with a name, a confidentiality level and a dimenstion
   * @param _name name o f the label
   * @param _conf the confidentiality value for the label
   * @param _dim dimension in bit of the label
   */
  class Label(_name: String, _conf: Confidentiality, _dim: BitQuantity) {
    val name = _name //name of the label
    val conf = _conf //confidentiality value of the label
    val dim = _dim // dimension in bit of the label

    /**
     * We use over-approximation variables only in case of concrete-only analysis
     */
    var oExpStm = ListBuffer[Statement]() // Over approximation of the statements applied to the label (explicit flow, not used at this time)
    var uExpStm = ListBuffer[Statement]() // Under approximation of the statements applied to the label (explicit flow, not used at this time)
    var oImplStm = ListBuffer[Statement]() // Over approximation of the statements applied to the label (implicit flow)
    var uImplStm = ListBuffer[Statement]() // Under approximation of the statements applied to the label (implicit flow)

    var oImplQuant = new BitQuantity() // Over approximation of the quantitative value released in the implicit flow
    var uImplQuant = new BitQuantity() // Under approximation of the quantitative value released in the implicit flow

    /**
     * Explicit flow, not used at this time
     * @param _stm a statement
     */
    def oExpListUpdater(_stm: Statement) {
      val stm = _stm
      oExpStm += stm
    }

    /**
     * Explicit flow, not used at this time
     * @param _stm a statement
     */
    def uExpListUpdater(_stm: Statement) {
      val stm = _stm
      uExpStm += stm
    }

    /**
     * @param _stm a statement
     */
    def oImplListUpdater(_stm: Statement) {
      val stm = _stm
      oImplStm += stm
    }

    /**
     * @param _stm a statement
     */
    def uImplListUpdater(_stm: Statement) {
      val stm = _stm
      oImplStm += stm
    }

    /**
     * Print only the name
     */
    def namePrint = name

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

    /**
     * Statement applied to the label. The statement can be a function or an operator.
     * @FIXME:  Does it must also have an associated label?
     *          If so, it is sufficient the name of the associated label, or we want a link to the other label object instance?
     * @constructor create a new statement instance with a name, an obfuscation power and a quantity of released bit
     * @param _name name of the function or the operator
     * @param _obf the obfuscation power of the statement
     * @param _implq the quantity of bit released by the statement
     * @param _aLabel the associated label @FIXME: is this correct?
     */
    class Statement(_name: String, _obf: Obfuscation, _implq: BitQuantity, _aLabel: Label) {
      val name = _name //name of the operator
      val obf = _obf // obfuscation power of the operator
      val implq = _implq // released bits
      val aLabel = _aLabel //associated label, see @FIXME above

      /**
       * It prints the Statement operator or function, with the associated label, see @FIXME above
       */
      def print = "(" + name + ", " + aLabel.namePrint + ")"
    }

    /**
     * Quantitative function (over approximation). It updates the implicit quantitative value each time the label is involved in an implicit flow.
     * @param _op Operation that produces implicit flow
     */
    def oImplQuantFunc(_stm: Statement) {
      //oImplQuant = oImplQuant + _stm.implq
      oImplQuant.oUpdate
    }

    /**
     * Quantitative function (under approximation).
     * @param _op Operation that produces implicit flow
     */
    def uImplQuantFunc(_stm: Statement) {
      //this.uimplq = this.uimplq + _stm.implq
      uImplQuant.uUpdate
    }
  }
}
