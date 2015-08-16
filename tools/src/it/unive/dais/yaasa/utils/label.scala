package it.unive.dais.yaasa.utils

/**
 * @author gbarbon
 *
 */

import scala.collection.mutable.ListBuffer

object abstract_values {

  /**
   * Obfuscation class
   */
  class Obfuscation() {

  }

  /**
   * Confidentiality class
   */
  class Confidentiality() {

  }

  /**
   * Quantitative value class
   */
  class BitQuantity(_quant: Int = 0) {
    var quant = _quant

    // It updates the quantitative value
    def update = quant += 1

    // It prints the quantitative value
    def print = quant
  }

  /**
   * @param _name name o f the label
   * @param _conf the confidentiality value for the label
   * @param _dim dimension in bit of the label
   */
  class Label(_name: String, _conf: Confidentiality, _dim: BitQuantity) {
    val name = _name //name of the label
    val conf = _conf //confidentiality value of the label
    val dim = _dim // dimension in bit of the label

    var oExpStm = ListBuffer[Statement]() // Over approximation of the statements applied to the label (explicit flow, not used at this time)
    var uExpStm = ListBuffer[Statement]() // Under approximation of the statements applied to the label (explicit flow, not used at this time)
    var oImplStm = ListBuffer[Statement]() // Over approximation of the statements applied to the label (implicit flow)
    var uImplStm = ListBuffer[Statement]() // Under approximation of the statements applied to the label (implicit flow)

    var oimplq = BitQuantity // Over approximation of the quantitative value released in the implicit flow
    var uimplq = BitQuantity // Under approximation of the quantitative value released in the implicit flow

    /**
     * Explicit flow, not used at this time
     * @param _stm a statement
     */
    def oExpListUpdater(_stm: Statement) {
      val stm = _stm
      this.oExpStm += stm
    }

    /**
     * Explicit flow, not used at this time
     * @param _stm a statement
     */
    def uExpListUpdater(_stm: Statement) {
      val stm = _stm
      this.uExpStm += stm
    }

    /**
     * @param _stm a statement
     */
    def oImplListUpdater(_stm: Statement) {
      val stm = _stm
      this.oImplStm += stm
    }

    /**
     * @param _stm a statement
     */
    def uImplListUpdater(_stm: Statement) {
      val stm = _stm
      this.oImplStm += stm
    }

    /**
     * Concrete Print function.
     * It prints the extended atomic data expression for the current label
     */
    def concretePrint = "<" + name + "{" + (this.oExpStm map print) + "}>"

    // explicit flow print function (concrete)
    // implicit flow print function (concrete)
    // implicit quantitative value print function (concrete)

    // explicit flow print function (abstract)
    // implicit flow print function (abstract)
    // implicit quantitative value print function (abstract)

    /**
     * Statement applied to the label
     * @FIXME: Does it must also have an associated label?
     * If so, it is sufficient the name of the associated label, or we want a link to the other label object instance?
     */
    class Statement(_name: String, _obf: Obfuscation, _implq: BitQuantity /*, associated label? _aLabel: Label*/ ) {
      val name = _name //name of the operator
      val obf = _obf // obfuscation power of the operator
      val implq = _implq // released bits
      //val aLabel = _aLabel //associated label, see @FIXME above

      // It prints the Statement operator or function, with the associated label, see @FIXME above
      def print = "(" + name + ", " /* + alabel.print */ + ")"
    }

    /**
     * Quantitative function (over approximation). It updates the implicit quantitative value each time the label is involved in an implicit flow.
     * @param _op Operation that produces implicit flow
     */
    def oImplQuantFunc(_stm: Statement) {
      //this.oimplq = this.oimplq + _stm.implq

      //this.oimplq.update()
      //@FIXME: update is not a member of BitQuantity, why?
    }

    /**
     * Quantitative function (under approximation).
     * @param _op Operation that produces implicit flow
     */
    def uImplQuantFunc(_stm: Statement) {
      //this.uimplq = this.uimplq + _stm.implq

      //this.uimplq.update()
      //@FIXME: update is not a member of BitQuantity, why?
    }
  }

}
