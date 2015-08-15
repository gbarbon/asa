package it.unive.dais.yaasa.utils

/**
 * @author gbarbon
 *
 */

object abstract_values {

  /**
   * @param _conf the confidentiality value for the label
   */
  class Label(_name: String, _conf: String, _oimplq: Int = 0, _uimplq: Int = 0) {
    val name = _name //name of the label
    val conf = _conf //confidentiality value of the label

    def list: List[Statement] = List[Statement]() // Over approximation of the operations applied to the label (explicit flow)
    // Over approximation of the operations applied to the label (implicit flow)
    // Under approximation of the operations applied to the label (explicit flow)
    // Under approximation of the operations applied to the label (implicit flow)

    var oimplq = _oimplq // Over approximation of the quantitative value released in the implicit flow
    var uimplq = _uimplq // Under approximation of the quantitative value released in the implicit flow

    /**
     * Statement applied to the label
     * @FIXME: Does it must also have an associated label?
     * If so, it is sufficient the name of the associated label, or we want a link to the other label object instance?
     */
    class Statement(_name: String, _obf: Double, _implq: Int /*, associated label?*/ ) {
      def name = _name //name of the operator
      def obf = _obf // obfuscation power of the operator
      def implq = _implq // released bits

      //def aslabel //associated label, see @FIXME above

    }

    /**
     * Quantitative function. It updates the implicit quantitative value each time the label is involved in an implicit flow.
     * @param _op Operation that produces implicit flow
     */
    def implQuantFunc(_stm: Statement) {
      this.oimplq = this.oimplq + _stm.implq
    }
  }
}
