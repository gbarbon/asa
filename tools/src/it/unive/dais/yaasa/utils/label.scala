package it.unive.dais.yaasa.utils

/**
 * @author gbarbon
 * @param _conf the confidentiality value for the label
 *
 */

object abstract_values {

  class label(_conf: String, _oquant: Int = 0) {

    val conf = _conf //confidentiality value of the label

    // Over approximation of the operations applied to the label (explicit flow)
    // Over approximation of the operations applied to the label (implicit flow)
    // Under approximation of the operations applied to the label (explicit flow)
    // Under approximation of the operations applied to the label (implicit flow)

    var oquant = _oquant // Over approximation of the quantitative value released in the implicit flow
    // Under approximation of the quantitative value released in the implicit flow

    def quantValUpdater(_update: Int) = this.oquant + _update

    // @FIXME _obf is temporary an Int
    // operation applied to the label
    class operation(_obf: Int) {

      // obfuscation power of the operator

    }
  }
}
