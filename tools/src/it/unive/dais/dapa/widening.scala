package it.unive.dais.dapa

/**
  * @author esteffin
  * @author gbarbon
  */

import it.unive.dais.dapa.datatype.ABSValue.ValueWithAbstraction
import it.unive.dais.dapa.datatype.widening_lattice.{WideningOp, WideningOpFactory}
import it.unive.dais.dapa.utils.prelude.pretty

object widening {

  private[widening] class ThresholdWidening private (var threshold: Int = config.value.widening_threshold)
    extends WideningOp[ValueWithAbstraction] with pretty {
    def widening(l: ValueWithAbstraction, r: ValueWithAbstraction): ValueWithAbstraction = {
      if (l != r && threshold > 0) {
        threshold = threshold - 1
        l join r
      }
      else if (l != r && threshold <= 0) {
        l widening r
      }
      else
        l
    }

    override def pretty: String = "Threshold: %s" format threshold
  }
  private[widening] object ThresholdWidening extends WideningOpFactory[ValueWithAbstraction] {
    def default: ThresholdWidening = new ThresholdWidening()
  }

  type WideningOperator = ThresholdWidening
  val  WideningOperator = ThresholdWidening

}
