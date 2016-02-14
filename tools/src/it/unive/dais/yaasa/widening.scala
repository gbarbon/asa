package it.unive.dais.yaasa

import it.unive.dais.yaasa.datatype.ABSValue.AbstractValue
import it.unive.dais.yaasa.datatype.widening_lattice.{WideningLattice, WideningOp, WideningOpFactory}


/**
  * Created by esteffin on 14/02/16.
  */
object widening {

  private[widening] class ThresholdWidening private (var threshold: Int = config.value.widening_threshold) extends WideningOp[AbstractValue] {
    override def widening(l: AbstractValue, r: AbstractValue): AbstractValue = {
      if (l != r && threshold > 0) {
        threshold = threshold - 1
        l join r
      }
      else if (l != r && threshold <= 0) {
        //println("%s w %s ==> %s" format (l, r, l widening r))
        l widening r
      }
      else
        l
    }

    override def pretty: String = "Threshold: %s" format threshold
  }
  private[widening] object ThresholdWidening extends WideningOpFactory[AbstractValue] {
    override def default: ThresholdWidening = new ThresholdWidening()
  }

  type WideningOperator = ThresholdWidening
  val  WideningOperator = ThresholdWidening

}
