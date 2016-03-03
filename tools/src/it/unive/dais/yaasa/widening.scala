package it.unive.dais.yaasa

import it.unive.dais.yaasa.datatype.ABSValue.{ValueWithAbstraction, AbstractValue}
import it.unive.dais.yaasa.datatype.widening_lattice.{WideningLattice, WideningOp, WideningOpFactory}
import it.unive.dais.yaasa.utils.prelude.pretty


/**
  * Created by esteffin on 14/02/16.
  */
object widening {

  private[widening] class ThresholdWidening private (var threshold: Int = config.value.widening_threshold)
    extends WideningOp[ValueWithAbstraction] with pretty {
    def widening(l: ValueWithAbstraction, r: ValueWithAbstraction): ValueWithAbstraction = {
      //fixme: is l!=r correct?
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
  private[widening] object ThresholdWidening extends WideningOpFactory[ValueWithAbstraction] {
    def default: ThresholdWidening = new ThresholdWidening()
  }

  type WideningOperator = ThresholdWidening
  val  WideningOperator = ThresholdWidening

}
