package it.unive.dais.yaasa

import it.unive.dais.yaasa.datatype.ABSValue.AbstractValue
import it.unive.dais.yaasa.datatype.FortyTwo.ValueWithAbstraction
import it.unive.dais.yaasa.datatype.widening_lattice.{WideningLattice, WideningOp, WideningOpFactory}
import it.unive.dais.yaasa.utils.prelude.pretty


/**
  * Created by esteffin on 14/02/16.
  */
object widening {

  private[widening] class ThresholdWidening private (var threshold: Int = config.value.widening_threshold) extends pretty /*extends WideningOp[ValueWithAbstraction] */ {
    def widening(l: ValueWithAbstraction, r: ValueWithAbstraction): ValueWithAbstraction = {
      //fixme: is l!=r correct?
      if (l != r && threshold > 0) {
        threshold = threshold - 1
        ValueWithAbstraction(l.value join r.value, l.adInfo join r.adInfo)
      }
      else if (l != r && threshold <= 0) {
        //println("%s w %s ==> %s" format (l, r, l widening r))
        ValueWithAbstraction(l.value widening r.value, l.adInfo widening r.adInfo)
      }
      else
        l
    }

    override def pretty: String = "Threshold: %s" format threshold
  }
  private[widening] object ThresholdWidening /*extends WideningOpFactory[AbstractValue]*/ {
    def default: ThresholdWidening = new ThresholdWidening()
  }

  type WideningOperator = ThresholdWidening
  val  WideningOperator = ThresholdWidening

}
