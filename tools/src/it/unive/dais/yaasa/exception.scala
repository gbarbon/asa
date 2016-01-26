package it.unive.dais.yaasa

import it.unive.dais.yaasa.utils.prelude.{string, MessageException}

/**
  * Created by esteffin on 19/01/16.
  */
object exception {
  case class EvaluationException(_message: string) extends MessageException("Evaluation exception: %s" format _message) {
    /*def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))*/
  }
  case class AbsValuesMismatch(_message: string) extends MessageException("AbstractValue exception: %s" format _message) {

  }
}
