package it.unive.dais.dapa

import it.unive.dais.dapa.utils.prelude.{MessageException, string}

object exception {
  case class EvaluationException(_message: string) extends MessageException("Evaluation exception: %s" format _message)
  case class TypeMismatchException(_message: string) extends MessageException("Type mismatch exception: %s" format _message)
  case class AbsValuesMismatch(_message: string) extends MessageException("AbstractValue exception: %s" format _message)
  case class WrongUpdateClass(_message: string) extends MessageException("Update Class exception: %s" format _message)
}
