package it.unive.dais.yaasa.utils

/**
 * @author esteffin
 */

object prelude {
  def printDefault(obj: Option[Any], default: String): String =
    obj match {
      case Some(value) => value.toString()
      case None        => default
    }

  def applyDefault[A, B](f: (A => B), obj: Option[A], default: B): B =
    obj match {
      case Some(value) => f(value)
      case None        => default
    }
}
