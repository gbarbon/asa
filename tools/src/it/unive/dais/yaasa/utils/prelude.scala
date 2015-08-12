package it.unive.dais.yaasa.utils

/**
 * @author esteffin
 */

object prelude {
  type string = String

  class Unexpected(fmt: string, args: Any) extends Exception {
    val message: String = "Unexpected failure: %s" format (fmt format args)
    override def toString(): string = message
  }

  def printDefault(default: String)(obj: Option[Any]): String =
    obj match {
      case Some(value) => value.toString()
      case None        => default
    }

  def applyDefault[A, B](f: (A => B))(default: B)(obj: Option[A]): B =
    obj match {
      case Some(value) => f(value)
      case None        => default
    }

  def sprintf(fmt: String)(args: Any): String =
    return fmt format args

  def printfn(fmt: String)(args: Any): Unit =
    printf("%s\n", (sprintf(fmt)(args)))
}
