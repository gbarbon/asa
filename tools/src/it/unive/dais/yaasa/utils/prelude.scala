package it.unive.dais.yaasa.utils

/**
 * @author esteffin
 */

object prelude {
  type string = String

  trait MessageException extends Exception {
    val message: string
    override def toString(): string = message
  }

  class Unexpected(_message: string) extends MessageException {
    val message: String = "Unexpected failure: %s" format _message
    def this(fmt: string, args: Any) = this(fmt format args)
  }

  class NotSupportedException(_message: String) extends MessageException {
    val message: String = "Feature not supported yet: %s" format _message
    def this(fmt: string, args: Any) = this(fmt format args)
  }

  class OptionHelper[A](value: Option[A]) {
    def printDefault(default: String): String =
      value match {
        case Some(value) => value.toString()
        case None        => default
      }

    def applyDefault[B](default: B)(f: (A => B)): B =
      value match {
        case Some(value) => f(value)
        case None        => default
      }
  }

  implicit def optionWrapper[A](value: Option[A]) = new OptionHelper(value)

  /*def sprintf(fmt: String)(args: Any): String =
    return fmt format args*/

  def printfn(fmt: String): Unit =
    printf("%s\n", fmt)
}
