package it.unive.dais.yaasa.utils

/**
 * @author esteffin
 */

object prelude {
  type string = String

  class MessageException(val message: string) extends Exception {
    override def toString(): string = message
  }

  class Unexpected(_message: string)
      extends MessageException("Unexpected failure: %s" format _message) {
    def this(fmt: string, args: Any) = this(fmt format args)
  }

  class NotSupportedException(_message: String)
      extends MessageException("Feature not supported yet: %s" format _message) {
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

  def min(l: Int, r: Int): Int =
    if (l <= r) l
    else r

  def max(l: Int, r: Int): Int =
    if (l >= r) l
    else r
}
