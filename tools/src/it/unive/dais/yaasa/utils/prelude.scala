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

  trait pretty {
    def pretty: String
    override def toString = pretty
  }

  trait parsable[A] {
    def parse(s: String): A
  }

  trait Wrapper[A] {
    val cnt: A
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

  def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }

  def toBool(s: String): Option[Boolean] = {
    try {
      Some(s.toBoolean)
    } catch {
      case e: Exception => None
    }
  }
}
