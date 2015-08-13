package it.unive.dais.yaasa.utils

/**
 * @author esteffin
 */

object prelude {
  type string = String

  class Unexpected(_message: string) extends Exception {
    val message: String = "Unexpected failure: %s" format _message
    def this(fmt: string, args: Any) = this(fmt format args)
    override def toString(): string = message
  }

  class NotSupportedException(_message: String) extends Exception {
    val message: String = "Feature not supported yet: %s" format _message
    def this(fmt: string, args: Any) = this(fmt format args)
    override def toString(): String = message
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

  def sprintf(fmt: String)(args: Any): String =
    return fmt format args

  def printfn(fmt: String)(args: Any): Unit =
    printf("%s\n", (sprintf(fmt)(args)))
}
