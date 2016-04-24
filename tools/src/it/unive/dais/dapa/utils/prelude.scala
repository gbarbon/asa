package it.unive.dais.dapa.utils

/**
  * @author esteffin
  * @author gbarbon
  */

object prelude {
  type string = String

  class MessageException(val message: string) extends Exception {
    override def toString: string = message
  }

  class Unexpected(_message: string)
      extends MessageException("Unexpected failure: %s" format _message) {
  }

  class NotSupportedException(_message: String)
      extends MessageException("Feature not supported yet: %s" format _message) {
    def this(fmt: string, args: Any) = this(fmt format args)
  }

  trait pretty {
    def pretty: String
    override def toString = pretty
  }

  trait parsable[+A] {
    def parse(s: String): A
  }

  trait Wrapper[+A] {
    override def equals(o: Any) =
      o match {
        case that: Wrapper[A] => that.content == this.content
        case _ => false
      }
    val content: A
  }

  class OptionHelper[A](value: Option[A]) {
    def printDefault(default: String): String =
      value match {
        case Some(v) => v.toString
        case None        => default
      }

    def applyDefault[B](default: B)(f: (A => B)): B =
      value match {
        case Some(v) => f(v)
        case None        => default
      }
  }

  implicit def optionWrapper[A](value: Option[A]): OptionHelper[A] = new OptionHelper(value)

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

  def argmax[A](f: (A => Int), v1: A, v2: A): A = {
    if (f(v1) >= f(v2)) v1
    else v2
  }
}
