package it.unive.dais.yaasa.utils

/**
 * @author esteffin
 */

import prelude._

object pretty_print {

  class StringHelper(str: String) {
    def <|>(that: String) = "%s%s" format (str, that)
    def <+>(that: String) = "%s %s" format (str, that)
  }

  implicit def stringWrapper(string: String) = new StringHelper(string)

  //type string = String
  //def $|$(that: String) = sprintf("%s%s")(this, that)
  def xhcat(sep: String)(l: Seq[Any]) =
    l.addString(new StringBuilder(), "[", sep, "]").toString()
  def xvcat(sep: string)(l: Iterable[Any]) =
    l.addString(new StringBuilder(), "[", sep + "\n", "]").toString()
  def parens(s: string) = s"($s)"
  def brakets(s: string) = s"[$s]"
  def braces(s: string) = s"{$s}"
  def chevrons(s: string) = s"<$s>"
}
