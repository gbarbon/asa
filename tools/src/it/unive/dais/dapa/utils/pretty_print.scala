package it.unive.dais.dapa.utils

/**
 * @author esteffin
 */

import prelude._

object pretty_print {

  class StringHelper(str: String) {
    def <|>(that: String) = "%s%s" format (str, that)
    def <+>(that: String) = "%s %s" format (str, that)
  }

  implicit def stringWrapper(string: String): StringHelper = new StringHelper(string)

  //type string = String
  //def $|$(that: String) = sprintf("%s%s")(this, that)
  def prettyList(sep: String)(l: Seq[Any]) =
    l.addString(new StringBuilder(), "[", sep, "]").toString()

  def prettySet[A](l: Set[A]) =
    if (l.isEmpty)
      "<emptySet>"
    else
      l.addString(new StringBuilder(), "{", ", ", "}").toString()

  def prettyMap[A, B](m: Map[A, B]) =
    if (m.isEmpty)
      "<emptyMap>"
    else
      m.map{ case (k, v) => "%s -> %s" format (k,v)}.addString(new StringBuilder(), "{", "\n ", "}").toString

  def xcat(sep: string)(l: Iterable[Any]) =
    l.addString(new StringBuilder(), "", sep, "").toString()

  def vcat(l: Iterable[Any]) =
    l.addString(new StringBuilder(), "", "\n", "").toString()

  def cat(l: Iterable[Any]) =
    l.addString(new StringBuilder(), "", "", "").toString()

  def sep(l: Iterable[Any]) =
    l.addString(new StringBuilder(), "", " ", "").toString()

  def parens(s: string) = s"($s)"

  def brakets(s: string) = s"[$s]"

  def braces(s: string) = s"{$s}"

  def chevrons(s: string) = s"<$s>"

}

