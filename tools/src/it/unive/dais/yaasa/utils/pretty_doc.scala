package it.unive.dais.yaasa.utils

import it.unive.dais.yaasa.utils.prelude._
import org.kiama.output.PrettyPrinter
import org.kiama.output.PrettyPrinter._

/**
  * Created by esteffin on 25/02/16.
  */
object pretty_doc {

  trait pretty_doc {
    def pretty_doc: Doc
  }

  class DocWrapper(d: Doc) extends pretty {
    def pretty: String = PrettyPrinter.pretty(d).layout
  }

  implicit def wrapDoc(d: Doc): DocWrapper = new DocWrapper(d)

  private def iterable_to_imm_seq[A](c: Iterable[A]): scala.collection.immutable.Seq[A] = {
    c.to[scala.collection.immutable.Seq]
  }

  def prettyList[A <: pretty_doc](l: Seq[A]) =
    if (l.isEmpty) text("[]")
    else brackets(folddoc(iterable_to_imm_seq(l.map{_.pretty_doc}), { (acc, e) => acc <> comma <+> e }))

  def prettySet[A <: pretty_doc](l: Set[A]): Doc =
    if (l.isEmpty) text("{}")
    else braces(folddoc(iterable_to_imm_seq(l.map{_.pretty_doc}), { (acc, e) => acc <> comma <+> e }))

  def prettyMap[A, B <: pretty_doc](m: Map[A, B]): Doc =
    if (m.isEmpty) text("[]")
    else brackets(folddoc(iterable_to_imm_seq(m.map{ case (k, v) => any(k) <+> text("->") <+> v.pretty_doc }), { (acc, e) => acc <> comma <%> e }))

  def chevrons = angles(_)


}
