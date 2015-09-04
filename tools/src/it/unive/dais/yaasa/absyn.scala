package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.utils.prelude._
import scala.util.parsing.input._
import it.unive.dais.yaasa.abstract_values._

object absyn {

  type id = String

  /*trait Position extends Positional {
    override val pos: Position
  }*/

  trait Annot
  case class LabelAnnot[A](name: String,
                           confidentiality: Lattice[A],
                           dimension: BitQuantity,
                           molteplicity: Int = 1) extends Annot {
    //@FIXME: annotations not printed
    def pretty = ""
    override def toString() = pretty
  }

  object LabelAnnot {
    def parse(strings: Map[String, String]) =
      {
        val name = strings("labelName")
        val conf = LMH.LMHFactory.parse(strings("conf"))
        val dim = new BitQuantity(strings("dim") toInt)
        if (strings contains "molt")
          LabelAnnot(name, conf, dim, strings("molt") toInt)
        else
          LabelAnnot(name, conf, dim)
      }
  }

  case class FunAnnot(name: String,
                      obfuscation: (List[LMH] => LMH),
                      quantity: BitQuantity) extends Annot {
    //@FIXME: annotations not printed
    def pretty = ""
    override def toString() = pretty
  }

  object FunAnnot {
    def parse(strings: Map[String, String]) =
      {
        val name = strings("name")
        val init_c = LMH.LMHFactory.parse(strings("obf"))
        val obf = { l: List[LMH] => init_c }
        val dim = new BitQuantity(strings("implq") toInt)
        FunAnnot(name, obf, dim)
      }
  }

  trait Node extends Positional {

    /**
     * toString method
     * @return
     */
    def pretty: String
    def prettyShort: String

    def loc = (this.pos.line, this.pos.column)
    //override def toString() = this.pretty()
  }

  case class Program(classes: List[Class])
      extends Node {

    override def pretty = "" + (classes map { _.pretty })
    override def prettyShort = "" + (classes map { _.prettyShort })
  }

  /**
   *
   */
  case class Class(name: String, ext: Option[String], fields: List[FieldDecl], methods: List[MethodDecl])
      extends Node {

    override def pretty =
      ext match {
        case Some(ext) => "class" + name + "extends" + ext + "{" + (fields map { _.pretty }) + (methods map { _.pretty }) + "}"
        case None      => "class" + name + "{" + (fields map { _.pretty }) + (methods map { _.pretty }) + "}"
      }
    override def prettyShort =
      ext match {
        case Some(ext) => "class" + name + "extends" + ext + "{ ... }"
        case None      => "class" + name + "{ ... }"
      }
  }

  case class FieldDecl(ty: Type, names: List[String])
      extends Node {

    override def pretty = ty + " " + (names.fold("")({ (acc, f) => acc + ", " + f })) + ";"
    override def prettyShort = ty + " " + (names.fold("")({ (acc, f) => acc + ", " + f })) + ";"
  }

  case class MethodDecl(returnTy: Option[Type], name: String, formals: List[Formal], body: Block, annot: Option[Annot])
      extends Node {

    //@FIXME: annotations not printed
    override def pretty =
      returnTy.applyDefault("void") { ty: Type => ty.pretty } + " " + name +
        "(" + ((formals.foldLeft("") { (acc, form) => acc + ", " + form.pretty })) + body.pretty
    override def prettyShort =
      returnTy.applyDefault("void") { ty: Type => ty.prettyShort } + " " + name +
        "(" + ((formals.foldLeft("") { (acc, form) => acc + ", " + form.prettyShort })) + body.prettyShort
  }

  case class Formal(ty: Type, name: String)
      extends Node {

    override def pretty = ty + " " + name + ";"
    override def prettyShort = ty + " " + name + ";"
  }

  abstract class Type(name: String)
      extends Node {

    override def pretty = name
    override def prettyShort = name
  }

  case object TyInt
    extends Type("int")

  case object TyBool
    extends Type("bool")

  case object TyString
    extends Type("string")

  case class TyType(name: String)
    extends Type(name)

  case class Block(varDecls: List[VarDecl], stmts: List[Stmt])
      extends Node {

    override def pretty = (varDecls.foldLeft("") { (acc, vd) => acc + vd.pretty }) +
      (stmts.foldLeft("") { (acc, stmt) => acc + stmt.pretty })
    override def prettyShort = (varDecls.foldLeft("") { (acc, vd) => acc + vd.prettyShort }) +
      (stmts.foldLeft("") { (acc, stmt) => acc + stmt.prettyShort })
  }

  case class VarDecl(ty: Type, ids: List[String])
      extends Node {

    /*override def pretty = ty + " " + id + ";\n"
    override def prettyShort = ty + " " + id + ";\n"*/
    override def pretty = ty + " " + (ids.fold("")({ (acc, f) => acc + ", " + f })) + ";"
    override def prettyShort = ty + " " + (ids.fold("")({ (acc, f) => acc + ", " + f })) + ";"
  }

  trait Stmt extends Node

  case object SSkip
      extends Stmt {

    override def pretty = "skip;\n"
    override def prettyShort = "skip;\n"
  }

  case class SAssign(name: String, value: Expr)
      extends Stmt {

    override def pretty = name + " = " + value.pretty + ";\n"
    override def prettyShort = name + " = " + "..." + ";\n"
  }

  case class SSetField(fi: Field, value: Expr)
      extends Stmt {

    override def pretty = fi.pretty + " = " + value.pretty + ";\n"
    override def prettyShort = fi.prettyShort + " = " + value.prettyShort + ";\n"
  }

  case class SCall(name: String, actuals: List[Expr])
      extends Stmt {

    override def pretty = name + "(" + (actuals map (_.pretty)) + ");\n"
    override def prettyShort = name + "(" + (actuals map (_.prettyShort)) + ");\n"
  }

  case class SPrint(newLine: Boolean, actual: Expr)
      extends Stmt {
    val name = if (newLine) "println" else "print"

    override def pretty = name + "(" + actual.pretty + ");\n"
    override def prettyShort = name + "(" + actual.prettyShort + ");\n"
  }

  case class SMethodCall(fi: Field, actuals: List[Expr])
      extends Stmt {

    override def pretty = fi.pretty + "(" + (actuals map (_.pretty)) + ");\n"
    override def prettyShort = fi.prettyShort + "(" + (actuals map (_.prettyShort)) + ");\n"
  }

  case class SReturn(value: Option[Expr])
      extends Stmt {

    override def pretty =
      "return " + value.applyDefault("") { ty: Expr => ty.pretty } + "\n"
    override def prettyShort = "return " + value.applyDefault("") { ty: Expr => ty.prettyShort } + "\n"
  }

  case class SIf(cond: Expr, thn: Stmt, els: Stmt)
      extends Stmt {

    override def pretty = "if (" + cond.pretty + ") then {\n" + thn.pretty + "}\nelse {\n" + els.pretty + "}\n"
    override def prettyShort = "if (" + cond.prettyShort + ") then {\n" + thn.prettyShort + "}\nelse {\n" + els.prettyShort + "}\n"
  }

  case class SWhile(cond: Expr, body: Stmt)
      extends Stmt {

    override def pretty = "while (" + cond.pretty + ") {\n" + body.pretty + "}\n"
    override def prettyShort = "while (" + cond.prettyShort + ") {\n" + body.prettyShort + "}\n"
  }

  case class SBlock(block: Block)
      extends Stmt {

    override def pretty = block.pretty
    override def prettyShort = block.prettyShort
  }

  // @FIXME: is "extends Node" correct?
  case class Field(expr: Expr, name: String)
      extends Node {

    override def pretty = expr.pretty + "." + name
    override def prettyShort = expr.prettyShort + "." + name
  }

  trait Expr extends Node

  case class EVariable(name: String)
      extends Expr {

    override def pretty = name
    override def prettyShort = name
  }

  case class EGetField(value: Field)
      extends Expr {

    override def pretty = value.pretty
    override def prettyShort = value.prettyShort
  }

  case class ECall(name: String, actuals: List[Expr])
      extends Expr {

    override def pretty = name + "(" + (actuals map (_.pretty)) + ")\n"
    override def prettyShort = name + "(" + (actuals map (_.prettyShort)) + ")\n"
  }

  case class EMethodCall(f: Field, actuals: List[Expr])
      extends Expr {

    override def pretty = f.pretty + "(" + (actuals map (_.pretty)) + ")"
    override def prettyShort = f.prettyShort + "(" + (actuals map (_.prettyShort)) + ")"
  }

  case object EThis
      extends Expr {

    override def pretty = "this"
    override def prettyShort = "this"
  }

  case class ENew(ty: String, actuals: List[Expr])
      extends Expr {

    override def pretty = "new " + ty + " (" + (actuals map (_.pretty)) + ")"
    override def prettyShort = "new " + ty + " (" + (actuals map (_.prettyShort)) + ")"
  }

  case class EBExpr(op: BOperator, left: Expr, right: Expr)
      extends Expr {

    override def pretty = left.pretty + " " + op + " " + right.pretty
    override def prettyShort = left.prettyShort + " " + op + " " + right.prettyShort
  }

  case class EUExpr(op: UOperator, value: Expr)
      extends Expr {

    override def pretty = op + " " + value.pretty
    override def prettyShort = op + " " + value.prettyShort
  }

  case class ELit(value: Literal)
      extends Expr {

    override def pretty = value.pretty
    override def prettyShort = value.prettyShort
  }

  trait BOperator extends Node {
    val annot: FunAnnot
  }

  case class BOPlus(annot: FunAnnot)
      extends BOperator {

    override def pretty = "+"
    override def prettyShort = "+"
  }

  case class BOMinus(annot: FunAnnot)
      extends BOperator {

    override def pretty = "-"
    override def prettyShort = "-"
  }

  case class BOMul(annot: FunAnnot)
      extends BOperator {

    override def pretty = "*"
    override def prettyShort = "*"
  }

  case class BODiv(annot: FunAnnot)
      extends BOperator {

    override def pretty = "/"
    override def prettyShort = "/"
  }

  case class BOAnd(annot: FunAnnot)
      extends BOperator {

    override def pretty = "&&"
    override def prettyShort = "&&"
  }

  case class BOOr(annot: FunAnnot)
      extends BOperator {

    override def pretty = "||"
    override def prettyShort = "||"
  }

  case class BOMod(annot: FunAnnot)
      extends BOperator {

    override def pretty = "%"
    override def prettyShort = "%"
  }

  case class BOLt(annot: FunAnnot)
      extends BOperator {

    override def pretty = "<"
    override def prettyShort = "<"
  }

  case class BOLeq(annot: FunAnnot)
      extends BOperator {

    override def pretty = "<="
    override def prettyShort = "<="
  }

  case class BOEq(annot: FunAnnot)
      extends BOperator {

    override def pretty = "=="
    override def prettyShort = "=="
  }

  case class BOGt(annot: FunAnnot)
      extends BOperator {

    override def pretty = ">"
    override def prettyShort = ">"
  }

  case class BOGeq(annot: FunAnnot)
      extends BOperator {

    override def pretty = ">="
    override def prettyShort = ">="
  }

  case class BONeq(annot: FunAnnot)
      extends BOperator {

    override def pretty = "!="
    override def prettyShort = "!="
  }

  /**
   * String concatenation operator
   */
  case class BOPlusPlus(annot: FunAnnot)
      extends BOperator {

    override def pretty = "++"
    override def prettyShort = "++"
  }

  trait UOperator extends Node {
    val annot: FunAnnot
  }

  case class UNot(annot: FunAnnot)
      extends UOperator {

    override def pretty = "!"
    override def prettyShort = "!"
  }

  case class UNeg(annot: FunAnnot)
      extends UOperator {

    override def pretty = "-"
    override def prettyShort = "-"
  }

  trait Literal extends Node

  case class IntLit(value: Int)
      extends Literal {

    override def pretty = value.toString
    override def prettyShort = value.toString
  }

  case class BoolLit(value: Boolean)
      extends Literal {

    override def pretty = value.toString
    override def prettyShort = value.toString
  }

  case class StringLit(value: String)
      extends Literal {

    override def pretty = value
    override def prettyShort = value
  }

  case object NullLit
      extends Literal {

    override def pretty = "null"
    override def prettyShort = "null"
  }
}
