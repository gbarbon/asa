package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.utils.prelude._
import scala.util.parsing.input._

object absyn {

  type id = String

  /*trait Position extends Positional {
    override val pos: Position
  }*/

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

  case class MethodDecl(returnTy: Option[Type], name: String, formals: List[Formal], body: Block, annot: List[(String, String)])
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

  case class TyInt()
    extends Type("int")

  case class TyBool()
    extends Type("bool")

  case class TyString()
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

  case class SSkip()
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

  case class EThis()
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

  trait BOperator extends Node

  case class BOPlus()
      extends BOperator {

    override def pretty = "+"
    override def prettyShort = "+"
  }

  case class BOMinus()
      extends BOperator {

    override def pretty = "-"
    override def prettyShort = "-"
  }

  case class BOMul()
      extends BOperator {

    override def pretty = "*"
    override def prettyShort = "*"
  }

  case class BODiv()
      extends BOperator {

    override def pretty = "/"
    override def prettyShort = "/"
  }

  case class BOAnd()
      extends BOperator {

    override def pretty = "&&"
    override def prettyShort = "&&"
  }

  case class BOOr()
      extends BOperator {

    override def pretty = "||"
    override def prettyShort = "||"
  }

  case class BOMod()
      extends BOperator {

    override def pretty = "%"
    override def prettyShort = "%"
  }

  case class BOLt()
      extends BOperator {

    override def pretty = "<"
    override def prettyShort = "<"
  }

  case class BOLeq()
      extends BOperator {

    override def pretty = "<="
    override def prettyShort = "<="
  }

  case class BOEq()
      extends BOperator {

    override def pretty = "=="
    override def prettyShort = "=="
  }

  case class BOGt()
      extends BOperator {

    override def pretty = ">"
    override def prettyShort = ">"
  }

  case class BOGeq()
      extends BOperator {

    override def pretty = ">="
    override def prettyShort = ">="
  }

  case class BONeq()
      extends BOperator {

    override def pretty = "!="
    override def prettyShort = "!="
  }

  /**
   * String concatenation operator
   */
  case class BOPlusPlus()
      extends BOperator {

    override def pretty = "++"
    override def prettyShort = "++"
  }

  trait UOperator extends Node

  case class UNot()
      extends UOperator {

    override def pretty = "!"
    override def prettyShort = "!"
  }

  case class UNeg()
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

  case class NullLit()
      extends Literal {

    override def pretty = "null"
    override def prettyShort = "null"
  }
}
