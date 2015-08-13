package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.utils._
import scala.util.parsing.input.Positional

object absyn {

  type id = String

  trait Node extends Positional {

    /**
     * toString method
     * @return
     */
    def pretty: String
    def prettyShort: String
    //override def toString() = this.pretty()
  }

  case class Program(_classes: List[Class])
      extends Node {
    val classes = _classes

    override def pretty = "" + (classes map { _.pretty })
    override def prettyShort = "" + (classes map { _.prettyShort })
  }

  /**
   *
   */
  case class Class(_name: String, _ext: String, _fields: List[FieldDecl], _methods: List[MethodDecl])
      extends Node {
    val name = _name
    val ext = _ext
    val fields = _fields
    val methods = _methods

    override def pretty = "class" + name + "extends" + ext + "{" + (fields map { _.pretty }) + (methods map { _.pretty }) + "}"
    override def prettyShort = "class" + name + "extends" + ext + "{ ... }"
  }

  case class FieldDecl(_ty: Type, _names: List[String])
      extends Node {
    val ty = _ty
    val names = _names

    override def pretty = ty + " " + (names.fold("")({ (acc, f) => acc + ", " + f })) + ";"
    override def prettyShort = ty + " " + (names.fold("")({ (acc, f) => acc + ", " + f })) + ";"
  }

  case class MethodDecl(_returnTy: Option[Type], _name: String, _formals: List[Formal], _body: Block)
      extends Node {
    val returnTy = _returnTy
    val name = _name
    val formals = _formals
    val body = _body

    override def pretty =
      prelude.applyDefault({ ty: Type => ty.pretty })("void")(returnTy) + " " + name +
        "(" + ((formals.foldLeft("") { (acc, form) => acc + ", " + form.pretty })) + body.pretty
    override def prettyShort =
      prelude.applyDefault({ ty: Type => ty.prettyShort })("void")(returnTy) + " " + name +
        "(" + ((formals.foldLeft("") { (acc, form) => acc + ", " + form.prettyShort })) + body.prettyShort
  }

  case class Formal(_ty: Type, _name: String)
      extends Node {
    val ty = _ty
    val name = _name

    override def pretty = ty + " " + name + ";"
    override def prettyShort = ty + " " + name + ";"
  }

  abstract class Type(_name: String)
      extends Node {
    //val name = _name

  }

  case class TyInt()
      extends Type("Int") {

    override def pretty = "int"
    override def prettyShort = "int"
  }

  case class TyBool()
      extends Type("Bool") {

    override def pretty = "bool"
    override def prettyShort = "bool"
  }

  case class TyString()
      extends Type("String") {

    override def pretty = "string"
    override def prettyShort = "string"
  }

  case class TyType(_name: String)
      extends Type(_name) {

    val name = _name

    override def pretty = name
    override def prettyShort = name
  }

  case class Block(_varDecls: List[VarDecl], _stmts: List[Stmt])
      extends Node {
    val varDecls = _varDecls
    val stmts = _stmts

    override def pretty = (varDecls.foldLeft("") { (acc, vd) => acc + vd.pretty }) +
      (stmts.foldLeft("") { (acc, stmt) => acc + stmt.pretty })
    override def prettyShort = (varDecls.foldLeft("") { (acc, vd) => acc + vd.prettyShort }) +
      (stmts.foldLeft("") { (acc, stmt) => acc + stmt.prettyShort })
  }

  case class VarDecl(_ty: Type, _id: String)
      extends Node {
    val ty = _ty
    val id = _id

    override def pretty = ty + " " + id + ";\n"
    override def prettyShort = ty + " " + id + ";\n"
  }

  trait Stmt extends Node

  case class SSkip()
      extends Stmt {

    override def pretty = "skip;\n"
    override def prettyShort = "skip;\n"
  }

  case class SAssign(_name: String, _value: Expr)
      extends Stmt {
    val name = _name
    val value = _value

    override def pretty = name + " = " + value.pretty + ";\n"
    override def prettyShort = name + " = " + "..." + ";\n"
  }

  case class SSetField(_fi: Field, _value: Expr)
      extends Stmt {
    val fi = _fi
    val value = _value

    override def pretty = fi.pretty + " = " + value.pretty + ";\n"
    override def prettyShort = fi.prettyShort + " = " + value.prettyShort + ";\n"
  }

  case class SCall(_name: String, _actuals: List[Expr])
      extends Stmt {
    val name = _name
    val actuals = _actuals

    override def pretty = name + "(" + (actuals map (_.pretty)) + ");\n"
    override def prettyShort = name + "(" + (actuals map (_.prettyShort)) + ");\n"
  }

  case class SMethodCall(_fi: Field, _actuals: List[Expr])
      extends Stmt {
    val fi = _fi
    val actuals = _actuals

    override def pretty = fi.pretty + "(" + (actuals map (_.pretty)) + ");\n"
    override def prettyShort = fi.prettyShort + "(" + (actuals map (_.prettyShort)) + ");\n"
  }

  case class SReturn(_value: Option[Expr])
      extends Stmt {
    val value = _value

    override def pretty =
      "return " + prelude.applyDefault({ ty: Expr => ty.pretty })("")(value) + "\n"
    override def prettyShort = "return " + prelude.applyDefault({ ty: Expr => ty.prettyShort })("")(value) + "\n"
  }

  case class SIf(_cond: Expr, _thn: Block, _els: Block)
      extends Stmt {
    val cond = _cond
    val thn = _thn
    val els = _els

    override def pretty = "if (" + cond.pretty + ") then {\n" + thn.pretty + "}\nelse {\n" + els.pretty + "}\n"
    override def prettyShort = "if (" + cond.prettyShort + ") then {\n" + thn.prettyShort + "}\nelse {\n" + els.prettyShort + "}\n"
  }

  case class SWhile(_cond: Expr, _body: Block)
      extends Stmt {
    val cond = _cond
    val body = _body

    override def pretty = "while (" + cond.pretty + ") {\n" + body.pretty + "}\n"
    override def prettyShort = "while (" + cond.prettyShort + ") {\n" + body.prettyShort + "}\n"
  }

  // @FIXME: is "extends Node" correct?
  case class Field(_expr: Expr, _name: String)
      extends Node {
    val expr = _expr
    val name = _name

    override def pretty = expr.pretty + "." + name
    override def prettyShort = expr.prettyShort + "." + name
  }

  trait Expr extends Node

  case class EVariable(_name: String)
      extends Expr {
    val name = _name

    override def pretty = name
    override def prettyShort = name
  }

  case class EGetField(_value: Field)
      extends Expr {
    val value = _value

    override def pretty = value.pretty
    override def prettyShort = value.prettyShort
  }

  case class ECall(_name: String, _actuals: List[Expr])
      extends Expr {
    val name = _name
    val actuals = _actuals

    override def pretty = name + "(" + (actuals map (_.pretty)) + ")\n"
    override def prettyShort = name + "(" + (actuals map (_.prettyShort)) + ")\n"
  }

  case class EMethodCall(_f: Field, _actuals: List[Expr])
      extends Expr {
    val f = _f
    val actuals = _actuals

    override def pretty = f.pretty + "(" + (actuals map (_.pretty)) + ")"
    override def prettyShort = f.prettyShort + "(" + (actuals map (_.prettyShort)) + ")"
  }

  case class EThis()
      extends Expr {

    override def pretty = "this"
    override def prettyShort = "this"
  }

  case class ENew(_ty: String, _actuals: List[Expr])
      extends Expr {
    val ty = _ty
    val actuals = _actuals

    override def pretty = "new " + ty + " (" + (actuals map (_.pretty)) + ")"
    override def prettyShort = "new " + ty + " (" + (actuals map (_.prettyShort)) + ")"
  }

  case class EBExpr(_op: BOperator, _left: Expr, _right: Expr)
      extends Expr {
    val op = _op
    val left = _left
    val right = _right

    override def pretty = left.pretty + " " + op + " " + right.pretty
    override def prettyShort = left.prettyShort + " " + op + " " + right.prettyShort
  }

  case class EUExpr(_op: UOperator, _value: Expr)
      extends Expr {
    val op = _op
    val value = _value

    override def pretty = op + " " + value.pretty
    override def prettyShort = op + " " + value.prettyShort
  }

  case class ELit(_value: Literal)
      extends Expr {
    val value = _value

    override def pretty = value.pretty
    override def prettyShort = value.prettyShort
  }

  trait BOperator extends Node

  case class Plus(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "+"
    override def prettyShort = "+"
  }

  case class Minus(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "-"
    override def prettyShort = "-"
  }

  case class Times(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "*"
    override def prettyShort = "*"
  }

  case class Factor(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "/"
    override def prettyShort = "/"
  }

  case class And(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "&&"
    override def prettyShort = "&&"
  }

  case class Or(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "||"
    override def prettyShort = "||"
  }

  case class Modulo(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "%"
    override def prettyShort = "%"
  }

  case class LessThan(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "<"
    override def prettyShort = "<"
  }

  case class LessEqualThan(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "<="
    override def prettyShort = "<="
  }

  case class Equal(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "=="
    override def prettyShort = "=="
  }

  case class HigherThan(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = ">"
    override def prettyShort = ">"
  }

  case class HigherEqualThan(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = ">="
    override def prettyShort = ">="
  }

  case class NotEqual(_op: String)
      extends BOperator {
    val op = _op

    override def pretty = "!="
    override def prettyShort = "!="
  }

  trait UOperator extends Node

  case class Not(_op: String)
      extends UOperator {
    val op = _op

    override def pretty = "!"
    override def prettyShort = "!"
  }

  case class Negative(_op: String)
      extends UOperator {
    val op = _op

    override def pretty = "-"
    override def prettyShort = "-"
  }

  trait Literal extends Node

  case class IntLit(_value: Int)
      extends Literal {
    val value = _value

    override def pretty = value.toString
    override def prettyShort = value.toString
  }

  case class BoolLit(_value: Boolean)
      extends Literal {
    val value = _value

    override def pretty = value.toString
    override def prettyShort = value.toString
  }

  case class StringLit(_value: String)
      extends Literal {
    val value = _value

    override def pretty = value
    override def prettyShort = value
  }

  case class NullLit()
      extends Literal {

    override def pretty = "null"
    override def prettyShort = "null"
  }
}
