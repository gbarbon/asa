package it.unive.dais.yaasa.absyn

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.utils.parsingUtils._
import scala.util.parsing.input.Positional

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

  override def pretty = ""
  override def prettyShort = ""
}

case class MethodDecl(_returnTy: Option[Type], _name: String, _formals: List[Formal], _body: Block)
    extends Node {
  val returnTy = _returnTy
  val name = _name
  val formals = _formals
  val body = _body

  override def pretty = ""
  override def prettyShort = ""
}

case class Formal(_ty: Type, _name: String)
    extends Node {
  val ty = _ty
  val name = _name

  override def pretty = ""
  override def prettyShort = ""
}

abstract class Type(_name: String)
    extends Node {
  val name = _name

}

case class TyInt()
    extends Type("Int") {

  override def pretty = ""
  override def prettyShort = ""
}

case class TyBool()
    extends Type("Bool") {

  override def pretty = ""
  override def prettyShort = ""
}

case class TyString()
    extends Type("String") {

  override def pretty = ""
  override def prettyShort = ""
}

case class TyType(_name: String)
    extends Type(_name) {

  override def pretty = ""
  override def prettyShort = ""
}

case class Block(_varDecls: List[VarDecl], _stmts: List[Stmt])
    extends Node {

  override def pretty = ""
  override def prettyShort = ""
}

case class VarDecl(_ty: Type, _id: String)
    extends Node {
  val ty = _ty
  val id = _id

  override def pretty = ""
  override def prettyShort = ""
}

trait Stmt extends Node

case class SSkip()
    extends Stmt {

  override def pretty = ""
  override def prettyShort = ""
}

case class SAssign(_name: String, _value: Expr)
    extends Stmt {
  val name = _name
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class SSetField(_fi: Field, _value: Expr)
    extends Stmt {
  val fi = _fi
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class SCall(_name: String, _actuals: List[Expr])
    extends Stmt {
  val name = _name
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class SMethodCall(_fi: Field, _actuals: List[Expr])
    extends Stmt {
  val fi = _fi
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class SReturn(_value: Option[Expr])
    extends Stmt {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class SIf(_cond: Expr, _thn: Block, _els: Block)
    extends Stmt {
  val cond = _cond
  val thn = _thn
  val els = _els

  override def pretty = ""
  override def prettyShort = ""
}

case class SWhile(_cond: Expr, _body: Block)
    extends Stmt {
  val cond = _cond
  val body = _body

  override def pretty = ""
  override def prettyShort = ""
}

// @FIXME: is "extends Node" correct?
case class Field(_expr: Expr, _name: String)
    extends Node {
  val expr = _expr
  val name = _name

  override def pretty = ""
  override def prettyShort = ""
}

trait Expr extends Node

case class EVariable(_name: String)
    extends Expr {
  val name = _name

  override def pretty = ""
  override def prettyShort = ""
}

case class EGetField(_value: Field)
    extends Expr {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class ECall(_name: String, _actuals: List[Expr])
    extends Expr {
  val name = _name
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class EMethodCall(_f: Field, _actuals: List[Expr])
    extends Expr {
  val f = _f
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class EThis()
    extends Expr {

  override def pretty = ""
  override def prettyShort = ""
}

case class ENew(_ty: String, _actuals: List[Expr])
    extends Expr {
  val ty = _ty
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class EBExpr(_op: String, _left: Expr, _right: Expr)
    extends Expr {
  val op = _op
  val left = _left
  val right = _right

  override def pretty = ""
  override def prettyShort = ""
}

case class EUExpr(_op: String, _value: Expr)
    extends Expr {
  val op = _op
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class ELit(_value: Literal)
    extends Expr {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

trait Literal extends Node

case class IntLit(_value: Int)
    extends Literal {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class BoolLit(_value: Boolean)
    extends Literal {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class StringLit(_value: String)
    extends Literal {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class NullLit()
    extends Literal {

  override def pretty = ""
  override def prettyShort = ""
}

