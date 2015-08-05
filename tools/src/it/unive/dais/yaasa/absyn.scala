package it.unive.dais.yaasa.absyn

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.utils.parsingUtils._

abstract class Node(loc: Location) {
  val location = loc

  /**
   * toString method
   * @return
   */
  def pretty: String
  def prettyShort: String
  //override def toString() = this.pretty()
}

case class Program(_classes: List[Class], _loc: Location)
    extends Node(_loc) {
  val classes = _classes

  override def pretty = "" + (classes map { _.pretty })
  override def prettyShort = "" + (classes map { _.prettyShort })
}

/**
 *
 */
case class Class(_name: String, _ext: String, _fields: List[FieldDecl], _methods: List[MethodDecl], _loc: Location)
    extends Node(_loc) {
  val name = _name
  val ext = _ext
  val fields = _fields
  val methods = _methods

  override def pretty = "class" + name + "extends" + ext + "{" + (fields map { _.pretty }) + (methods map { _.pretty }) + "}"
  override def prettyShort = "class" + name + "extends" + ext + "{ ... }"
}

case class FieldDecl(_ty: Type, _names: List[String], _loc: Location)
    extends Node(_loc) {
  val ty = _ty
  val names = _names

  override def pretty = ""
  override def prettyShort = ""
}

case class MethodDecl(_returnTy: Option[Type], _name: String, _formals: List[Formal], _body: Block, _loc: Location)
    extends Node(_loc) {
  val returnTy = _returnTy
  val name = _name
  val formals = _formals
  val body = _body

  override def pretty = ""
  override def prettyShort = ""
}

case class Formal(_ty: Type, _name: String, _loc: Location)
    extends Node(_loc) {
  val ty = _ty
  val name = _name

  override def pretty = ""
  override def prettyShort = ""
}

abstract class Type(_name: String, _loc: Location)
    extends Node(_loc) {
  val name = _name

}

case class TyInt(_loc: Location)
    extends Type("Int", _loc) {

  override def pretty = ""
  override def prettyShort = ""
}

case class TyBool(_loc: Location)
    extends Type("Bool", _loc) {

  override def pretty = ""
  override def prettyShort = ""
}

case class TyString(_loc: Location)
    extends Type("String", _loc) {

  override def pretty = ""
  override def prettyShort = ""
}

case class TyType(_name: String, _loc: Location)
    extends Type(_name, _loc) {

  override def pretty = ""
  override def prettyShort = ""
}

case class Block(_varDecls: List[VarDecl], _stmts: List[Stmt], _loc: Location)
    extends Node(_loc) {

  override def pretty = ""
  override def prettyShort = ""
}

case class VarDecl(_ty: Type, _id: String, _loc: Location)
    extends Node(_loc) {
  val ty = _ty
  val id = _id

  override def pretty = ""
  override def prettyShort = ""
}

abstract class Stmt(_loc: Location)
  extends Node(_loc) {}

case class SSkip(_loc: Location)
    extends Stmt(_loc) {

  override def pretty = ""
  override def prettyShort = ""
}

case class SAssign(_name: String, _value: Expr, _loc: Location)
    extends Stmt(_loc) {
  val name = _name
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class SSetField(_fi: Field, _value: Expr, _loc: Location)
    extends Stmt(_loc) {
  val fi = _fi
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class SCall(_name: String, _actuals: List[Expr], _loc: Location)
    extends Stmt(_loc) {
  val name = _name
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class SMethodCall(_fi: Field, _actuals: List[Expr], _loc: Location)
    extends Stmt(_loc) {
  val fi = _fi
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class SReturn(_value: Option[Expr], _loc: Location)
    extends Stmt(_loc) {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class SIf(_cond: Expr, _thn: Block, _els: Block, _loc: Location)
    extends Stmt(_loc) {
  val cond = _cond
  val thn = _thn
  val els = _els

  override def pretty = ""
  override def prettyShort = ""
}

case class SWhile(_cond: Expr, _body: Block, _loc: Location)
    extends Stmt(_loc) {
  val cond = _cond
  val body = _body

  override def pretty = ""
  override def prettyShort = ""
}

// @FIXME: is "extends Node" correct?
case class Field(_expr: Expr, _name: String, _loc: Location)
    extends Node(_loc) {
  val expr = _expr
  val name = _name

  override def pretty = ""
  override def prettyShort = ""
}

abstract class Expr(_loc: Location) extends Node(_loc) {

}

case class EVariable(_name: String, _loc: Location)
    extends Expr(_loc) {
  val name = _name

  override def pretty = ""
  override def prettyShort = ""
}

case class EGetField(_value: Field, _loc: Location)
    extends Expr(_loc) {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class ECall(_name: String, _actuals: List[Expr], _loc: Location)
    extends Expr(_loc) {
  val name = _name
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class EMethodCall(_f: Field, _actuals: List[Expr], _loc: Location)
    extends Expr(_loc) {
  val f = _f
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class EThis(_loc: Location)
    extends Expr(_loc) {

  override def pretty = ""
  override def prettyShort = ""
}

case class ENew(_ty: String, _actuals: List[Expr], _loc: Location)
    extends Expr(_loc) {
  val ty = _ty
  val actuals = _actuals

  override def pretty = ""
  override def prettyShort = ""
}

case class EBExpr(_op: String, _left: Expr, _right: Expr, _loc: Location)
    extends Expr(_loc) {
  val op = _op
  val left = _left
  val right = _right

  override def pretty = ""
  override def prettyShort = ""
}

case class EUExpr(_op: String, _value: Expr, _loc: Location)
    extends Expr(_loc) {
  val op = _op
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class ELit(_value: Literal, _loc: Location)
    extends Expr(_loc) {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

abstract class Literal(_loc: Location) extends Node(_loc) {}

case class IntLit(_value: Int, _loc: Location)
    extends Literal(_loc) {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class BoolLit(_value: Boolean, _loc: Location)
    extends Literal(_loc) {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class StringLit(_value: String, _loc: Location)
    extends Literal(_loc) {
  val value = _value

  override def pretty = ""
  override def prettyShort = ""
}

case class NullLit(_loc: Location)
    extends Literal(_loc) {

  override def pretty = ""
  override def prettyShort = ""
}

