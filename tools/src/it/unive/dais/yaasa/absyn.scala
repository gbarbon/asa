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
  //override def toString() = this.pretty()
}

/**
 *
 */
case class Class(_name: String, _ext: String, _fields: List[FieldDecl], _methods: List[MethodDecl], _loc: Location)
    extends Node(_loc) {
  val name = _name
  val ext = _ext
  val fields = _fields

  override def pretty = ""
}

case class FieldDecl(_ty: Type, _names: List[String], _loc: Location)
    extends Node(_loc) {
  val ty = _ty
  val names = _names

  override def pretty = ""
}

case class MethodDecl(_returnTy: Option[Type], _name: String, _formals: Option[List[Formal]], _body: Block, _loc: Location)
    extends Node(_loc) {
  val returnTy = _returnTy
  val name = _name
  val formals = _formals
  val body = _body

  override def pretty = ""
}

case class Formal(_ty: Type, _name: String, _loc: Location)
    extends Node(_loc) {
  val ty = _ty
  val name = _name

  override def pretty = ""
}

abstract class Type(_name: String, _loc: Location)
    extends Node(_loc) {
  val name = _name

}

case class TyInt(_loc: Location)
    extends Type("Int", _loc) {

  override def pretty = ""
}

case class TyBool(_loc: Location)
    extends Type("Bool", _loc) {

  override def pretty = ""
}

case class TyString(_loc: Location)
    extends Type("String", _loc) {

  override def pretty = ""
}

case class TyType(_name: String, _loc: Location)
    extends Type(_name, _loc) {

  override def pretty = ""
}

case class Block(_varDecls: List[VarDecl], _stmts: List[Stmt], _loc: Location)
    extends Node(_loc) {

  override def pretty = ""
}

case class VarDecl(_ty: Type, _id: String, _loc: Location)
    extends Node(_loc) {
  val ty = _ty
  val id = _id

  override def pretty = ""
}

abstract class Stmt(_loc: Location)
  extends Node(_loc) {}

case class Skip(_loc: Location)
    extends Stmt(_loc) {

  override def pretty = ""
}

case class Assign(_name: String, _value: Expr, _loc: Location)
    extends Stmt(_loc) {
  val name = _name
  val value = _value

  override def pretty = ""
}

case class SetField(_fi: Field, _value: Expr, _loc: Location)
    extends Stmt(_loc) {
  val fi = _fi
  val value = _value

  override def pretty = ""
}

case class StmtCall(_name: String, _actuals: List[Expr], _loc: Location)
    extends Stmt(_loc) {
  val name = _name
  val actuals = _actuals

  override def pretty = ""
}

case class MethodCall(_fi: Field, _actuals: List[Expr], _loc: Location)
    extends Stmt(_loc) {
  val fi = _fi
  val actuals = _actuals

  override def pretty = ""
}

case class Return(_value: Option[Expr], _loc: Location)
    extends Stmt(_loc) {
  val value = _value

  override def pretty = ""
}

case class If(_cond: Expr, _thn: Block, _els: Block, _loc: Location)
    extends Stmt(_loc) {
  val cond = _cond
  val thn = _thn
  val els = _els

  override def pretty = ""
}

case class While(_cond: Expr, _body: Block, _loc: Location)
    extends Stmt(_loc) {
  val cond = _cond
  val body = _body

  override def pretty = ""
}

// @FIXME: is "extends Node" correct?
case class Field(_expr: Expr, _name: String, _loc: Location)
    extends Node(_loc) {
  val expr = _expr
  val name = _name

  override def pretty = ""
}

abstract class Expr(_loc: Location) extends Node(_loc) {

}

case class Id(_name: String, _loc: Location)
    extends Expr(_loc) {
  val name = _name

  override def pretty = ""
}

case class GetField(_value: Field, _loc: Location)
    extends Expr(_loc) {
  val value = _value

  override def pretty = ""
}

case class ExprCall(_name: String, _actuals: List[Expr], _loc: Location)
    extends Expr(_loc) {
  val name = _name
  val actuals = _actuals

  override def pretty = ""
}

case class This(_loc: Location)
    extends Expr(_loc) {

  override def pretty = ""
}

case class New(_ty: Type, _actuals: List[Expr], _loc: Location)
    extends Expr(_loc) {
  val ty = _ty
  val actuals = _actuals

  override def pretty = ""
}

case class BExpr(_op: String, _left: Expr, _right: Expr, _loc: Location)
    extends Expr(_loc) {
  val op = _op
  val left = _left
  val right = _right

  override def pretty = ""
}

case class UExpr(_op: String, _value: Expr, _loc: Location)
    extends Expr(_loc) {
  val op = _op
  val value = _value

  override def pretty = ""
}

case class LitExpr(_value: Literal, _loc: Location)
    extends Expr(_loc) {
  val value = _value

  override def pretty = ""
}

abstract class Literal(_loc: Location) extends Node(_loc) {}

case class IntLit(_value: Int, _loc: Location)
    extends Literal(_loc) {
  val value = _value

  override def pretty = ""
}

case class BoolLit(_value: Boolean, _loc: Location)
    extends Literal(_loc) {
  val value = _value

  override def pretty = ""
}

case class StringLit(_value: String, _loc: Location)
    extends Literal(_loc) {
  val value = _value

  override def pretty = ""
}

