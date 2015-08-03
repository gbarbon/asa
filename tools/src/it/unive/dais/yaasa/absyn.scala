package it.unive.dais.yaasa.absyn

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.utils.parsingUtils._

class Node(loc: Location) {
  val location = loc
}

class TaggedNode[T](loc: Location, _tag: T)
    extends Node(loc) {
  val tag = _tag
}

case class Class(name: String, ext: String, fields: List[FieldDecl], methods: List[MethodDecl]) {
}

case class FieldDecl(ty: Type, names: List[String]) {

}

case class MethodDecl(returnTy: Option[Type], name: String, formals: Option[List[Formal]], body: Block) {
}

case class Formal(ty: Type, name: String) {

}

abstract class Type(_name: String) {
  val name = _name
}

case class TyInt()
  extends Type("Int") {}

case class TyBool()
  extends Type("Bool") {}

case class TyString()
  extends Type("String") {}

case class TyType(_name: String)
  extends Type(_name) {}

case class Block(varDecls: List[VarDecl], stmts: List[Stmt]) {

}

case class VarDecl(ty: Type, id: String) {

}

abstract class Stmt() {}

case class Skip() extends Stmt() {}

case class Assign(name: String, value: Expr)
  extends Stmt() {}

case class SetField(loc: Field, value: Expr)
  extends Stmt() {}

case class StmtCall(name: String, actuals: List[Expr])
  extends Stmt() {}

case class MethodCall(loc: Field, actuals: List[Expr])
  extends Stmt() {}

case class Return(value: Option[Expr])
  extends Stmt() {}

case class If(cond: Expr, thn: Block, els: Block)
  extends Stmt() {}

case class While(cond: Expr, body: Block)
  extends Stmt() {}

case class Field(expr: Expr, name: String) {
}

abstract class Expr() {

}

case class Id(name: String)
    extends Expr() {

}

case class GetField(value: Field)
    extends Expr() {

}

case class ExprCall(name: String, actuals: List[Expr])
    extends Expr() {

}

case class This()
    extends Expr() {

}

case class New(ty: Type, actuals: List[Expr])
    extends Expr() {

}

case class BExpr(op: String, left: Expr, right: Expr)
    extends Expr() {

}

case class UExpr(op: String, value: Expr)
    extends Expr() {

}

case class LitExpr(value: Literal)
    extends Expr() {

}

abstract class Literal() {}

case class IntLit(value: Int)
  extends Literal() {}

case class BoolLit(value: Boolean)
  extends Literal() {}

case class StringLit(value: String)
  extends Literal() {}

