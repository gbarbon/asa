package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.utils.prelude._
import scala.util.parsing.input._
import it.unive.dais.yaasa.datatype.LMH._
import it.unive.dais.yaasa.datatype.ADType._

object absyn {

  type id = String

  type Uid = String

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
    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = name + "(" + (actuals map (_.pretty)) + ");\n"
    override def prettyShort = name + "(" + (actuals map (_.prettyShort)) + ");\n"
  }
  object SCall {
    def create(name: String, actuals: List[Expr], fname: String): SCall = {
      val call = SCall(name, actuals)
      call.fname = fname
      call
    }
  }

  case class SNativeCall(name: String, actuals: List[Expr])
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

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = name + "(" + (actuals map (_.pretty)) + ")\n"
    override def prettyShort = name + "(" + (actuals map (_.prettyShort)) + ")\n"
  }
  object ECall {
    def create(name: String, actuals: List[Expr], fname: String): ECall = {
      val call = ECall(name, actuals)
      call.fname = fname
      call
    }
  }

  case class ENativeCall(name: String, actuals: List[Expr])
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
    def uid: Uid
  }

  case class BOPlus(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "+"
    override def prettyShort = "+"
  }
  object BOPlus {
    def create(annot: FunAnnot, fname: String): BOPlus = {
      val op = BOPlus(annot)
      op.fname = fname
      op
    }
  }

  case class BOMinus(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "-"
    override def prettyShort = "-"
  }
  object BOMinus {
    def create(annot: FunAnnot, fname: String): BOMinus = {
      val op = BOMinus(annot)
      op.fname = fname
      op
    }
  }

  case class BOMul(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "*"
    override def prettyShort = "*"
  }
  object BOMul {
    def create(annot: FunAnnot, fname: String): BOMul = {
      val op = BOMul(annot)
      op.fname = fname
      op
    }
  }
  //TODO: add uid in pretty print...
  case class BODiv(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "/"
    override def prettyShort = "/"
  }
  object BODiv {
    def create(annot: FunAnnot, fname: String): BODiv = {
      val op = BODiv(annot)
      op.fname = fname
      op
    }
  }

  case class BOAnd(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "&&"
    override def prettyShort = "&&"
  }
  object BOAnd {
    def create(annot: FunAnnot, fname: String): BOAnd = {
      val op = BOAnd(annot)
      op.fname = fname
      op
    }
  }

  case class BOOr(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "||"
    override def prettyShort = "||"
  }
  object BOOr {
    def create(annot: FunAnnot, fname: String): BOOr = {
      val op = BOOr(annot)
      op.fname = fname
      op
    }
  }

  case class BOMod(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "%"
    override def prettyShort = "%"
  }
  object BOMod {
    def create(annot: FunAnnot, fname: String): BOMod = {
      val op = BOMod(annot)
      op.fname = fname
      op
    }
  }

  case class BOLt(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "<"
    override def prettyShort = "<"
  }
  object BOLt {
    def create(annot: FunAnnot, fname: String): BOLt = {
      val op = BOLt(annot)
      op.fname = fname
      op
    }
  }

  case class BOLeq(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "<="
    override def prettyShort = "<="
  }
  object BOLeq {
    def create(annot: FunAnnot, fname: String): BOLeq = {
      val op = BOLeq(annot)
      op.fname = fname
      op
    }
  }

  case class BOEq(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "=="
    override def prettyShort = "=="
  }
  object BOEq {
    def create(annot: FunAnnot, fname: String): BOEq = {
      val op = BOEq(annot)
      op.fname = fname
      op
    }
  }

  case class BOGt(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = ">"
    override def prettyShort = ">"
  }
  object BOGt {
    def create(annot: FunAnnot, fname: String): BOGt = {
      val op = BOGt(annot)
      op.fname = fname
      op
    }
  }

  case class BOGeq(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = ">="
    override def prettyShort = ">="
  }
  object BOGeq {
    def create(annot: FunAnnot, fname: String): BOGeq = {
      val op = BOGeq(annot)
      op.fname = fname
      op
    }
  }

  case class BONeq(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "!="
    override def prettyShort = "!="
  }
  object BONeq {
    def create(annot: FunAnnot, fname: String): BONeq = {
      val op = BONeq(annot)
      op.fname = fname
      op
    }
  }

  /**
   * String concatenation operator
   */
  case class BOPlusPlus(annot: FunAnnot)
      extends BOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "++"
    override def prettyShort = "++"
  }
  object BOPlusPlus {
    def create(annot: FunAnnot, fname: String): BOPlusPlus = {
      val op = BOPlusPlus(annot)
      op.fname = fname
      op
    }
  }

  trait UOperator extends Node {
    val annot: FunAnnot
    def uid: Uid
  }

  case class UNot(annot: FunAnnot)
      extends UOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "!"
    override def prettyShort = "!"
  }
  object UNot {
    def create(annot: FunAnnot, fname: String): UNot = {
      val op = UNot(annot)
      op.fname = fname
      op
    }
  }

  case class UNeg(annot: FunAnnot)
      extends UOperator {

    private var fname: String = "WARNING! Node with Uid without file name. POSSIBLE CLASHES"
    def uid: Uid = "%s@%s" format (fname, (this.pos).toString)

    override def pretty = "-"
    override def prettyShort = "-"
  }
  object UNeg {
    def create(annot: FunAnnot, fname: String): UNeg = {
      val op = UNeg(annot)
      op.fname = fname
      op
    }
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
