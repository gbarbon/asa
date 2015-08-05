package it.unive.dais.yaasa.parser

/**
 * @author esteffin
 */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.Either
import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.absyn._

class LoopParser extends RegexParsers {
  //override type Elem = Char
  private def eloc = Location.empty
  val name: Parser[String] = "[A-Z_a-z][A-Z_a-z0-9]*".r

  val kwClass: Parser[String] = "class\\b".r
  val kwExtends: Parser[String] = "extends\\b".r
  val kwVoid: Parser[String] = "void\\b".r
  val kwInt: Parser[String] = "int\\b".r
  val kwBoolean: Parser[String] = "boolean\\b".r
  val kwString: Parser[String] = "string\\b".r
  val kwSkip: Parser[String] = "skip\\b".r
  val kwReturn: Parser[String] = "return\\b".r
  val kwIf: Parser[String] = "if\\b".r
  val kwThen: Parser[String] = "then\\b".r
  val kwElse: Parser[String] = "else\\b".r
  val kwWhile: Parser[String] = "while\\b".r
  val kwThis: Parser[String] = "this\\b".r
  val kwNew: Parser[String] = "new\\b".r
  val kwTrue: Parser[String] = "true\\b".r
  val kwFalse: Parser[String] = "false\\b".r
  val kwNull: Parser[String] = "null\b".r

  val reserved: Parser[String] =
    (kwClass | kwExtends | kwVoid | kwInt | kwBoolean | kwString |
      kwSkip | kwReturn | kwIf | kwThen | kwElse | kwWhile |
      kwThis | kwNew | kwTrue | kwFalse | kwNull)

  val id: Parser[String] = not(reserved) ~> name

  def _true = kwTrue ^^ { _ => BoolLit(true, eloc) }
  def _false = kwFalse ^^ { _ => BoolLit(false, eloc) }
  def _null = kwNull ^^ { _ => NullLit(eloc) }
  def integer = """(0|[1-9]\d*)""".r ^^ { i => IntLit(i.toInt, eloc) }

  def program = (_class*) ^^ { classes => Program(classes, eloc) }

  def _class =
    kwClass ~ id ~ kwExtends ~ id ~ "{" ~ (fieldDecl*) ~ (methodDecl*) ~ "}" ^^
      {
        case _ ~ name ~ _ ~ extName ~ _ ~ fields ~ methods ~ _ =>
          Class(name, extName, fields, methods, eloc)
      }

  def fieldDecl =
    _type ~ id ~ (("," ~ id)*) ~ ";" ^^
      {
        case ty ~ n1 ~ others ~ _ =>
          val names = n1 :: (others map { case _ ~ n => n })
          FieldDecl(ty, names, eloc)
      }

  def _type: Parser[Type] =
    (kwInt ^^ { _ => TyInt(eloc) }) |
      (kwBoolean ^^ { _ => TyBool(eloc) }) |
      (kwString ^^ { _ => TyString(eloc) }) |
      (id ^^ { id => TyType(id, eloc) })

  def voidMethodDecl =
    kwVoid ~ id ~ formals ~ block ^^ //"{" ~ "}" ^^ //
      {
        case _ ~ name ~ formals ~ block => // ~ _ ~ _ => //
          println("hey voidMeth");
          MethodDecl(None, name, formals, block, eloc)
      }

  def retMethodDecl =
    _type ~ id ~ formals ~ "{" ~ "}" ^^ //block ^^
      {
        case ty ~ name ~ formals ~ _ ~ _ => //~ block =>
          println("hey");
          MethodDecl(Some(ty), name, formals, Block(List(), List(), eloc), eloc)
      }

  def methodDecl =
    voidMethodDecl | retMethodDecl

  def formals =
    ("(" ~ ")" ^^ { _ => List() }) |
      "(" ~ _type ~ id ~ (("," ~> _type ~ id)*) ~ ")" ^^
      {
        case _ ~ ty ~ id ~ others ~ _ =>
          Formal(ty, id, eloc) :: (others map { case ty ~ n => Formal(ty, n, eloc) })
      }

  def block: Parser[Block] =
    /*("{" ~ (varDecl*) ~ "}" ^^ { case _ ~ vds ~ _ => Block(vds, List(), eloc) }) |
      ("{" ~ (statement*) ~ "}" ^^ { case _ ~ stmts ~ _ => Block(List(), stmts, eloc) }) |*/
    "{" ~ (varDecl*) ~ (statement) ~ "}" ^^
      {
        case _ ~ vars ~ stmts ~ _ =>
          println("hey block");
          Block(vars, List(stmts), eloc)
      }

  def varDecl =
    _type ~ id ~ ";" ^^
      {
        case ty ~ name ~ _ =>
          println("hey vd " + name);
          VarDecl(ty, name, eloc)
      }

  def statement =
    skip | assign | scall | _return | _if | _while

  def skip =
    kwSkip ~ ";" ^^
      { _ =>
        println("hey skip");
        SSkip(eloc)
      }

  def assign =
    location ~ "=" ~ expr ~ ";" ^^
      {
        case Left(id) ~ _ ~ exp ~ _   => SAssign(id, exp, eloc)
        case Right(loc) ~ _ ~ exp ~ _ => SSetField(loc, exp, eloc)
      }

  def scall =
    bcall ^^
      {
        case (Left(id), acts, loc) => SCall(id, acts, loc)
        case (Right(f), acts, loc) => SMethodCall(f, acts, loc)
      }

  def bcall =
    location ~ actuals ~ ";" ^^
      {
        case Left(id) ~ acts ~ _   => (Left(id), acts, eloc)
        case Right(loc) ~ acts ~ _ => (Right(loc), acts, eloc)
      }

  def _return =
    (kwReturn ~ ";" ^^ { _ =>
      println("void return");
      SReturn(None, eloc)
    }) |
      (kwReturn ~ expr ~ ";" ^^ {
        case _ ~ e ~ _ =>
          println("e return");
          SReturn(Some(e), eloc)
      })

  def _if =
    kwIf ~ "(" ~ expr ~ ")" ~ kwThen ~ block ~ kwElse ~ block ^^
      { case _ ~ _ ~ cond ~ _ ~ _ ~ thn ~ _ ~ els => SIf(cond, thn, els, eloc) }

  def _while =
    kwWhile ~ "(" ~ expr ~ ")" ~ block ^^
      { case _ ~ _ ~ cond ~ _ ~ body => SWhile(cond, body, eloc) }

  def location: Parser[Either[String, Field]] =
    idLoc | fieldLoc

  def idLoc: Parser[Either[String, Field]] =
    id ^^
      { l => Left(l) }

  def fieldLoc: Parser[Either[String, Field]] =
    expr ~ "." ~ id ^^
      { case exp ~ _ ~ fn => Right(Field(exp, fn, eloc)) }

  def actuals =
    ("(" ~ ")" ^^ { _ => List() }) |
      ("(" ~ expr ~ (("," ~ expr)*) ~ ")" ^^
        { case _ ~ e1 ~ others ~ _ => e1 :: (others map { case _ ~ e => e }) })

  def expr: Parser[Expr] =
    variable | ecall | _this | _new | binexp | unexp | elit | parexp

  def parexp =
    "(" ~> expr <~ ")" ^^ { e => e }

  def variable =
    location ^^
      {
        case Left(name) => EVariable(name, eloc)
        case Right(loc) => EGetField(loc, eloc)
      }

  def ecall =
    bcall ^^
      {
        case (Left(id), acts, loc) => ECall(id, acts, loc)
        case (Right(f), acts, loc) => EMethodCall(f, acts, loc)
      }

  def _this =
    kwThis ^^ { _ => EThis(eloc) }

  def _new =
    kwNew ~ id ~ actuals ^^
      {
        case _ ~ ty ~ acts => ENew(ty, acts, eloc)
      }

  def binexp =
    expr ~ binop ~ expr ^^
      { case l ~ op ~ r => EBExpr(op, l, r, eloc) }

  def unexp =
    unop ~ expr ^^
      { case op ~ e => EUExpr(op, e, eloc) }

  def elit = lit ^^ { l => ELit(l, eloc) }

  def lit =
    _true | _false | _null | integer

  def binop = "%" ^^ { l => l }

  def unop = "!" ^^ { l => l }

}

object TestLoopParser extends LoopParser {
  def parse(text: String) = parseAll(program, text)
}
