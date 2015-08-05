package it.unive.dais.yaasa.parser.tiny

/**
 * @author esteffin
 */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.Either
import it.unive.dais.yaasa._
import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.absyn._

class WhileParser extends RegexParsers {
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
  def integer = """(-?)(0|[1-9]\d*)""".r ^^ { i => IntLit(i.toInt, eloc) }

  def program = (block) ^^ { b => b }

  def _type: Parser[Type] =
    (kwInt ^^ { _ => TyInt(eloc) }) |
      (kwBoolean ^^ { _ => TyBool(eloc) }) |
      (kwString ^^ { _ => TyString(eloc) }) |
      (id ^^ { id => TyType(id, eloc) })

  def block: Parser[Block] =
    "{" ~ (varDecl*) ~ (statement*) ~ "}" ^^
      {
        case _ ~ vars ~ stmts ~ _ =>
          println("hey block");
          Block(vars, stmts, eloc)
      }

  def varDecl =
    _type ~ id ~ ";" ^^
      {
        case ty ~ name ~ _ =>
          println("hey vd " + name);
          VarDecl(ty, name, eloc)
      }

  def statement =
    skip | assign | scall | _if | _while // | _return

  def skip =
    kwSkip ~ ";" ^^
      { _ =>
        println("hey skip");
        SSkip(eloc)
      }

  def assign =
    id ~ "=" ~ expr ~ ";" ^^
      {
        case id ~ _ ~ exp ~ _ => SAssign(id, exp, eloc)
      }

  def scall =
    bcall ~ ";" ^^
      {
        case (id, acts, loc) ~ _ => SCall(id, acts, loc)
      }

  def bcall =
    id ~ actuals ^^ { case id ~ acts => (id, acts, eloc) }
  /*
  def _return =
    (kwReturn ~ ";" ^^ { _ =>
      println("void return");
      SReturn(None, eloc)
    }) |
      (kwReturn ~ expr ~ ";" ^^ {
        case _ ~ e ~ _ =>
          println("e return");
          SReturn(Some(e), eloc)
      })*/

  def _if =
    kwIf ~ "(" ~ expr ~ ")" ~ kwThen ~ block ~ kwElse ~ block ^^
      { case _ ~ _ ~ cond ~ _ ~ _ ~ thn ~ _ ~ els => SIf(cond, thn, els, eloc) }

  def _while =
    kwWhile ~ "(" ~ expr ~ ")" ~ block ^^
      { case _ ~ _ ~ cond ~ _ ~ body => SWhile(cond, body, eloc) }

  /*
  def location: Parser[Either[String, Field]] =
    idLoc | fieldLoc

  def idLoc: Parser[Either[String, Field]] =
    id ^^
      { l => Left(l) }

  def fieldLoc: Parser[Either[String, Field]] =
    expr ~ "." ~ id ^^
      { case exp ~ _ ~ fn => Right(Field(exp, fn, eloc)) }
      */

  def actuals =
    ("(" ~ ")" ^^ { _ => List() }) |
      ("(" ~ expr ~ (("," ~ expr)*) ~ ")" ^^
        { case _ ~ e1 ~ others ~ _ => e1 :: (others map { case _ ~ e => e }) })

  def expr: Parser[Expr] =
    variable | elit | parexp | binexp | unexp | ecall //// | _this | _new

  def parexp =
    "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }

  def variable =
    id ^^
      {
        name => EVariable(name, eloc)
      }

  def ecall =
    bcall ^^
      {
        case (id, acts, loc) => ECall(id, acts, loc)
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

object TestWhileParser extends WhileParser {
  def parse(text: String) = parseAll(program, text)
}
