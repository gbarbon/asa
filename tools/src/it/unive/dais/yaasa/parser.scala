package it.unive.dais.yaasa.parser

/**
 * @author esteffin
 */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.Either
import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.absyn.EThis

class LoopParser extends RegexParsers {
  override type Elem = Char
  private def eloc = Location.empty
  def id = """[_\p{L}][_\p{L}\p{Nd}]*""".r
  def integer = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def _true = "true"
  def _false = "false"
  def program = _class*

  def _class =
    "class" ~ id ~ "extends" ~ id ~ "{" ~ (fieldDecl*) ~ (methodDecl*) ~ "}" ^^
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
    id ^^
      {
        case "int"     => TyInt(eloc)
        case "boolean" => TyBool(eloc)
        case "string"  => TyString(eloc)
        case id        => TyType(id, eloc)
      }

  def voidMethodDecl =
    "void" ~ id ~ formals ~ block ^^
      {
        case _ ~ name ~ formals ~ block =>
          MethodDecl(None, name, formals, block, eloc)
      }

  def retMethodDecl =
    _type ~ id ~ formals ~ block ^^
      {
        case ty ~ name ~ formals ~ block =>
          MethodDecl(Some(ty), name, formals, block, eloc)
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
    "{" ~> (varDecl*) ~ (statement*) <~ "}" ^^
      { case vars ~ stmts => Block(List(), List(), eloc) }

  def varDecl =
    _type ~ id ~ ";" ^^
      { case ty ~ name ~ _ => VarDecl(ty, name, eloc) }

  def statement =
    skip | assign | scall | _return | _if | _while

  def skip =
    "skip" ~ ";" ^^
      { _ => SSkip(eloc) }

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
    ("return" ~ ";" ^^ { _ => SReturn(None, eloc) }) |
      ("return" ~ expr ~ ";" ^^ { case _ ~ e ~ _ => SReturn(Some(e), eloc) })

  def _if =
    "if" ~ "(" ~ expr ~ ")" ~ "then" ~ block ~ "else" ~ block ^^
      { case _ ~ _ ~ cond ~ _ ~ _ ~ thn ~ _ ~ els => SIf(cond, thn, els, eloc) }

  def _while =
    "while" ~ "(" ~ expr ~ ")" ~ block ^^
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
      "(" ~ expr ~ (("," ~ expr)*) ~ ")" ^^
      { case _ ~ e1 ~ others ~ _ => e1 :: (others map { case _ ~ e => e }) }

  def expr: Parser[Expr] =
    variable | ecall | _this

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
    "this" ^^ { _ => EThis(eloc) }

  def _new =
    "new" ~ id ~ actuals ^^
      {
        case _ ~ ty ~ acts => ENew(ty, acts, eloc)
      }

  def binexp =
    expr ~ binop ~ expr ^^
      { case l ~ op ~ r => EBExpr(op, l, r, eloc) }

  def unexp =
    unop ~ expr ^^
      { case op ~ e => EUExpr(op, e, eloc) }

  //def lit =

  def binop = "%" ^^ { l => l }

  def unop = "!" ^^ { l => l }

  /*
  def loop =
    "for" ~ id ~ "in" ~ integer ~ "to" ~ integer ~ statement ^^
      {
        case f ~ variable ~ i ~ lBound ~ t ~ uBound ~ statement =>

          ForLoop(variable, lBound, uBound, statement)
      }
  def statements = statement*
  def block = "{" ~> statements <~ "}" ^^ { l => Block(l) }
  def statement: Parser[Statement] = loop | block*/
}
/*
abstract trait Statement
case class Block(statements: List[Statement])
  extends Statement
case class ForLoop(variable: String, lowerBound: Int, upperBound: Int, statement: Statement)
  extends Statement
*/

object TestLoopParser extends LoopParser {
  def parse(text: String) = parseAll(loop, text)
}
