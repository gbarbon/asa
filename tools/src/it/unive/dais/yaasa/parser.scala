package it.unive.dais.yaasa.parser

/**
 * @author esteffin
 */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.utils.parsingUtils._

class LoopParser extends RegexParsers {
  override type Elem = Char
  private def eloc = Location.empty
  def id = """[_\p{L}][_\p{L}\p{Nd}]*""".r
  def integer = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def _true = "true"
  def _false = "false"
  def program = _class*

  def _class =
    "class" ~ id ~ "extends" ~ id ~ "{" ~ (fieldDecl*) ~ /*(method*) ~*/ "}" ^^
      {
        case _ ~ name ~ _ ~ extName ~ _ ~ fields ~ /*methods ~ */ _ =>
          Class(name, extName, fields, List(), eloc)
      }

  def fieldDecl =
    _type ~ id ~ (("," ~ id)*) ~ ";" ^^
      {
        case ty ~ n1 ~ others ~ _ =>
          val names = n1 :: (others map { case _ ~ n => n })
          FieldDecl(ty, names, eloc)
      }

  def _type =
    id ^^
      {
        case "int"     => TyInt(eloc)
        case "boolean" => TyBool(eloc)
        case "string"  => TyString(eloc)
        case id        => TyType(id, eloc)
      }

  def loop =
    "for" ~ id ~ "in" ~ integer ~ "to" ~ integer ~ statement ^^
      {
        case f ~ variable ~ i ~ lBound ~ t ~ uBound ~ statement =>

          ForLoop(variable, lBound, uBound, statement)
      }
  def statements = statement*
  def block = "{" ~> statements <~ "}" ^^ { l => Block(l) }
  def statement: Parser[Statement] = loop | block
}

abstract trait Statement
case class Block(statements: List[Statement])
  extends Statement
case class ForLoop(variable: String, lowerBound: Int, upperBound: Int, statement: Statement)
  extends Statement

object TestLoopParser extends LoopParser {
  def parse(text: String) = parseAll(loop, text)
}
