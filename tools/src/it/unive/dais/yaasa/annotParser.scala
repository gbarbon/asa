package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.Either
import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.datatype.FortyTwo._

object annotParser {
  object AnnotationParser extends RegexParsers {
    override protected val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

    //type Parser[A] = String

    private val kwAtat: Parser[String] = "@@" //
    private val kwSqBra: Parser[String] = "[" //
    private val kwSqKet: Parser[String] = "]" //
    private val kwComma: Parser[String] = "," //
    private val kwColon: Parser[String] = ":" //
    private val kwSemicolon: Parser[String] = ";" //

    private val reserved: Parser[String] =
      (kwSqBra | kwSqKet | kwAtat | kwComma | kwColon | kwSemicolon)

    private val name: Parser[String] = "[A-Z_a-z][A-Z_a-z0-9]*".r
    private val id: Parser[String] = not(reserved) ~> name
    private def string = """\"[^'"']*\"""".r ^^ { s => s.substring(1, s.length - 1) }

    private def annot: Parser[Annot] =
      kwAtat ~ kwSqBra ~> id ~ kwColon ~ string ~ ((kwSemicolon ~> id ~ kwColon ~ string)*) <~ kwSqKet ^^ {
        case id ~ _ ~ v1 ~ others =>
          val oths = for ((n ~ _ ~ v) <- others) yield (n, v)
          val cont = ((id, v1) :: oths).toMap
          if (cont contains "conf")
            LabelAnnot.parse(cont)
          else
            FunAnnot.parse(cont)
      }
    private def row: Parser[(String, Annot)] =
      id ~ kwComma ~ annot ^^ { case id ~ _ ~ annot => (id, annot) }

    private def rows: Parser[Map[String, Annot]] =
      (row*) ^^ { rows => rows toMap }

    def parse(text: String): Annot = {
      parseAll(annot, text) match {
        case Success(lup, _) => lup
        case Error(msg, next) =>
          throw ParseError("Parsing annotation %s failed with ERROR at %s\nwith message %s" format (text, next.pos.toString(), msg))
        case Failure(msg, next) =>
          throw ParseError("Parse annotation %s failed at %s\nwith message %s" format (text, next.pos.toString(), msg))
      }
    }
    def parseAll(text: String): Map[String, Annot] = {
      parseAll(rows, text) match {
        case Success(lup, _) => lup
        case Error(msg, next) =>
          throw ParseError("Parsing annotation %s failed with ERROR at %s\nwith message %s" format (text, next.pos.toString(), msg))
        case Failure(msg, next) =>
          throw ParseError("Parse annotation %s failed at %s\nwith message %s" format (text, next.pos.toString(), msg))
      }
    }
  }
}
