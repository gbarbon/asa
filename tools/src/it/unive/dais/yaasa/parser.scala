package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.Either
import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.abstract_values._
import it.unive.dais.yaasa.absyn._

object parser {

  class FJPPParser(library: Boolean = false, operatorAnnots: Map[String, FunAnnot]) extends RegexParsers {
    //override type Elem = Char
    override protected val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

    val kwClass: Parser[String] = "class\\b".r
    val kwExtends: Parser[String] = "extends\\b".r
    val kwVoid: Parser[String] = "void\\b".r
    val kwInt: Parser[String] = "int\\b".r
    val kwBoolean: Parser[String] = "boolean\\b".r
    val kwString: Parser[String] = "string\\b".r
    val kwSkip: Parser[String] = "skip\\b".r
    val kwReturn: Parser[String] = "return\\b".r
    val kwIf: Parser[String] = "if\\b".r
    val kwElse: Parser[String] = "else\\b".r
    val kwWhile: Parser[String] = "while\\b".r
    val kwThis: Parser[String] = "this\\b".r
    val kwNew: Parser[String] = "new\\b".r
    val kwTrue: Parser[String] = "true\\b".r
    val kwFalse: Parser[String] = "false\\b".r
    val kwNull: Parser[String] = "null\\b".r
    val kwPrint: Parser[String] = "print\\b".r
    val kwPrintLn: Parser[String] = "println\\b".r
    val kwStatic: Parser[String] = "static\\b".r
    val kwBra: Parser[String] = "(" //
    val kwKet: Parser[String] = ")" //
    val kwSqBra: Parser[String] = "[" //
    val kwSqKet: Parser[String] = "]" //
    val kwCurBra: Parser[String] = "{" //
    val kwCurKet: Parser[String] = "}" //
    val kwDot: Parser[String] = "." //
    val kwComma: Parser[String] = "," //
    val kwEquals: Parser[String] = "=" //
    val kwColon: Parser[String] = ":" //
    val kwSemicolon: Parser[String] = ";" //
    val kwAtat: Parser[String] = "@@" //
    val kwConcat: Parser[String] = "++" //
    val kwPlus: Parser[String] = "+" //
    val kwMinus: Parser[String] = "-" //
    val kwMul: Parser[String] = "*" //
    val kwDiv: Parser[String] = "/" //
    val kwAnd: Parser[String] = "&&" //
    val kwOr: Parser[String] = "||" //
    val kwMod: Parser[String] = "%" //
    val kwLt: Parser[String] = "<" //
    val kwLeq: Parser[String] = "<=" //
    val kwEq: Parser[String] = "==" //
    val kwGt: Parser[String] = ">" //
    val kwGeq: Parser[String] = ">=" //
    val kwNeq: Parser[String] = "!=" //
    val kwNot: Parser[String] = "!" //

    val reserved: Parser[String] =
      (kwClass | kwExtends | kwStatic | kwVoid | kwInt | kwBoolean | kwString |
        kwSkip | kwReturn | kwIf | kwElse | kwWhile |
        kwThis | kwNew | kwTrue | kwFalse | kwNull |
        kwPrint | kwPrintLn |
        kwBra | kwKet | kwSqBra | kwSqKet | kwCurBra | kwCurKet |
        kwDot | kwComma | kwEquals | kwColon | kwSemicolon | kwAtat |
        kwConcat | kwPlus | kwMinus | kwMul | kwDiv | kwMod |
        kwEq | kwNeq | kwLt | kwLeq | kwGt | kwGeq |
        kwAnd | kwOr | kwNot)

    val name: Parser[String] = "[A-Z_a-z][A-Z_a-z0-9]*".r
    //val float: Parser[String] = """[0-9]+.[0-9]*""".r
    val libName: Parser[String] = "#[A-Z_a-z][A-Z_a-z0-9]*".r
    val id: Parser[String] = not(reserved) ~> (if (!library) name else libName | name)
    val qid: Parser[String] = id ~ kwDot ~ id ^^ { case n1 ~ _ ~ n2 => "%s.%s" format (n1, n2) }
    val mqid: Parser[String] = qid | id
    //val annid: Parser[String] = float | mqid

    def _true = positioned(kwTrue ^^ { _ => BoolLit(true) })
    def _false = positioned(kwFalse ^^ { _ => BoolLit(false) })
    def _null = positioned(kwNull ^^ { _ => NullLit })
    def integer = positioned("""(-?)(0|[1-9]\d*)""".r ^^ { i => IntLit(i.toInt) })
    def string = positioned("""\"[^"]*\"""".r ^^ { s => StringLit(s.substring(1, s.length - 1)) })
    def annot = """@@\[[^\[\]]*\]""".r ^^ { s => s }

    def program = (_class*) ^^ { classes => Program(classes) }

    def _class =
      positioned(
        kwClass ~ id ~ ((kwExtends ~> id)?) ~ kwCurBra ~ (fieldDecl*) ~ (methodDecl*) ~ kwCurKet ^^
          {
            case _ ~ name ~ extName ~ _ ~ fields ~ methods ~ _ =>
              Class(name, extName, fields, methods)
          })

    def fieldDecl =
      positioned(
        kwStatic ~> _type ~ id ~ ((kwComma ~ id)*) ~ kwSemicolon ^^
          {
            case ty ~ n1 ~ others ~ _ =>
              val names = n1 :: (others map { case _ ~ n => n })
              FieldDecl(ty, names)
          })

    def _type: Parser[Type] =
      positioned(
        (kwInt ^^ { _ => TyInt }) |
          (kwBoolean ^^ { _ => TyBool }) |
          (kwString ^^ { _ => TyString }) |
          (id ^^ { id => TyType(id) }))

    def methodDecl: Parser[MethodDecl] =
      positioned(
        if (!library)
          (voidMethodDecl | retMethodDecl ^^ {
          case md =>
            md
        })
        else
          ((annot?) ~ (voidMethodDecl | retMethodDecl) ^^ {
            case None ~ md => md
            case Some(annot) ~ md =>
              val annot_body: Annot = annotParser.AnnotationParser.parse(annot)
              md.copy(annot = Some(annot_body))
          }))

    def voidMethodDecl =
      positioned(
        kwStatic ~> kwVoid ~ id ~ formals ~ block ^^
          {
            case _ ~ name ~ formals ~ block =>
              MethodDecl(None, name, formals, block, None)
          })

    def retMethodDecl =
      positioned(
        kwStatic ~> _type ~ id ~ formals ~ block ^^
          {
            case ty ~ name ~ formals ~ block =>
              MethodDecl(Some(ty), name, formals, block, None)
          })

    def formals: Parser[List[Formal]] =
      (kwBra ~ kwKet ^^ { _ => List() }) |
        (kwBra ~> formal <~ kwKet ^^ { List(_) }) |
        (kwBra ~ formal ~ ((kwComma ~> formal)*) ~ kwKet ^^
          {
            case _ ~ f ~ others ~ _ =>
              f :: others
          })
    def formal =
      positioned(_type ~ id ^^ { case ty ~ id => Formal(ty, id) })

    def block: Parser[Block] =
      positioned(
        kwCurBra ~ (varDecl*) ~ (statement*) ~ kwCurKet ^^
          {
            case _ ~ vars ~ stmts ~ _ =>
              Block(vars, stmts)
          })

    def varDecl =
      positioned(
        _type ~ id ~ ((kwComma ~ id)*) ~ kwSemicolon ^^
          {
            case ty ~ n1 ~ others ~ _ =>
              val names = n1 :: (others map { case _ ~ n => n })
              VarDecl(ty, names)
          })

    def statement: Parser[Stmt] =
      positioned(
        skip | _return | assign | sprint | scall | _if | _while | sblock)

    def skip =
      positioned(
        kwSkip ~ kwSemicolon ^^
          { _ =>
            SSkip
          })

    def assign =
      positioned(
        mqid ~ kwEquals ~ expr ~ kwSemicolon ^^
          {
            case /*Left(id)*/ id ~ _ ~ exp ~ _ => SAssign(id, exp)
            //case Right(loc) ~ _ ~ exp ~ _ => SSetField(loc, exp)
          })

    def scall =
      positioned(bcall <~ kwSemicolon ^^
        {
          case (id /*Left(id)*/ , acts) => SCall(id, acts)
          //case (Right(f), acts) => SMethodCall(f, acts)
        })

    def sprint: Parser[SPrint] =
      positioned(
        kwPrint ~> kwBra ~> expr <~ kwKet <~ kwSemicolon ^^ { SPrint(false, _) } |
          kwPrintLn ~> kwBra ~> expr <~ kwKet <~ kwSemicolon ^^ { SPrint(true, _) })

    def bcall =
      mqid /*location*/ ~ actuals ^^
        {
          case id /*Left(id)*/ ~ acts => (id /*Left(id)*/ , acts)
          //case Right(loc) ~ acts => (Right(loc), acts)
        }

    def _return =
      positioned(kwReturn ~ kwSemicolon ^^ { _ =>
        SReturn(None)
      }) |
        positioned(kwReturn ~ expr ~ kwSemicolon ^^ {
          case _ ~ e ~ _ =>
            SReturn(Some(e))
        })

    def _if: Parser[Stmt] =
      positioned(kwIf ~ kwBra ~ expr ~ kwKet ~ /*kwThen ~*/ statement ~ kwElse ~ statement ^^
        { case _ ~ _ ~ cond ~ _ ~ /*_ ~*/ thn ~ _ ~ els => SIf(cond, thn, els) })

    def _while: Parser[Stmt] =
      positioned(kwWhile ~ kwBra ~ expr ~ kwKet ~ statement ^^
        { case _ ~ _ ~ cond ~ _ ~ body => SWhile(cond, body) })

    def sblock: Parser[Stmt] =
      block ^^ { SBlock(_) }

    /*def location: Parser[Either[String, Field]] =
      idLoc | fieldLoc

    def idLoc: Parser[Either[String, Field]] =
      id ^^
        { l => Left(l) }

    def fieldLoc: Parser[Either[String, Field]] =
      floc ^^ { Right(_) }

    def floc =
      positioned("(" ~> expr ~ ")" ~ "." ~ id ^^
        { case exp ~ _ ~ _ ~ fn => Field(exp, fn) })*/

    def actuals =
      (kwBra ~ kwKet ^^ { _ => List() }) |
        kwBra ~ expr ~ ((kwComma ~> expr)*) ~ kwKet ^^
        { case _ ~ e1 ~ others ~ _ => e1 :: others }

    def expr: Parser[Expr] =
      positioned(_this | _new | ecall | variable | unexp | binexp | elit | parexp)

    def parexp =
      kwBra ~> expr <~ kwKet ^^ { e => e }

    def variable =
      positioned(mqid ^^
        {
          EVariable(_)
          //case Left(name) => EVariable(name)
          //case Right(loc) => EGetField(loc)
        })

    def ecall =
      positioned(bcall ^^
        {
          case (id /*Left(id)*/ , acts) => ECall(id, acts)
          //case (Right(f), acts) => EMethodCall(f, acts)
        })

    def _this =
      positioned(kwThis ^^ { _ => EThis })

    def _new =
      positioned(kwNew ~ id ~ actuals ^^
        {
          case _ ~ ty ~ acts => ENew(ty, acts)
        })

    def binexp =
      positioned(kwBra ~> expr ~ binop ~ expr <~ kwKet ^^
        { case l ~ op ~ r => EBExpr(op, l, r) })

    def unexp =
      positioned(unop ~ expr ^^
        { case op ~ e => EUExpr(op, e) })

    def elit = positioned(lit ^^ { l => ELit(l) })

    def lit =
      positioned(_true | _false | _null | integer | string)

    def binop =
      //positioned(
      kwConcat ^^ { l => BOPlusPlus(operatorAnnots("BOPlusPlus")) } |
        kwPlus ^^ { l => BOPlus(operatorAnnots("BOPlus")) } |
        kwMinus ^^ { l => BOMinus(operatorAnnots("BOMinus")) } |
        kwMul ^^ { l => BOMul(operatorAnnots("BOMul")) } |
        kwDiv ^^ { l => BODiv(operatorAnnots("BODiv")) } |
        kwMod ^^ { l => BOMod(operatorAnnots("BOMod")) } |
        kwAnd ^^ { l => BOAnd(operatorAnnots("BOAnd")) } |
        kwEq ^^ { l => BOEq(operatorAnnots("BOEq")) } |
        kwNeq ^^ { l => BONeq(operatorAnnots("BONeq")) } |
        kwOr ^^ { l => BOOr(operatorAnnots("BOOr")) } |
        kwLt ^^ { l => BOLt(operatorAnnots("BOLt")) } |
        kwLeq ^^ { l => BOLeq(operatorAnnots("BOLeq")) } |
        kwGt ^^ { l => BOGt(operatorAnnots("BOGt")) } |
        kwGeq ^^ { l => BOGeq(operatorAnnots("BOGeq")) }

    def unop =
      //positioned(
      kwMinus ^^ { l => UNeg(operatorAnnots("UNeg")) } |
        kwNot ^^ { l => UNot(operatorAnnots("UNot")) }

  }

  object FJPPParser {
    def parse(library: Boolean, funAnnots: Map[String, FunAnnot], text: String, fname: String): Program = {
      val parser = new FJPPParser(library, funAnnots)
      parser.parseAll(parser.program, text) match {
        case parser.Success(lup, _) => lup
        case parser.Error(msg, next) =>
          throw ParseError("Parse of %s failed with ERROR at %s\nwith message %s" format (fname, next.pos.toString(), msg))
        case parser.Failure(msg, next) =>
          throw ParseError("Parse of %s failed at %s\nwith message %s" format (fname, next.pos.toString(), msg))
      }
    }
  }
}
