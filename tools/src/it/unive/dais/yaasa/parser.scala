package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
import scala.util.Either
import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.absyn._

object parser {

  class FJPPParser extends RegexParsers {
    //override type Elem = Char
    override protected val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
    private def eloc = Location.empty

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

    val reserved: Parser[String] =
      (kwClass | kwExtends | kwStatic | kwVoid | kwInt | kwBoolean | kwString |
        kwSkip | kwReturn | kwIf | kwElse | kwWhile |
        kwThis | kwNew | kwTrue | kwFalse | kwNull |
        kwPrint | kwPrintLn)

    val name: Parser[String] = "[A-Z_a-z][A-Z_a-z0-9]*".r
    //val float: Parser[String] = """[0-9]+.[0-9]*""".r
    val id: Parser[String] = not(reserved) ~> name
    val qid: Parser[String] = id ~ "." ~ id ^^ { case n1 ~ _ ~ n2 => "%s.%s" format (n1, n2) }
    val mqid: Parser[String] = qid | id
    //val annid: Parser[String] = float | mqid

    def _true = positioned(kwTrue ^^ { _ => BoolLit(true) })
    def _false = positioned(kwFalse ^^ { _ => BoolLit(false) })
    def _null = positioned(kwNull ^^ { _ => NullLit() })
    def integer = positioned("""(-?)(0|[1-9]\d*)""".r ^^ { i => IntLit(i.toInt) })
    def string = positioned("""\"[^'"']*\"""".r ^^ { s => StringLit(s.substring(1, s.length - 1)) })

    def program = (_class*) ^^ { classes => Program(classes) }

    def _class =
      positioned(
        kwClass ~ id ~ ((kwExtends ~> id)?) ~ "{" ~ (fieldDecl*) ~ (methodDecl*) ~ "}" ^^
          {
            case _ ~ name ~ extName ~ _ ~ fields ~ methods ~ _ =>
              Class(name, extName, fields, methods)
          })

    def fieldDecl =
      positioned(
        kwStatic ~> _type ~ id ~ (("," ~ id)*) ~ ";" ^^
          {
            case ty ~ n1 ~ others ~ _ =>
              val names = n1 :: (others map { case _ ~ n => n })
              FieldDecl(ty, names)
          })

    def _type: Parser[Type] =
      positioned(
        (kwInt ^^ { _ => TyInt() }) |
          (kwBoolean ^^ { _ => TyBool() }) |
          (kwString ^^ { _ => TyString() }) |
          (id ^^ { id => TyType(id) }))

    def methodDecl =
      positioned((annot?) ~ (voidMethodDecl | retMethodDecl) ^^ {
        case annot ~ md =>
          /*println(annot);
          println(md.prettyShort);*/
          md
      })

    def annot =
      "@@" ~> "[" ~> id ~ ":" ~ string ~ ((";" ~> id ~ ":" ~ string)*) <~ "]" ^^ {
        case id ~ _ ~ v1 ~ others =>
          val rest = (id, v1) :: others map { case id ~ _ ~ value => (id, value) }
          rest
        case boh =>
          println(boh);
          List()
      }

    def voidMethodDecl =
      positioned(
        kwStatic ~> kwVoid ~ id ~ formals ~ block ^^
          {
            case _ ~ name ~ formals ~ block =>
              MethodDecl(None, name, formals, block)
          })

    def retMethodDecl =
      positioned(
        kwStatic ~> _type ~ id ~ formals ~ block ^^
          {
            case ty ~ name ~ formals ~ block =>
              MethodDecl(Some(ty), name, formals, block)
          })

    def formals: Parser[List[Formal]] =
      ("(" ~ ")" ^^ { _ => List() }) |
        ("(" ~> formal <~ ")" ^^ { List(_) }) |
        ("(" ~ formal ~ (("," ~> formal)*) ~ ")" ^^
          {
            case _ ~ f ~ others ~ _ =>
              f :: others
          })
    def formal =
      positioned(_type ~ id ^^ { case ty ~ id => Formal(ty, id) })

    def block: Parser[Block] =
      positioned(
        "{" ~ (varDecl*) ~ (statement*) ~ "}" ^^
          {
            case _ ~ vars ~ stmts ~ _ =>
              Block(vars, stmts)
          })

    def varDecl =
      positioned(
        _type ~ id ~ (("," ~ id)*) ~ ";" ^^
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
        kwSkip ~ ";" ^^
          { _ =>
            SSkip()
          })

    def assign =
      positioned(
        mqid ~ "=" ~ expr ~ ";" ^^
          {
            case /*Left(id)*/ id ~ _ ~ exp ~ _ => SAssign(id, exp)
            //case Right(loc) ~ _ ~ exp ~ _ => SSetField(loc, exp)
          })

    def scall =
      positioned(bcall <~ ";" ^^
        {
          case (id /*Left(id)*/ , acts) => SCall(id, acts)
          //case (Right(f), acts) => SMethodCall(f, acts)
        })

    def sprint: Parser[SPrint] =
      positioned(
        kwPrint ~> "(" ~> expr <~ ")" <~ ";" ^^ { SPrint(false, _) } |
          kwPrintLn ~> "(" ~> expr <~ ")" <~ ";" ^^ { SPrint(true, _) })

    def bcall =
      mqid /*location*/ ~ actuals ^^
        {
          case id /*Left(id)*/ ~ acts => (id /*Left(id)*/ , acts)
          //case Right(loc) ~ acts => (Right(loc), acts)
        }

    def _return =
      positioned(kwReturn ~ ";" ^^ { _ =>
        SReturn(None)
      }) |
        positioned(kwReturn ~ expr ~ ";" ^^ {
          case _ ~ e ~ _ =>
            SReturn(Some(e))
        })

    def _if: Parser[Stmt] =
      positioned(kwIf ~ "(" ~ expr ~ ")" ~ /*kwThen ~*/ statement ~ kwElse ~ statement ^^
        { case _ ~ _ ~ cond ~ _ ~ /*_ ~*/ thn ~ _ ~ els => SIf(cond, thn, els) })

    def _while: Parser[Stmt] =
      positioned(kwWhile ~ "(" ~ expr ~ ")" ~ statement ^^
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
      ("(" ~ ")" ^^ { _ => List() }) |
        "(" ~ expr ~ (("," ~> expr)*) ~ ")" ^^
        { case _ ~ e1 ~ others ~ _ => e1 :: others }

    def expr: Parser[Expr] =
      positioned(_this | _new | ecall | variable | unexp | binexp | elit | parexp)

    def parexp =
      "(" ~> expr <~ ")" ^^ { e => e }

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
      positioned(kwThis ^^ { _ => EThis() })

    def _new =
      positioned(kwNew ~ id ~ actuals ^^
        {
          case _ ~ ty ~ acts => ENew(ty, acts)
        })

    def binexp =
      positioned("(" ~> expr ~ binop ~ expr <~ ")" ^^
        { case l ~ op ~ r => EBExpr(op, l, r) })

    def unexp =
      positioned(unop ~ expr ^^
        { case op ~ e => EUExpr(op, e) })

    def elit = positioned(lit ^^ { l => ELit(l) })

    def lit =
      positioned(_true | _false | _null | integer | string)

    def binop =
      //positioned(
      "++" ^^ { l => BOPlusPlus() } |
        "+" ^^ { l => BOPlus() } |
        "-" ^^ { l => BOMinus() } |
        "*" ^^ { l => BOMul() } |
        "/" ^^ { l => BODiv() } |
        "&&" ^^ { l => BOAnd() } |
        "||" ^^ { l => BOOr() } |
        "%" ^^ { l => BOMod() } |
        "<" ^^ { l => BOLt() } |
        "<=" ^^ { l => BOLeq() } |
        "==" ^^ { l => BOEq() } |
        ">" ^^ { l => BOGt() } |
        ">=" ^^ { l => BOGeq() } |
        "!=" ^^ { l => BONeq() }

    def unop =
      //positioned(
      "-" ^^ { l => UNeg() } |
        "!" ^^ { l => UNot() }

  }

  object FJPPParser extends FJPPParser {
    def parse(text: String): Program =
      parseAll(program, text) match {
        case Success(lup, _) => lup
        case Error(msg, next) =>
          throw ParseError("Parse failed with ERROR at %s\nwith message %s" format (next.pos.toString(), msg))
        case Failure(msg, next) =>
          throw ParseError("Parse failed at %s\nwith message %s" format (next.pos.toString(), msg))
      }
  }
}
