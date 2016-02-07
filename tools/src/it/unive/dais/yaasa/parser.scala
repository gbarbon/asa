package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.datatype.ABSValue.{TyType, TyBool, TyString, TyNum}
import it.unive.dais.yaasa.utils.prelude.Unexpected
//import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
//import scala.util.Either
import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.datatype.FortyTwo._
import it.unive.dais.yaasa.absyn._

object parser {

  class FJPPParser(fname: String, library: Boolean = false, operatorAnnots: Map[String, FunAnnot]) extends RegexParsers {
    //override type Elem = Char
    override protected val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

    val kwClass:     Parser[String] = "class\\b".r
    val kwExtends:   Parser[String] = "extends\\b".r
    val kwVoid:      Parser[String] = "void\\b".r
    val kwInt:       Parser[String] = "int\\b".r
    val kwBoolean:   Parser[String] = "boolean\\b".r
    val kwString:    Parser[String] = "String\\b".r
    val kwSkip:      Parser[String] = "skip\\b".r
    val kwReturn:    Parser[String] = "return\\b".r
    val kwIf:        Parser[String] = "if\\b".r
    val kwElse:      Parser[String] = "else\\b".r
    val kwWhile:     Parser[String] = "while\\b".r
    val kwThis:      Parser[String] = "this\\b".r
    val kwNew:       Parser[String] = "new\\b".r
    val kwTrue:      Parser[String] = "true\\b".r
    val kwFalse:     Parser[String] = "false\\b".r
    val kwNull:      Parser[String] = "null\\b".r
    val kwLog:     Parser[String] = "log\\b".r
    val kwPrint:     Parser[String] = "print\\b".r
    val kwPrintLn:   Parser[String] = "println\\b".r
    val kwStatic:    Parser[String] = "static\\b".r
    val kwBra:       Parser[String] = "(" //
    val kwKet:       Parser[String] = ")" //
    val kwSqBra:     Parser[String] = "[" //
    val kwSqKet:     Parser[String] = "]" //
    val kwCurBra:    Parser[String] = "{" //
    val kwCurKet:    Parser[String] = "}" //
    val kwDot:       Parser[String] = "." //
    val kwComma:     Parser[String] = "," //
    val kwEquals:    Parser[String] = "=" //
    val kwColon:     Parser[String] = ":" //
    val kwSemicolon: Parser[String] = ";" //
    val kwAtat:      Parser[String] = "@@" //
    val kwConcat:    Parser[String] = "++" //
    val kwPlus:      Parser[String] = "+" //
    val kwMinus:     Parser[String] = "-" //
    val kwMul:       Parser[String] = "*" //
    val kwDiv:       Parser[String] = "/" //
    val kwAnd:       Parser[String] = "&&" //
    val kwOr:        Parser[String] = "||" //
    val kwMod:       Parser[String] = "%" //
    val kwLt:        Parser[String] = "<" //
    val kwLeq:       Parser[String] = "<=" //
    val kwEq:        Parser[String] = "==" //
    val kwGt:        Parser[String] = ">" //
    val kwGeq:       Parser[String] = ">=" //
    val kwNeq:       Parser[String] = "!=" //
    val kwNot:       Parser[String] = "!" //

    val reserved: Parser[String] =
        kwClass  | kwExtends | kwStatic  | kwVoid  | kwInt       | kwBoolean | kwString |
        kwSkip   | kwReturn  | kwIf      | kwElse  | kwWhile     |
        kwThis   | kwNew     | kwTrue    | kwFalse | kwNull      |
        kwLog    | kwPrint   | kwPrintLn |
        kwBra    | kwKet     | kwSqBra   | kwSqKet | kwCurBra    | kwCurKet  |
        kwDot    | kwComma   | kwEquals  | kwColon | kwSemicolon | kwAtat    |
        kwConcat | kwPlus    | kwMinus   | kwMul   | kwDiv       | kwMod     |
        kwEq     | kwNeq     | kwLt      | kwLeq   | kwGt        | kwGeq     |
        kwAnd    | kwOr      | kwNot

    val name: Parser[String] = "[A-Z_a-z][A-Z_a-z0-9]*".r
    //val float: Parser[String] = """[0-9]+.[0-9]*""".r
    val libName: Parser[String] = "#[A-Z_a-z][A-Z_a-z0-9]*".r
    val id: Parser[String] = not(reserved) ~> name
    val native: Parser[String] = not(reserved) ~> libName ^^ { _ stripPrefix "#" }
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
            case _ ~ cname ~ extName ~ _ ~ fields ~ methods ~ _ =>
              Class(cname, extName, fields, methods)
          })

    def fieldDecl =
      positioned(
        kwStatic ~> _type ~ id ~ ((kwComma ~ id)*) ~ kwSemicolon ^^
          {
            case ty ~ n1 ~ others ~ _ =>
              val names = n1 :: (others map { case _ ~ n => n })
              FieldDecl(ty, names)
          })

    def _type: Parser[AnnotatedType] =
      positioned(
        (kwInt ^^ { _ => AnnotatedType(TyNum) }) |
          (kwBoolean ^^ { _ => AnnotatedType(TyBool) }) |
          (kwString ^^ { _ => AnnotatedType(TyString) }) |
          (id ^^ { id => AnnotatedType(TyType(id)) }))

    def methodDecl: Parser[MethodDecl] =
      positioned(
        if (!library)
          voidMethodDecl | retMethodDecl ^^ {
          case md =>
            md
        }
        else
          (annot?) ~ (voidMethodDecl | retMethodDecl) ^^ {
            case None ~ md => md
            case Some(annot) ~ md =>
              val annot_body: Annot = annotParser.AnnotationParser.parse(annot)
              md.copy(annot = Some(annot_body))
          })

    def voidMethodDecl =
      positioned(
        kwStatic ~> kwVoid ~ id ~ formals ~ block ^^
          {
            case _ ~ cname ~ formals ~ block =>
              MethodDecl(None, cname, formals, block, None)
          })

    def retMethodDecl =
      positioned(
        kwStatic ~> _type ~ id ~ formals ~ block ^^
          {
            case ty ~ cname ~ formals ~ block =>
              MethodDecl(Some(ty), cname, formals, block, None)
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
      positioned(_type ~ id ^^ { case ty ~ cid => Formal(ty, cid) })

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
      if (!library)
        positioned(skip | _return | assign | slog | sprint | scall | _if | _while | sblock)
      else
        positioned(skip | _return | assign | slog | sprint | scall | sNativeCall | _if | _while | sblock)

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
            case /*Left(id)*/ cid ~ _ ~ exp ~ _ => SAssign(cid, exp)
            //case Right(loc) ~ _ ~ exp ~ _ => SSetField(loc, exp)
          })

    def scall =
      positioned(bcall <~ kwSemicolon ^^
        {
          case (cid /*Left(id)*/ , acts) => SCall.create(cid, acts, fname)
          //case (Right(f), acts) => SMethodCall(f, acts)
        })


    def sNativeCall =
      positioned(bNativeCall <~ kwSemicolon ^^
        {
          case (cname, acts) => SNativeCall.create(cname, acts, fname)
        })

    def sprint: Parser[SPrint] =
      positioned(
        kwPrint ~> kwBra ~> expr <~ kwKet <~ kwSemicolon ^^ { SPrint(false, _) } |
        kwPrintLn ~> kwBra ~> expr <~ kwKet <~ kwSemicolon ^^ { SPrint(true, _) })

    def slog: Parser[SLog] =
      positioned(
        kwLog ~> kwBra ~> expr <~ kwKet <~ kwSemicolon ^^ { SLog })

    def bcall =
      mqid /*location*/ ~ actuals ^^
        {
          case cid /*Left(id)*/ ~ acts => (cid, acts)
          //case Right(loc) ~ acts => (Right(loc), acts)
        }

    def bNativeCall =
      native ~ actuals ^^
        {
          case cnative ~ acts => (cnative, acts)
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
      block ^^ { SBlock }

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
      (kwBra ~ expr ~ ((kwComma ~> expr)*) ~ kwKet) ^^
        { case _ ~ e1 ~ others ~ _ => e1 :: others }

    def expr: Parser[Expr] =
      if (!library)
        positioned(_this | _new | ecall | variable | unexp | binexp | elit | parexp)
      else
        positioned(_this | _new | ecall | eNativeCall | variable | unexp | binexp | elit | parexp)

    def parexp =
      kwBra ~> expr <~ kwKet ^^ { e => e }

    def variable =
      positioned(mqid ^^ {
          EVariable
          //case Left(name) => EVariable(name)
          //case Right(loc) => EGetField(loc)
        })

    def ecall =
      positioned(bcall ^^
        {
          case (cid /*Left(id)*/ , acts) => ECall.create(cid, acts, fname)
          //case (Right(f), acts) => EMethodCall(f, acts)
        })

    def eNativeCall =
      positioned(bNativeCall ^^
        {
          case (cname, acts) => ENativeCall.create(cname, acts, fname)
        })


    def _this =
      positioned(kwThis ^^ { _ => EThis })

    def _new =
      positioned(kwNew ~ id ~ actuals ^^ {
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
      positioned(
        kwConcat ^^ { l => BOPlusPlus.create(operatorAnnots("BOPlusPlus"), fname) } |
        kwPlus   ^^ { l => BOPlus.create(    operatorAnnots("BOPlus"), fname) } |
        kwMinus  ^^ { l => BOMinus.create(   operatorAnnots("BOMinus"), fname) } |
        kwMul    ^^ { l => BOMul.create(     operatorAnnots("BOMul"), fname) } |
        kwDiv    ^^ { l => BODiv.create(     operatorAnnots("BODiv"), fname) } |
        kwMod    ^^ { l => BOMod.create(     operatorAnnots("BOMod"), fname) } |
        kwAnd    ^^ { l => BOAnd.create(     operatorAnnots("BOAnd"), fname) } |
        kwEq     ^^ { l => BOEq.create(      operatorAnnots("BOEq"), fname) } |
        kwNeq    ^^ { l => BONeq.create(     operatorAnnots("BONeq"), fname) } |
        kwOr     ^^ { l => BOOr.create(      operatorAnnots("BOOr"), fname) } |
        kwLt     ^^ { l => BOLt.create(      operatorAnnots("BOLt"), fname) } |
        kwLeq    ^^ { l => BOLeq.create(     operatorAnnots("BOLeq"), fname) } |
        kwGt     ^^ { l => BOGt.create(      operatorAnnots("BOGt"), fname) } |
        kwGeq    ^^ { l => BOGeq.create(     operatorAnnots("BOGeq"), fname) })

    def unop =
      positioned(
        kwMinus ^^ { l => UNeg.create( operatorAnnots("UNeg"), fname) } |
        kwNot   ^^ { l => UNot.create( operatorAnnots("UNot"), fname) })

  }

  object FJPPParser {
    def parse(library: Boolean, funAnnots: Map[String, FunAnnot], text: String, fname: String): Program = {
      val parser = new FJPPParser(fname, library, funAnnots)
      parser.parseAll(parser.program, text) match {
        case parser.Success(lup, _) =>
            lup match {
              case p: Program => p
              case _ => throw new Unexpected("The parsed file IS a program.") }
        case parser.Error(msg, next) =>
          throw ParseError("Parse of %s failed with ERROR at %s\nwith message %s" format (fname, next.pos.toString(), msg))
        case parser.Failure(msg, next) =>
          throw ParseError("Parse of %s failed at %s\nwith message %s" format (fname, next.pos.toString(), msg))
        }
      }
    }
  }
