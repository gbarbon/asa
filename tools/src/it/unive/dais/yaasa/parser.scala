package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import it.unive.dais.yaasa.datatype.ABSValue._
import it.unive.dais.yaasa.utils.prelude.Unexpected

import scala.util.parsing.combinator.PackratParsers

//import scala.util.parsing.combinator._
import scala.util.parsing.combinator.RegexParsers
//import scala.util.Either
import it.unive.dais.yaasa.utils.parsingUtils._
import it.unive.dais.yaasa.datatype.FortyTwo._
import it.unive.dais.yaasa.absyn._

object parser {

  class FJPPParser(fname: String, library: Boolean = false, operatorAnnots: Map[String, FunAnnot])
    extends RegexParsers with PackratParsers {

    type P[+A] = PackratParser[A]

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
    val kwLog:       Parser[String] = "log\\b".r
    val kwPrint:     Parser[String] = "print\\b".r
    val kwPrintLn:   Parser[String] = "println\\b".r
    val kwStatic:    Parser[String] = "static\\b".r
    val kwLength:    Parser[String] = "len\\b".r
    val kwToCharArr: Parser[String] = "toCharArray\\b".r
    val kwBra:       Parser[String] = "(" //
    val kwKet:       Parser[String] = ")" //
    val kwSqBra:     Parser[String] = "[" //
    val kwSqKet:     Parser[String] = "]" //
    val kwSqBraKet:  Parser[String] = "[]" //
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
        kwBra    | kwKet     | kwSqBra   | kwSqKet | kwCurBra    | kwCurKet  |
        kwDot    | kwComma   | kwEquals  | kwColon | kwSemicolon | kwAtat    |
        kwConcat | kwPlus    | kwMinus   | kwMul   | kwDiv       | kwMod     |
        kwEq     | kwNeq     | kwLt      | kwLeq   | kwGt        | kwGeq     |
        kwAnd    | kwOr      | kwNot     |           kwToCharArr

    val reserved_call: Parser[String] =
      reserved | kwLog | kwPrint   | kwPrintLn | kwLength

    //val float: Parser[String] = """[0-9]+.[0-9]*""".r
    val name:     Parser[String] = "[A-Z_a-z][A-Z_a-z0-9]*".r
    val libName:  Parser[String] = "#[A-Z_a-z][A-Z_a-z0-9]*".r
    val callName: Parser[String] = not(reserved_call) ~> name
    val id:       Parser[String] = not(reserved) ~> name
    val native:   Parser[String] = not(reserved) ~> libName ^^ { _ stripPrefix "#" }
    val qid:      Parser[String] = id ~ kwDot ~ id ^^ { case n1 ~ _ ~ n2 => "%s.%s" format (n1, n2) }
    val mqid:     Parser[String] = qid | id
    val callQid:  Parser[String] = id ~ kwDot ~ callName ^^ { case n1 ~ _ ~ n2 => "%s.%s" format (n1, n2) }
    val mCallqid: Parser[String] = qid | callName
    //val annid: Parser[String] = float | mqid

    def _true = positioned(kwTrue ^^ { _ => BoolLit(true) })
    def _false = positioned(kwFalse ^^ { _ => BoolLit(false) })
    def _null = positioned(kwNull ^^ { _ => NullLit })
    def integer = positioned("""(-?)(0|[1-9]\d*)""".r ^^ { i => IntLit(i.toInt) })
    def string = positioned("""\"[^"]*\"""".r ^^ { s => StringLit(s.substring(1, s.length - 1)) })
    def annot = """@@\[[^\[\]]*\]""".r ^^ { s => s }

    lazy val program: P[Program] = (_class*) ^^ { classes => Program(classes) }

    lazy val _class: P[Class] =
      positioned(
        kwClass ~ id ~ ((kwExtends ~> id)?) ~ kwCurBra ~ (fieldDecl*) ~ (methodDecl*) ~ kwCurKet ^^
          {
            case _ ~ cname ~ extName ~ _ ~ fields ~ methods ~ _ =>
              Class(cname, extName, fields, methods)
          })

    lazy val fieldDecl: P[FieldDecl] =
      positioned(
        kwStatic ~> _type ~ id ~ ((kwComma ~ id)*) ~ kwSemicolon ^^
          {
            case ty ~ n1 ~ others ~ _ =>
              val names = n1 :: (others map { case _ ~ n => n })
              FieldDecl(ty, names)
          })

    lazy val _type: P[AnnotatedType] =
      positioned(
          _base_type ~ (kwSqBraKet*) ^^ {
            case bt ~ bk =>
              val ty = bk.foldLeft(bt.ty){ (t, _) => TyArray(t) }
              AnnotatedType(ty) })

    lazy val _base_type: P[AnnotatedType] =
      positioned(
          (kwInt ^^ { _ => AnnotatedType(TyNum) }) |
          (kwBoolean ^^ { _ => AnnotatedType(TyBool) }) |
          (kwString ^^ { _ => AnnotatedType(TyString) }) /*|
          (id ^^ { id => AnnotatedType(TyType(id)) })*/)

    lazy val methodDecl: P[MethodDecl] =
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

    lazy val voidMethodDecl: P[MethodDecl] =
      positioned(
        kwStatic ~> kwVoid ~ callName ~ formals ~ block ^^
          {
            case _ ~ cname ~ formals ~ block =>
              MethodDecl(None, cname, formals, block, None)
          })

    lazy val retMethodDecl: P[MethodDecl] =
      positioned(
        kwStatic ~> _type ~ callName ~ formals ~ block ^^
          {
            case ty ~ cname ~ formals ~ block =>
              MethodDecl(Some(ty), cname, formals, block, None)
          })

    lazy val formals: P[List[Formal]] =
      (kwBra ~ kwKet ^^ { _ => List() }) |
        (kwBra ~> formal <~ kwKet ^^ { List(_) }) |
        (kwBra ~ formal ~ ((kwComma ~> formal)*) ~ kwKet ^^
          {
            case _ ~ f ~ others ~ _ =>
              f :: others
          })

    lazy val formal: P[Formal] =
      positioned(_type ~ id ^^ { case ty ~ cid => Formal(ty, cid) })

    lazy val block: P[Block] =
      positioned(
        kwCurBra ~ (varDecl*) ~ (statement*) ~ kwCurKet ^^ {
            case _ ~ vars ~ stmts ~ _ =>
              Block(vars, stmts)
          })

    lazy val varDecl: P[VarDecl] =
      positioned(
        _type ~ id ~ ((kwComma ~ id)*) ~ kwSemicolon ^^
          {
            case ty ~ n1 ~ others ~ _ =>
              val names = n1 :: (others map { case _ ~ n => n })
              VarDecl(ty, names)
          })

    lazy val statement: P[Stmt] =
      if (!library)
        positioned(skip | _return | assign | slog | sprint | scall | _if | _while | sblock)
      else
        positioned(skip | _return | assign | slog | sprint | scall | sNativeCall | _if | _while | sblock)

    lazy val loc: P[(String, List[Expr])] =
      mqid ~ ((kwSqBra ~> expr <~ kwSqKet)*) ^^ {
        case n ~ indexes =>
          if (indexes.length > 1) throw new Unexpected("Arrays with multiple indices are not allowed...")
          else (n, indexes) }

    lazy val skip: P[SSkip.type] =
      positioned(
        kwSkip ~ kwSemicolon ^^
          { _ =>
            SSkip
          })

    lazy val assign: P[Stmt] =
      positioned(
        loc ~ kwEquals ~ expr ~ kwSemicolon ^^
          {
            case (n, List()) ~ _ ~ exp ~ _ => SAssign(n, exp)
            case (n, indexes) ~ _ ~ exp ~ _ => SArrayAssign(n, indexes, exp)
            //case Right(loc) ~ _ ~ exp ~ _ => SSetField(loc, exp)
          })
/*
    lazy val array_set: P[SArrayAssign] =
      positioned(
        expr ~ kwSqBra ~ expr ~ kwSqKet ~ kwEquals ~ expr ~ kwSemicolon ^^ {
          case n ~ _ ~ idx ~ _ ~ _ ~ value ~ _ => SArrayAssign(n, idx, value)
        }
      )*/

    lazy val scall: P[SCall] =
      positioned(bcall <~ kwSemicolon ^^
        {
          case (cid /*Left(id)*/ , acts) => SCall.create(cid, acts, fname)
          //case (Right(f), acts) => SMethodCall(f, acts)
        })


    lazy val sNativeCall: P[SNativeCall] =
      positioned(bNativeCall <~ kwSemicolon ^^
        {
          case (cname, acts) => SNativeCall.create(cname, acts, fname)
        })

    lazy val sprint: P[SPrint] =
      positioned(
        kwPrint ~> kwBra ~> expr <~ kwKet <~ kwSemicolon ^^ { SPrint(false, _) } |
        kwPrintLn ~> kwBra ~> expr <~ kwKet <~ kwSemicolon ^^ { SPrint(true, _) })

    lazy val slog: P[SLog] =
      positioned(
        kwLog ~> kwBra ~> expr <~ kwKet <~ kwSemicolon ^^ { SLog })

    lazy val bcall: P[(String, List[Expr])] =
      mCallqid /*location*/ ~ actuals ^^
        {
          case cid /*Left(id)*/ ~ acts => (cid, acts)
          //case Right(loc) ~ acts => (Right(loc), acts)
        }

    lazy val bNativeCall: P[(String, List[Expr])] =
      native ~ actuals ^^
        {
          case cnative ~ acts => (cnative, acts)
          //case Right(loc) ~ acts => (Right(loc), acts)
        }

    lazy val _return: P[SReturn] =
      positioned(kwReturn ~ kwSemicolon ^^ { _ =>
        SReturn(None)
      }) |
      positioned(kwReturn ~ expr ~ kwSemicolon ^^ {
        case _ ~ e ~ _ =>
          SReturn(Some(e))
      })

    lazy val _if: P[Stmt] =
      positioned(kwIf ~ kwBra ~ expr ~ kwKet ~ /*kwThen ~*/ statement ~ kwElse ~ statement ^^
        { case _ ~ _ ~ cond ~ _ ~ /*_ ~*/ thn ~ _ ~ els => SIf(cond, thn, els) })

    lazy val _while: P[Stmt] =
      positioned(kwWhile ~ kwBra ~ expr ~ kwKet ~ statement ^^
        { case _ ~ _ ~ cond ~ _ ~ body => SWhile(cond, body) })

    lazy val sblock: P[Stmt] =
      block ^^ { SBlock }

    /*lazy val location: P[Either[String, Field]] =
      idLoc | fieldLoc

    lazy val idLoc: P[Either[String, Field]] =
      id ^^
        { l => Left(l) }

    lazy val fieldLoc: P[Either[String, Field]] =
      floc ^^ { Right(_) }

    lazy val floc =
      positioned("(" ~> expr ~ ")" ~ "." ~ id ^^
        { case exp ~ _ ~ _ ~ fn => Field(exp, fn) })*/

    lazy val actuals: P[List[Expr]] =
      (kwBra ~ kwKet ^^ { _ => List() }) |
      (kwBra ~ expr ~ ((kwComma ~> expr)*) ~ kwKet) ^^
        { case _ ~ e1 ~ others ~ _ => e1 :: others }

    lazy val expr: P[Expr] =
      if (!library)
        positioned(binexp | _this | eAarrayNew | eArrayLength | /*_new |*/ toCharArray | ecall | variable | unexp | elit | parexp)
      else
        positioned(binexp | _this | eAarrayNew | eArrayLength | /*_new |*/ toCharArray | ecall | eNativeCall | variable | unexp | elit | parexp)

    lazy val toCharArray: P[EToCharArray] =
      positioned(
        kwToCharArr ~ kwBra ~> expr <~ kwKet ^^ { EToCharArray.create(_, fname) })

    lazy val parexp: P[Expr] =
      kwBra ~> expr <~ kwKet ^^ { e => e }

    lazy val variable: P[Expr] =
      positioned(loc ^^ {
        case (v, List()) => EVariable(v)
        case (v, indexes) => EArrayGet(v, indexes)
          //case Left(name) => EVariable(name)
          //case Right(loc) => EGetField(loc)
        })

    lazy val ecall: P[ECall] =
      positioned(bcall ^^
        {
          case (cid /*Left(id)*/ , acts) => ECall.create(cid, acts, fname)
          //case (Right(f), acts) => EMethodCall(f, acts)
        })

    lazy val eAarrayNew: P[EArrayNew] =
      positioned(
        kwNew ~> _type ~ kwSqBra ~ integer ~ kwSqKet ^^ {
          case ty ~ _ ~ dim ~ _ =>
            EArrayNew(ty, dim)
        })

    lazy val eArrayLength: P[EArrayLength] = {
      positioned(
        kwLength ~ kwBra ~> expr <~ kwKet ^^ {
          arr => EArrayLength(arr)
        })
    }

    lazy val eNativeCall: P[ENativeCall] =
      positioned(bNativeCall ^^
        {
          case (cname, acts) => ENativeCall.create(cname, acts, fname)
        })


    lazy val _this: P[EThis.type] =
      positioned(kwThis ^^ { _ => EThis })

    /*lazy val _new =
      positioned(kwNew ~ id ~ actuals ^^ {
          case _ ~ ty ~ acts => ENew(ty, acts)
        })*/

    lazy val binexp: P[EBExpr] =
      positioned(expr ~ binop ~ expr ^^
        { case l ~ op ~ r => EBExpr(op, l, r) })

    lazy val unexp: P[EUExpr] =
      positioned(unop ~ expr ^^
        { case op ~ e => EUExpr(op, e) })

    lazy val elit: P[ELit] = positioned(lit ^^ { l => ELit(l) })

    lazy val lit: P[Literal] =
      positioned(_true | _false | _null | integer | string)

    lazy val binop: P[BOperator] =
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

    lazy val unop: P[UOperator] =
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
