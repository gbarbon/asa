
package it.unive.dais.yaasa
/**
 * @author esteffin
 */

import scala.util.parsing.input._
import absyn._
import utils.prelude._
import utils.env._
//import scala.collection.breakOut

object qualifiedRename {

  private type QVarEnv = Env[String, String]
  private type QFunEnv = Env[String, String]

  case class RenameException(_message: string) extends MessageException {
    val message = "RenameException exception: %s" format _message
    /*def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))*/
  }

  def qualifyProgram(program: Program) =
    program match {
      case Program(classes) =>
        Program(classes map qualifyClass).setPos(program.pos)
    }

  private def qualifyClass(c: Class): Class =
    c match {
      case Class(name, ext, fields, methods) =>
        val venv = Env((for (FieldDecl(_, ns) <- fields; n <- ns) yield (n, "%s.%s" format (name, n)))toMap)
        val fenv = Env((for (MethodDecl(_, n, _, _, _) <- methods) yield (n, "%s.%s" format (name, n)))toMap)
        c.copy(methods = c.methods map { qualifyMethod(venv, fenv, _) })
    }

  private def qualifyMethod(venv: QVarEnv, fenv: QFunEnv, m: MethodDecl): MethodDecl =
    m match {
      case MethodDecl(retTy, name, formals, body, _) =>
        val binded = for (Formal(_, name) <- formals) yield name
        if (binded.exists { v => venv.keys.exists { _ == v } })
          throw RenameException("Trying to re-define a variable in the same class at %s" format m.loc)
        else
          m.copy(body = qualifyBlock(venv, fenv, body))

    }

  private def qualifyBlock(venv: QVarEnv, fenv: QFunEnv, b: Block) =
    b match {
      case Block(vars, stmts) =>
        val binded = for (VarDecl(_, names) <- vars; name <- names) yield name
        if (binded.exists { v => venv.keys exists { _ == v } })
          throw RenameException("Trying to re-define a variable in the same class at %s" format b.loc)
        else
          b copy (stmts = stmts map { qualifyStmt(venv, fenv, _) })

    }

  private def qualifyStmt(venv: QVarEnv, fenv: QFunEnv, stmt: Stmt): Stmt =
    stmt match {
      case s @ SAssign(x, e) =>
        if (venv.keys exists { _ == x })
          s copy (name = venv lookup x) //SAssign(venv lookup x, e).setPos(stmt.pos)
        else
          stmt
      case s @ SIf(c, thn, els) =>
        SIf(qualifyExpr(venv, fenv, c), qualifyStmt(venv, fenv, thn), qualifyStmt(venv, fenv, els)).setPos(stmt.pos)
      case SWhile(c, body) =>
        SWhile(qualifyExpr(venv, fenv, c), qualifyStmt(venv, fenv, body)).setPos(stmt.pos)
      case SCall(name, actuals) =>
        val nname = if (fenv.keys exists { _ == name }) fenv lookup name else name
        SCall(nname, actuals map { qualifyExpr(venv, fenv, _) }).setPos(stmt.pos)
      case SBlock(block)      => SBlock(qualifyBlock(venv, fenv, block)).setPos(stmt.pos)
      case SReturn(Some(e))   => SReturn(Some(qualifyExpr(venv, fenv, e))).setPos(stmt.pos)
      case SPrint(ln, actual) => SPrint(ln, qualifyExpr(venv, fenv, actual)).setPos(stmt.pos)
      case _                  => stmt
    }

  private def qualifyExpr(venv: QVarEnv, fenv: QFunEnv, expr: Expr): Expr =
    expr match {
      case EVariable(x) =>
        if (venv.keys exists { _ == x })
          EVariable(venv lookup x).setPos(expr.pos)
        else
          expr
      case ECall(name, actuals) =>
        val nname = if (fenv.keys exists { _ == name }) fenv lookup name else name
        ECall(nname, actuals map { qualifyExpr(venv, fenv, _) }).setPos(expr.pos)
      case EBExpr(op, l, r) => EBExpr(op, qualifyExpr(venv, fenv, l), qualifyExpr(venv, fenv, r)).setPos(expr.pos)
      case EUExpr(op, e)    => EUExpr(op, qualifyExpr(venv, fenv, e)).setPos(expr.pos)
      case _                => expr
    }

}
