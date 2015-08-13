package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import utils.prelude._
import utils.pretty_print._
import utils.env._
import absyn._
import scala.collection.breakOut

object evaluator {
  case class EvaluationException(_message: string) extends Exception {
    val message = "Evaluation exception: %s" format _message
    /*def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))*/
    override def toString(): String = message
  }
  trait ConcreteValue {
    val value: Any
    val ty: Type
  }

  type EvEnv = Env[String, ConcreteValue]

  case class IntValue(value: Int) extends ConcreteValue {
    def this() = this(0)
    val ty = TyInt()
    override def toString() = "%d" format value
  }
  case class BoolValue(value: Boolean) extends ConcreteValue {
    def this() = this(false)
    val ty = TyBool()
    override def toString() = "%b" format value
  }
  case class StringValue(value: String) extends ConcreteValue {
    def this() = this(null) //Only for compatibility with the horrendous java
    val ty = TyString()
    override def toString() = "%s" format value
  }
  case class UnitValue() extends ConcreteValue {
    val ty = TyType("Unit")
    val value = throw new EvaluationException("Cannot access unit value")
    override def toString() = "()"
  }

  def evaluateProgram(program: Program) =
    {
      program.classes match {
        case List() => throw new Unexpected("Empty class definition.")
        case c :: _ =>
          c.methods match {
            case List() => throw new Unexpected("No methods in first class :%s.", c.name)
            case m :: _ =>
              {
                //val env = new Env[String, ConcreteValue]()
                evaluateBlock(new Env[String, ConcreteValue](), m.body) //(env)
              }
          }
      }
    }

  def evaluateBlock(env: EvEnv, block: Block): (Option[ConcreteValue], EvEnv) = //(env: Env[String, ConcreteValue]) =
    {
      val nenv: List[(id, ConcreteValue)] =
        (for (vd <- block.varDecls)
          yield (vd.id,
          vd.ty match {
            case TyInt()    => new IntValue()
            case TyBool()   => new BoolValue()
            case TyString() => new StringValue()
            case _          => throw new Unexpected("Variable %s has not supported type %s", (vd.id, vd.ty))
          }))

      val (ret, fenv) = block.stmts.foldLeft(None: Option[ConcreteValue], env binds nenv) {
        case (ret @ (Some(_), env), stmt) => ret
        case ((None, env), stmt)          => evaluateStmt(env, stmt)
      }
      (ret, env update_values (fenv))
      //evaluateStmts(env, block.stmts)
    }
  /*def evaluateStmts(env: EvEnv, stmts: List[Stmt]): (Option[ConcreteValue], EvEnv) =
    stmts match {
      case List() => (None, env)
      case (retStmt @ SReturn(r)) :: List() =>
        evaluateReturn(env, retStmt)
      case (retStmt @ SReturn(r)) :: _ =>
        //warning: to be improved...
        //throw new EvaluationException("Return should be at end of the block at %O", retStmt.pos)
        //Just ignoring further statements
        evaluateReturn(env, retStmt)
      case stmt :: stmts =>
        evaluateStmt(env, stmt) match {
          case (None, nenv) =>
            evaluateStmts(nenv, stmts)
          case ret => ret //if the statement has returned something we just stop further evaluations
        }
    }
  def evaluateReturn(env: EvEnv, retStmt: SReturn): (Option[ConcreteValue], EvEnv) =
    retStmt match {
      case SReturn(None) => (Some(UnitValue()), env)
      case SReturn(Some(e)) =>
        val (nenv, res) = evaluateExpr(env, e)
        (Some(res), nenv)
    }
    */
  def evaluateStmt(env: EvEnv, stmt: Stmt): (Option[ConcreteValue], EvEnv) =
    stmt match {
      case SSkip() => (None, env)
      case SAssign(x, e) =>
        val (res, nenv) = evaluateExpr(env, e)
        if (res.ty != nenv.lookup(x).ty)
          throw new EvaluationException("Type error: variable %s has type %s, but is given type %s at %s".format(x, nenv.lookup(x).ty, res.ty, stmt.pos.toString()))
        else
          (None, nenv.update(x) { _ => res })
      case SIf(c, thn, els) =>
        val (cond, nenv) = evaluateExpr(env, c)
        cond match {
          case BoolValue(v) =>
            if (v)
              evaluateBlock(nenv, thn)
            else
              evaluateBlock(nenv, els)
          case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.pos)
        }
      case SWhile(c, body) =>
        val (cond, nenv) = evaluateExpr(env, c)
        cond match {
          case BoolValue(v) =>
            if (v) {
              evaluateBlock(nenv, body) match {
                case (None, wenv)         => evaluateStmt(wenv, stmt)
                case ret @ (Some(_), env) => ret
              }
            }
            else
              (None, nenv)
          case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.pos)
        }
      case SReturn(None) => (Some(UnitValue()), env)
      case SReturn(Some(e)) =>
        val (res, nenv) = evaluateExpr(env, e)
        (Some(res), nenv)
      //case rets @ SReturn(_) => evaluateReturn(env, rets)
      case SMethodCall(_, _) => throw new NotSupportedException("Statement Method Call not supported at %s" format stmt.pos)
      case SCall(_, _)       => throw new NotSupportedException("Statement Call not supported at %s" format stmt.pos)
      case SSetField(_, _)   => throw new NotSupportedException("Set field not supported at %s" format stmt.pos)
    }

  def evaluateExpr(env: EvEnv, expr: Expr): (ConcreteValue, EvEnv) =
    expr match {
      case EVariable(x) =>
        (env.lookup(x), env)
      case EBExpr(op, l, r) =>
        val (lv, nenv) = evaluateExpr(env, l)
        val (rv, fenv) = evaluateExpr(env, r)
        try
          ((evaluateBinOp(op, lv, rv), fenv))
        catch {
          case EvaluationException(_) =>
            throw new EvaluationException("The evaluation of the binary expression has wrong arguments type at %s" format expr.pos)
        }
      case EUExpr(op, e) =>
        val (v, nenv) = evaluateExpr(env, e)
        try
          ((evaluateUnOp(op, v), nenv))
        catch {
          case EvaluationException(_) =>
            throw new EvaluationException("The evaluation of the unary expression has wrong arguments type at %s" format expr.pos)
        }
      case ELit(IntLit(v))    => (IntValue(v), env)
      case ELit(BoolLit(v))   => (BoolValue(v), env)
      case ELit(StringLit(v)) => (StringValue(v), env)
      case ELit(NullLit())    => throw new NotSupportedException("Expression \"null\" not supported at %O", expr.pos)
      case ENew(_, _)         => throw new NotSupportedException("Expression New not supported at %O", expr.pos)
      case EThis()            => throw new NotSupportedException("Expression This not supported at %O", expr.pos)
      case EMethodCall(_, _)  => throw new NotSupportedException("Expression Method Call not supported at %O", expr.pos)
      case ECall(_, _)        => throw new NotSupportedException("Expression Call not supported at %O", expr.pos)
      case EGetField(_)       => throw new NotSupportedException("Get Field Expression not supported at %O", expr.pos)
    }
  def evaluateBinOp(op: BOperator, lv: ConcreteValue, rv: ConcreteValue): ConcreteValue =
    (lv, rv) match {
      case (IntValue(l), IntValue(r)) =>
        op match {
          case BOPlus()  => IntValue(l + r)
          case BOMinus() => IntValue(l - r)
          case BOMul()   => IntValue(l * r)
          case BODiv()   => IntValue(l / r)
          case BOMod()   => IntValue(l % r)
          case BOEq()    => BoolValue(l == r)
          case BONeq()   => BoolValue(l != r)
          case BOLt()    => BoolValue(l < r)
          case BOLeq()   => BoolValue(l <= r)
          case BOGt()    => BoolValue(l > r)
          case BOGeq()   => BoolValue(l >= r)
          case _         => throw new EvaluationException("Type mismatch on binary operation")
        }
      case (StringValue(l), StringValue(r)) =>
        op match {
          case BOPlus() => StringValue(l + r)
          case BOEq()   => BoolValue(l == r)
          case BONeq()  => BoolValue(l != r)
          case BOLt()   => BoolValue(l < r)
          case BOLeq()  => BoolValue(l <= r)
          case BOGt()   => BoolValue(l > r)
          case BOGeq()  => BoolValue(l >= r)
          case _        => throw new EvaluationException("Type mismatch on binary operation")
        }
      case (BoolValue(l), BoolValue(r)) =>
        op match {
          case BOAnd() => BoolValue(l && r)
          case BOOr()  => BoolValue(l || r)
          case BOEq()  => BoolValue(l == r)
          case BONeq() => BoolValue(l != r)
          /*case BOLt()  => BoolValue(l < r)
          case BOLeq() => BoolValue(l <= r)
          case BOGt()  => BoolValue(l > r)
          case BOGeq() => BoolValue(l >= r)*/
          case _       => throw new EvaluationException("Type mismatch on binary operation")
        }
    }

  def evaluateUnOp(op: UOperator, v: ConcreteValue): ConcreteValue =
    v match {
      case IntValue(i) =>
        op match {
          case UNeg() => IntValue(-i)
          case _      => throw new EvaluationException("Type mismatch on unary operation")
        }
      case BoolValue(b) =>
        op match {
          case UNot() => BoolValue(!b)
          case _      => throw new EvaluationException("Type mismatch on unary operation")
        }
      case _ => throw new EvaluationException("Type mismatch on unary operation")
    }
}

