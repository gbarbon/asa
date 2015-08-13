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
    val message = "Evaluation exception:" format _message
    def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))
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
  }
  case class BoolValue(value: Boolean) extends ConcreteValue {
    def this() = this(false)
    val ty = TyBool()
  }
  case class StringValue(value: String) extends ConcreteValue {
    def this() = this(null) //Only for compatibility with the horrendous java
    val ty = TyString()
  }
  case class UnitValue() extends ConcreteValue {
    val ty = TyType("Unit")
    val value = throw new EvaluationException("Cannot access unit value")
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
      val nenv: Env[id, ConcreteValue] =
        Env(
          (for (vd <- block.varDecls)
            yield (vd.id,
            vd.ty match {
              case TyInt()    => new IntValue()
              case TyBool()   => new BoolValue()
              case TyString() => new StringValue()
              case _          => throw new Unexpected("Variable %s has not supported type %s", (vd.id, vd.ty))
            }))(breakOut))
      val (ret, fenv) = block.stmts.foldLeft(None: Option[ConcreteValue], nenv) {
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
      case SAssign(n, e) =>
        val (nenv, res) = evaluateExpr(env, e)
        (None, nenv.update(n) { _ => res })
      case SIf(c, thn, els) =>
        val (nenv, cond) = evaluateExpr(env, c)
        cond match {
          case BoolValue(v) =>
            if (v)
              evaluateBlock(nenv, thn)
            else
              evaluateBlock(nenv, els)
          case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %O", stmt.pos)
        }
      case SWhile(c, body) =>
        val (nenv, cond) = evaluateExpr(env, c)
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
          case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %O", stmt.pos)
        }
      case SReturn(None) => (Some(UnitValue()), env)
      case SReturn(Some(e)) =>
        val (nenv, res) = evaluateExpr(env, e)
        (Some(res), nenv)
      //case rets @ SReturn(_) => evaluateReturn(env, rets)
      case SMethodCall(_, _) => throw new NotSupportedException("Statement  Method Call not supported at %O", stmt.pos)
      case SCall(_, _)       => throw new NotSupportedException("Statement Call not supported at %O", stmt.pos)
      case SSetField(_, _)   => throw new NotSupportedException("Set field not supported at %O", stmt.pos)
    }

  def evaluateExpr(env: EvEnv, expr: Expr): (EvEnv, ConcreteValue) =
    (env, new IntValue(): ConcreteValue)
}

