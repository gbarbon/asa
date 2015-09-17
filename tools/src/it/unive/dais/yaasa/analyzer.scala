package it.unive.dais.yaasa

/**
 * @author esteffin
 * @author gbarbon
 */

import utils.prelude._
import utils.pretty_print._
import utils.env._
import absyn._
import scala.collection.breakOut
import functConvert._
import it.unive.dais.yaasa.datatype.ADType._
import it.unive.dais.yaasa.datatype.types._

/**
 *
 */
object analyzer {

  case class EvaluationException(_message: string) extends MessageException {
    val message = "Evaluation exception: %s" format _message
    /*def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))*/
  }
  trait ConcreteValue {
    val value: Any
    val ty: Type
  }

  type ValueWAbstr = (ConcreteValue, ADInfo)

  type EvEnv = Env[String, ValueWAbstr]

  type MethInfo = Env[String, (MethodDecl, EvEnv)]

  case class IntValue(value: Int) extends ConcreteValue {
    def this() = this(0)
    val ty = TyInt
    override def toString() = "%d" format value
  }
  case class BoolValue(value: Boolean) extends ConcreteValue {
    def this() = this(false)
    val ty = TyBool
    override def toString() = "%b" format value
  }
  case class StringValue(value: String) extends ConcreteValue {
    def this() = this(null) //Only for compatibility with the horrendous java
    val ty = TyString
    override def toString() = "%s" format value
  }
  case class UnitValue() extends ConcreteValue {
    val ty = TyType("Unit")
    val value = throw new EvaluationException("Cannot access unit value")
    override def toString() = "()"
  }

  class Analyzer(program: Program) {
    /**
     * Must require the insertion of the confidential labels before the execution.
     *
     * 1) The user must manually insert all the confidential label used in the program.
     * 2) Or we must introduce a way to locate them in the code.
     *    But, maybe it is faster to insert the list of all the confidential labels before,
     *    rather than searching them inside the code and enrich the code with something that locate the label.
     * 3) We can think to load both the code file with a label file.
     * 4) Or, we can modify the code and insert at the beginning the list of confidential labels. <---
     * 5) or we can recognize them with a function readLabel <---
     * --> OR GIVE ALL THE OPTIONS TO THE USER <--
     *
     * So at the begin, all the label objects are created.
     */

    private val ctx: MethInfo =
      program match {
        case Program(List()) => throw new EvaluationException("Empty class definition.")
        case Program(classes) =>
          val venv: EvEnv =
            Env(
              ((for (Class(name, _, fields, _) <- classes)
                yield createVars(fields map { case FieldDecl(ty, ns) => (ty, ns map { "%s.%s" format (name, _) }) })) flatten)toMap)

          Env(
            (for (Class(cname, _, _, methods) <- classes; m <- methods)
              yield ("%s.%s" format (cname, m.name), (m, venv))) toMap)
      }

    /**
     * @param
     * @return
     */
    def evaluateProgram() =
      {
        ctx search_by_key { _ endsWith ".main" } match {
          case Some(main) => evaluateCall(main, List())
          case None       => throw new EvaluationException("No main found...")
        }
      }

    def evaluateCall(call: (MethodDecl, EvEnv), actuals: List[ValueWAbstr]): (Option[ValueWAbstr], EvEnv) =
      {
        val (md, env) = call

        if (md.formals.length != actuals.length)
          throw new EvaluationException("Function %s is called with wrong argument number")

        val form_bind =
          for ((form, act) <- md.formals.zip(actuals))
            yield (
            if (form.ty != act._1.ty)
              throw new EvaluationException("Type error in method %s: formal %s has type %s, but is given type %s at %s".format(md.name, form.ty, act._1.ty, md.loc))
            else
              (form.name, act))
        val (ret, fenv) = evaluateBlock(env binds_new form_bind, md.body)
        //val adexp = actuals.head
        val new_ret = (ret, md.annot) match {
          case (None, _)   => ret
          case (ret, None) => ret
          case (Some((retv, retLab)), Some(fannot)) =>
            fannot match {
              case annot @ FunAnnot(_, _, _) => //@FIXME: implement new update method
              //val list_stm: List[EStatement] = actuals map { case (_, x) => Statement.sCreator(x.label, annot) }
              //Some(retv, list_stm.foldLeft(actuals.head._2) { case (acc, stm) => acc.addExpStm(stm) }) //@FIXME: matrix...
              //@FIXME: we should use update method with list of label, but there are no label to use as "this"...
              case lab: LabelAnnot           => Some((retv, CADInfo.Factory.newInfoFromAnnot(lab)))
              case _                         => throw new Unexpected("Unknown annotation type %s." format fannot.toString)
            }
        }
        //(new_ret, env update_values fenv)
        null
      }

    /**
     * Create the set of fields in a class, all empty labels
     */
    def createVars(vars: List[(Type, List[string])]): List[(string, ValueWAbstr)] =
      for ((ty, names) <- vars; name <- names)
        yield (name,
        ty match {
          case TyInt    => (new IntValue(), CADInfo.Factory.star)
          case TyBool   => (new BoolValue(), CADInfo.Factory.star)
          case TyString => (new StringValue(), CADInfo.Factory.star)
          case _        => throw new Unexpected("Variable %s has not supported type %s", (name, ty))
        })

    /**
     * Evaluate the block
     */
    def evaluateBlock(env: EvEnv, block: Block): (Option[ValueWAbstr], EvEnv) = //(env: Env[String, ConcreteValue]) =
      {
        val nenv: List[(id, ValueWAbstr)] = createVars(block.varDecls map { vd => (vd.ty, vd.ids) })

        val (ret, fenv) = block.stmts.foldLeft(None: Option[ValueWAbstr], env binds_new nenv) {
          case (ret @ (Some(_), _), stmt) => ret
          case ((None, env), stmt)        => evaluateStmt(env, stmt)
        }
        (ret, env update_values fenv)
        //evaluateStmts(env, block.stmts)
      }

    def evaluateStmt(env: EvEnv, stmt: Stmt): (Option[ValueWAbstr], EvEnv) =
      stmt match {
        case SSkip => (None, env)
        case SAssign(x, e) =>
          val (res, nenv) = evaluateExpr(env, e)
          if (res._1.ty != nenv.lookup(x)._1.ty)
            throw new EvaluationException("Type error: variable %s has type %s, but is given type %s at %s".format(x, nenv.lookup(x)._1.ty, res._1.ty, stmt.loc.toString()))
          else
            (None, nenv.update(x) { _ => res })
        case SIf(c, thn, els) => //@TODO: collect the implicit!!
          val (cond, nenv) = evaluateExpr(env, c)
          cond._1 match {
            case BoolValue(v) =>
              if (v)
                evaluateStmt(nenv, thn)
              else
                evaluateStmt(nenv, els)
            case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
          }
        case SWhile(c, body) => //@TODO: collect the implicit!!
          val (cond, nenv) = evaluateExpr(env, c)
          cond._1 match {
            case BoolValue(v) =>
              if (v) {
                evaluateStmt(nenv, body) match {
                  case (None, wenv)       => evaluateStmt(wenv, stmt)
                  case ret @ (Some(_), _) => ret
                }
              }
              else
                (None, nenv)
            case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
          }
        case SBlock(block) => evaluateBlock(env, block)
        case SReturn(None) => ((Some(UnitValue(), CADInfo.Factory.star)), env) //@FIXME: label.empty is not correct!
        case SReturn(Some(e)) =>
          val (res, nenv) = evaluateExpr(env, e)
          (Some(res), nenv)
        case SPrint(ln, actual) =>
          val (vactual, nenv) = evaluateExpr(env, actual)
          if (ln) println(vactual._1.value) else print(vactual._1.value)
          (None, nenv)
        case SCall(name, actuals) =>
          applyCall(env, name, actuals) match {
            case (Some(_), env) => (None, env)
            case (None, env)    => (None, env) //@FIXME: URGENT!!!
          }

        //case rets @ SReturn(_) => evaluateReturn(env, rets)
        case SMethodCall(_, _) => throw new NotSupportedException("Statement Method Call not supported at %s" format stmt.loc)
        case SSetField(_, _)   => throw new NotSupportedException("Set field not supported at %s" format stmt.loc)
      }

    def applyCall(env: EvEnv, name: String, actuals: List[Expr]): (Option[ValueWAbstr], EvEnv) =
      if (ctx.occurs(name) || name.startsWith("#")) {
        val (vacts, nenv) = evaluateActuals(env, actuals)
        if (name startsWith "#") {
          (Some((functConvert.applyNative(name stripPrefix "#", vacts), CADInfo.Factory.star)), nenv)

        }
        else {
          val (m, cenv) = ctx lookup name
          val (ret, fcenv) = evaluateCall((m, cenv update_values nenv), vacts)
          (ret, nenv update_values fcenv)
        }
      }
      else
        throw new EvaluationException("Could not find the function named %s." format (name))

    def evaluateActuals(env: EvEnv, actuals: List[Expr]): (List[ValueWAbstr], EvEnv) =
      actuals.foldLeft((List[ValueWAbstr](), env)) {
        case ((others, env), expr) =>
          val (v, nenv) = evaluateExpr(env, expr)
          (others ++ List(v), nenv)
      }

    def evaluateExpr(env: EvEnv, expr: Expr): (ValueWAbstr, EvEnv) =
      expr match {
        case EVariable(x) =>
          (env.lookup(x), env)
        case EBExpr(op, l, r) =>
          val (lv, nenv) = evaluateExpr(env, l)
          val (rv, fenv) = evaluateExpr(nenv, r)
          try
            ((evaluateBinOp(op, lv, rv), fenv))
          catch {
            case EvaluationException(_) =>
              throw new EvaluationException("The evaluation of the binary expression has wrong arguments type at %s" format expr.loc) //%d,%d" format (expr.loc.line, expr.loc.column))
          }
        case EUExpr(op, e) =>
          val (v, nenv) = evaluateExpr(env, e)
          try
            ((evaluateUnOp(op, v), nenv))
          catch {
            case EvaluationException(_) =>
              throw new EvaluationException("The evaluation of the unary expression has wrong arguments type at %s" format expr.loc)
          }
        case ECall(name, actuals) =>
          applyCall(env, name, actuals) match {
            case (None, _)                     => throw new EvaluationException("The function %s is void so it cannot be used in an expression call at %s" format (name, expr.loc))
            case (Some(ret: ValueWAbstr), env) => (ret, env)
          }
        case ELit(IntLit(v))    => ((IntValue(v), CADInfo.Factory.star), env)
        case ELit(BoolLit(v))   => ((BoolValue(v), CADInfo.Factory.star), env)
        case ELit(StringLit(v)) => ((StringValue(v), CADInfo.Factory.star), env)
        case ELit(NullLit)      => throw new NotSupportedException("Expression \"null\" not supported at %O", expr.loc)
        case ENew(_, _)         => throw new NotSupportedException("Expression New not supported at %O", expr.loc)
        case EThis              => throw new NotSupportedException("Expression This not supported at %O", expr.loc)
        case EMethodCall(_, _)  => throw new NotSupportedException("Expression Method Call not supported at %O", expr.loc)
        case EGetField(_)       => throw new NotSupportedException("Get Field Expression not supported at %O", expr.loc)
      }

    // Binary operation evaluation. Return the value + the label
    def evaluateBinOp(op: BOperator, lv: ValueWAbstr, rv: ValueWAbstr): ValueWAbstr =
      {
        val res =
          (lv._1, rv._1) match {
            case (IntValue(l), IntValue(r)) =>
              op match {
                case BOPlus(ann)  => IntValue(l + r)
                case BOMinus(ann) => IntValue(l - r)
                case BOMul(ann)   => IntValue(l * r)
                case BODiv(ann)   => IntValue(l / r)
                case BOMod(ann)   => IntValue(l % r)
                case BOEq(ann)    => BoolValue(l == r)
                case BONeq(ann)   => BoolValue(l != r)
                case BOLt(ann)    => BoolValue(l < r)
                case BOLeq(ann)   => BoolValue(l <= r)
                case BOGt(ann)    => BoolValue(l > r)
                case BOGeq(ann)   => BoolValue(l >= r)
                case _            => throw new EvaluationException("Type mismatch on binary operation")
              }
            case (StringValue(l), StringValue(r)) =>
              op match {
                case BOPlusPlus(ann) => StringValue(l + r)
                case BOEq(ann)       => BoolValue(l == r)
                case BONeq(ann)      => BoolValue(l != r)
                case BOLt(ann)       => BoolValue(l < r)
                case BOLeq(ann)      => BoolValue(l <= r)
                case BOGt(ann)       => BoolValue(l > r)
                case BOGeq(ann)      => BoolValue(l >= r)
                case _               => throw new EvaluationException("Type mismatch on binary operation")
              }
            case (BoolValue(l), BoolValue(r)) =>
              op match {
                case BOAnd(ann) => BoolValue(l && r)
                case BOOr(ann)  => BoolValue(l || r)
                case BOEq(ann)  => BoolValue(l == r)
                case BONeq(ann) => BoolValue(l != r)
                /*case BOLt  (_) => BoolValue(l < r)
            case BOLeq (_) => BoolValue(l <= r)
            case BOGt  (_) => BoolValue(l > r)
            case BOGeq (_) => BoolValue(l >= r)*/
                case _          => throw new EvaluationException("Type mismatch on binary operation")
              }
            case _ => throw new EvaluationException("Type mismatch on binary operation")
          }
        //(res, lv._2.addExpStm(Statement.sCreator(rv._2.label, op.annot))) //@FIXME: we must create the same record for the second label!
        //(res, lv._2.update(rv._2, CFElement.Factory.fromFunAnnot(op.annot))) //@FIXME: maybe use annot instead of FlowElement...
        (res, lv._2.update(rv._2, op.annot)) //@FIXME: ???
      }

    // Unary operation evaluation. Return the value + the label
    def evaluateUnOp(op: UOperator, v: ValueWAbstr): ValueWAbstr =
      v match {
        case (IntValue(i), lab) =>
          op match {
            //case UNeg(ann) => (IntValue(-i), lab.addExpStm(Statement.sCreator(lab.label, ann))) //@FIXME: impplement update method
            case UNeg(ann) => (IntValue(-i), lab.update(ann))
            case _         => throw new EvaluationException("Type mismatch on unary operation")
          }
        case (BoolValue(b), lab) =>
          op match {
            //case UNot(ann) => (BoolValue(!b), lab.addExpStm(Statement.sCreator(lab.label, ann))) //@FIXME: impplement update method
            case UNot(ann) => (BoolValue(!b), lab.update(ann))
            case _         => throw new EvaluationException("Type mismatch on unary operation")
          }
        case _ => throw new EvaluationException("Type mismatch on unary operation")
      }
  }
}
