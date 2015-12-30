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

  case class EvaluationException(_message: string) extends MessageException("Evaluation exception: %s" format _message) {
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

    var logs: List[ValueWAbstr] = List.empty[ValueWAbstr]

    private val ctx: MethInfo =
      program match {
        case Program(List()) => throw new EvaluationException("Empty class definition.")
        case Program(classes) =>
          val venv: EvEnv =
            Env(
              ((for (Class(name, _, fields, _) <- classes)
                yield createVars(fields map { case FieldDecl(ty, ns) => (ty, ns map { "%s.%s" format (name, _) }) }, CADInfo.Factory.empty)) flatten)toMap) //@FIXME: cosa passiamo come implFlow??

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
          case Some(main) => evaluateCall(main, List(), "MAIN", CADInfo.Factory.empty) //@FIXME: cosa passiamo come implFlow??
          case None       => throw new EvaluationException("No main found...")
        }
      }

    def evaluateCall(call: (MethodDecl, EvEnv), actuals: List[ValueWAbstr], call_point_uid: Uid, implFlow: ADInfo): (Option[ValueWAbstr], EvEnv) =
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
        val (ret, fenv) = evaluateBlock(env binds_new form_bind, md.body, implFlow)
        //val adexp = actuals.head
        val new_ret = (ret, md.annot) match {
          case (None, _)   => ret
          case (ret, None) => ret
          case (Some((retv, retLab)), Some(fannot)) =>
            fannot match {
              case annot: FunAnnot =>
                val actuals_annots = actuals map { _._2 }
                actuals_annots.length match {
                  // @FIXME: fix theUid; actuals(1)_1 is a ConcreteValue, but we need abstract! (are we sure that actuals contains the parameters?)
                  case 1 => Some((retv, actuals_annots.head.update(annot, call_point_uid, actuals(1)_1).join(implFlow))) //@TODO: check correctness of implicit
                  case 2 => Some((retv, actuals_annots.head.update(annot, call_point_uid, actuals.map(_._1).toList, actuals_annots(1)).join(implFlow))) //@TODO: check correctness of implicit
                  case _ => Some((retv, actuals_annots.head.update(annot, call_point_uid, actuals.map(_._1).toList, actuals_annots.tail).join(implFlow))) //@TODO: check correctness of implicit
                }
              case lab: LabelAnnot => Some((retv, CADInfo.Factory.fromLabelAnnot(lab).join(implFlow))) //@TODO: check correctness of implicit
              case _               => throw new Unexpected("Unknown annotation type %s." format fannot.toString)
            }
        }
        (new_ret, env update_values fenv)
      }

    /**
     * Create the set of fields in a class, all empty labels
     */
    def createVars(vars: List[(Type, List[string])], implFlow: ADInfo): List[(string, ValueWAbstr)] =
      for ((ty, names) <- vars; name <- names)
        yield (name,
        ty match {
          case TyInt    => (new IntValue(), CADInfo.Factory.star.join(implFlow)) //@TODO: check correctness of implicit
          case TyBool   => (new BoolValue(), CADInfo.Factory.star.join(implFlow)) //@TODO: check correctness of implicit
          case TyString => (new StringValue(), CADInfo.Factory.star.join(implFlow)) //@TODO: check correctness of implicit
          case _        => throw new Unexpected("Variable %s has not supported type %s", (name, ty))
        })

    /**
     * Evaluate the block
     */
    def evaluateBlock(env: EvEnv, block: Block, implFlow: ADInfo): (Option[ValueWAbstr], EvEnv) =
      {
        val nenv: List[(id, ValueWAbstr)] = createVars(block.varDecls map { vd => (vd.ty, vd.ids) }, implFlow)

        val (ret, fenv) = block.stmts.foldLeft(None: Option[ValueWAbstr], env binds_new nenv) {
          case (ret @ (Some(_), _), stmt) => ret
          case ((None, env), stmt)        => evaluateStmt(env, stmt, implFlow)
        }
        (ret, env update_values fenv)
      }

    def evaluateStmt(env: EvEnv, stmt: Stmt, implFlow: ADInfo): (Option[ValueWAbstr], EvEnv) =
      stmt match {
        case SSkip => (None, env)
        case SAssign(x, e) =>
          val (res, nenv) = evaluateExpr(env, e, implFlow)
          if (res._1.ty != nenv.lookup(x)._1.ty)
            throw new EvaluationException("Type error: variable %s has type %s, but is given type %s at %s".format(x, nenv.lookup(x)._1.ty, res._1.ty, stmt.loc.toString()))
          else
            (None, nenv.update(x) { _ => res })
        case SIf(c, thn, els) => //@TODO: collect the implicit!!
          val (cond, nenv) = evaluateExpr(env, c, implFlow)
          cond._1 match {
            case BoolValue(v) =>
              if (v)
                evaluateStmt(nenv, thn, cond._2.asImplicit)
              else
                evaluateStmt(nenv, els, cond._2.asImplicit)
            case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
          }
        case SWhile(c, body) => //@TODO: collect the implicit!!
          val (cond, nenv) = evaluateExpr(env, c, implFlow)
          cond._1 match {
            case BoolValue(v) =>
              if (v) {
                evaluateStmt(nenv, body, implFlow) match {
                  case (None, wenv)       => evaluateStmt(wenv, stmt, implFlow)
                  case ret @ (Some(_), _) => ret
                }
              }
              else
                (None, nenv)
            case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
          }
        case SBlock(block) => evaluateBlock(env, block, implFlow)
        case SReturn(None) => ((Some(UnitValue(), CADInfo.Factory.star.join(implFlow))), env) //@TODO: check correctness of implicit
        case SReturn(Some(e)) =>
          val (res, nenv) = evaluateExpr(env, e, implFlow)
          (Some(res), nenv)
        case SPrint(ln, actual) =>
          val (vactual, nenv) = evaluateExpr(env, actual, implFlow)
          logs = vactual :: logs
          if (config.value.verbose)
            if (ln) println(vactual._1.value) else print(vactual._1.value)
          (None, nenv)
        case scall@SCall(name, actuals) => //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyCall(env, name, actuals, scall.uid, implFlow) match {
            case (Some(_), env) => (None, env)
            case (None, env)    => (None, env) //@FIXME: URGENT!!!
          }
        case SNativeCall(name, actuals) => throw new EvaluationException("Native calls not supported yet")
        //case rets @ SReturn(_) => evaluateReturn(env, rets)
        case SMethodCall(_, _)          => throw new NotSupportedException("Statement Method Call not supported at %s" format stmt.loc)
        case SSetField(_, _)            => throw new NotSupportedException("Set field not supported at %s" format stmt.loc)
      }

    def applyCall(env: EvEnv, name: String, actuals: List[Expr], call_point_uid : Uid, implFlow: ADInfo): (Option[ValueWAbstr], EvEnv) =
    //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
      if (ctx.occurs(name) || name.startsWith("#")) {
        val (vacts, nenv) = evaluateActuals(env, actuals, implFlow)
        if (name startsWith "#") {
          (Some((functConvert.applyNative(name stripPrefix "#", vacts), CADInfo.Factory.star.join(implFlow))), nenv) //@TODO: check correctness of implicit

        }
        else {
          val (m, cenv) = ctx lookup name
          val (ret, fcenv) = evaluateCall((m, cenv update_values nenv), vacts, call_point_uid, implFlow)
          (ret, nenv update_values fcenv)
        }
      }
      else
        throw new EvaluationException("Could not find the function named %s." format (name))

    def evaluateActuals(env: EvEnv, actuals: List[Expr], implFlow: ADInfo): (List[ValueWAbstr], EvEnv) =
      actuals.foldLeft((List[ValueWAbstr](), env)) {
        case ((others, env), expr) =>
          val (v, nenv) = evaluateExpr(env, expr, implFlow)
          (others ++ List(v), nenv)
      }

    def evaluateExpr(env: EvEnv, expr: Expr, implFlow: ADInfo): (ValueWAbstr, EvEnv) =
      expr match {
        case EVariable(x) =>
          (env.lookup(x), env)
        case EBExpr(op, l, r) =>
          val (lv, nenv) = evaluateExpr(env, l, implFlow)
          val (rv, fenv) = evaluateExpr(nenv, r, implFlow)
          try
            ((evaluateBinOp(op, lv, rv, implFlow), fenv))
          catch {
            case EvaluationException(_) =>
              throw new EvaluationException("The evaluation of the binary expression has wrong arguments type at %s" format expr.loc) //%d,%d" format (expr.loc.line, expr.loc.column))
          }
        case EUExpr(op, e) =>
          val (v, nenv) = evaluateExpr(env, e, implFlow)
          try
            ((evaluateUnOp(op, v, implFlow), nenv))
          catch {
            case EvaluationException(_) =>
              throw new EvaluationException("The evaluation of the unary expression has wrong arguments type at %s" format expr.loc)
          }
        case ecall@ECall(name, actuals) => //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyCall(env, name, actuals, ecall.uid, implFlow) match {
            case (None, _)                     => throw new EvaluationException("The function %s is void so it cannot be used in an expression call at %s" format (name, expr.loc))
            case (Some(ret: ValueWAbstr), env) => (ret, env)
          }
        case ENativeCall(name, actuals) => throw new EvaluationException("Native calls not supported yet")
        case ELit(IntLit(v))            => ((IntValue(v), CADInfo.Factory.star.join(implFlow)), env) //@TODO: check correctness of implicit
        case ELit(BoolLit(v))           => ((BoolValue(v), CADInfo.Factory.star.join(implFlow)), env) //@TODO: check correctness of implicit
        case ELit(StringLit(v))         => ((StringValue(v), CADInfo.Factory.star.join(implFlow)), env) //@TODO: check correctness of implicit
        case ELit(NullLit)              => throw new NotSupportedException("Expression \"null\" not supported at %O", expr.loc)
        case ENew(_, _)                 => throw new NotSupportedException("Expression New not supported at %O", expr.loc)
        case EThis                      => throw new NotSupportedException("Expression This not supported at %O", expr.loc)
        case EMethodCall(_, _)          => throw new NotSupportedException("Expression Method Call not supported at %O", expr.loc)
        case EGetField(_)               => throw new NotSupportedException("Get Field Expression not supported at %O", expr.loc)
      }

    // Binary operation evaluation. Return the value + the label
    def evaluateBinOp(op: BOperator, lv: ValueWAbstr, rv: ValueWAbstr, implFlow: ADInfo): ValueWAbstr =
      {
        val res =
          (lv._1, rv._1) match {
            case (IntValue(l), IntValue(r)) =>
              op match {
                case BOPlus(uid, ann)  => IntValue(l + r)
                case BOMinus(uid, ann) => IntValue(l - r)
                case BOMul(uid, ann)   => IntValue(l * r)
                case BODiv(uid, ann)   => IntValue(l / r)
                case BOMod(uid, ann)   => IntValue(l % r)
                case BOEq(uid, ann)    => BoolValue(l == r)
                case BONeq(uid, ann)   => BoolValue(l != r)
                case BOLt(uid, ann)    => BoolValue(l < r)
                case BOLeq(uid, ann)   => BoolValue(l <= r)
                case BOGt(uid, ann)    => BoolValue(l > r)
                case BOGeq(uid, ann)   => BoolValue(l >= r)
                case _                 => throw new EvaluationException("Type mismatch on binary operation")
              }
            case (StringValue(l), StringValue(r)) =>
              op match {
                case BOPlusPlus(uid, ann) => StringValue(l + r)
                case BOEq(uid, ann)       => BoolValue(l == r)
                case BONeq(uid, ann)      => BoolValue(l != r)
                case BOLt(uid, ann)       => BoolValue(l < r)
                case BOLeq(uid, ann)      => BoolValue(l <= r)
                case BOGt(uid, ann)       => BoolValue(l > r)
                case BOGeq(uid, ann)      => BoolValue(l >= r)
                case _                    => throw new EvaluationException("Type mismatch on binary operation")
              }
            case (BoolValue(l), BoolValue(r)) =>
              op match {
                case BOAnd(uid, ann) => BoolValue(l && r)
                case BOOr(uid, ann)  => BoolValue(l || r)
                case BOEq(uid, ann)  => BoolValue(l == r)
                case BONeq(uid, ann) => BoolValue(l != r)
                case _               => throw new EvaluationException("Type mismatch on binary operation")
              }
            case _ => throw new EvaluationException("Type mismatch on binary operation")
          }
        // @FIXME: fix theUid; List(lv._1, rv._1) contains ConcreteValue, but we need abstract!
        (res, lv._2.update(op.annot, op.uid, List(lv._1, rv._1), rv._2).join(implFlow)) //@TODO: check correctness of implicit
      }

    // Unary operation evaluation. Return the value + the label
    def evaluateUnOp(op: UOperator, v: ValueWAbstr, implFlow: ADInfo): ValueWAbstr =
      v match {
        case (IntValue(i), lab) =>
          op match {
            // @FIXME: fix theUid; v._1 is a ConcreteValue, but we need abstract!
            case UNeg(uid, ann) => (IntValue(-i), lab.update(ann, op.uid, v._1).join(implFlow)) //@TODO: check correctness of implicit
            case _              => throw new EvaluationException("Type mismatch on unary operation")
          }
        case (BoolValue(b), lab) =>
          op match {
            // @FIXME: fix theUid; v._1 is a ConcreteValue, but we need abstract!
            case UNot(uid, ann) => (BoolValue(!b), lab.update(ann, op.uid, v._1).join(implFlow)) //@TODO: check correctness of implicit
            case _              => throw new EvaluationException("Type mismatch on unary operation")
          }
        case _ => throw new EvaluationException("Type mismatch on unary operation")
      }
  }
}
