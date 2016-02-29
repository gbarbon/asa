package it.unive.dais.yaasa

/**
 * @author esteffin
 * @author gbarbon
 */

import it.unive.dais.yaasa.abstract_types._
import it.unive.dais.yaasa.datatype.ABSValue._
import it.unive.dais.yaasa.exception.TypeMismatchException

//import it.unive.dais.yaasa.exception
import utils.prelude._
//import utils.pretty_print._
import utils.env._
import absyn._
import it.unive.dais.yaasa.datatype.CADInfo.CADInfo
import it.unive.dais.yaasa.datatype.CADInfo.CADInfoFactory
import it.unive.dais.yaasa.datatype.FortyTwo._
import it.unive.dais.yaasa.widening.WideningOperator

object analyzer {

  case class EvaluationException(_message: string) extends MessageException("Evaluation exception: %s" format _message) {
    /*def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))*/
  }

  type EvEnv = Env[String, ValueWithAbstraction]

  type MethInfo = Env[String, (MethodDecl, EvEnv)]

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

    var logs: List[ValueWithAbstraction] = List.empty[ValueWithAbstraction]

    private val ctx: MethInfo =
      program match {
        case Program(List()) => throw new EvaluationException("Empty class definition.")
        case Program(classes) =>
          val venv: EvEnv =
            (for (Class(name, _, fields, _) <- classes)
                yield createBindVars(
                  Env.empty[String, ValueWithAbstraction],
                  fields map { case FieldDecl(ty, ns) => (ty, ns.map({ x => "%s.%s" format (name, x) })) },
                  CADInfoFactory.empty)).foldLeft(Env.empty[String, ValueWithAbstraction]){ (acc, e) => acc.append(e) }

          Env(
            (for (Class(cname, _, _, methods) <- classes; m <- methods)
              yield ("%s.%s" format (cname, m.name), (m, venv))) toMap)
      }

    def evaluateProgram(): (Option[ValueWithAbstraction], EvEnv) =
      {
        ctx search_by_key { _ endsWith ".main" } match {
          case Some(main) => evaluateCall(main, List(), "MAIN", CADInfoFactory.empty) //@FIXME: cosa passiamo come implFlow??
          case None       => throw new EvaluationException("No main found...")
        }
      }

    def evaluateCall(call: (MethodDecl, EvEnv), actuals: List[ValueWithAbstraction], call_point_uid: Uid, implFlow: CADInfo): (Option[ValueWithAbstraction], EvEnv) = {
      val (md, env) = call

      if (md.formals.length != actuals.length)
        throw new EvaluationException("Function %s is called with wrong argument number".format(md.name))

      val form_bind =
        for ((form, act) <- md.formals.zip(actuals))
          yield
            if (form.ty.ty != act.value.ty)
              throw new TypeMismatchException("in method %s parameter %s has type %s, but is given type %s at %s".format(md.name, form.name, form.ty.ty, act.value.ty, md.loc))
            else
              (form.name, act)
      val (ret, fenv) = evaluateBlock(env binds_new form_bind, md.body, implFlow)
      val new_ret = (ret, md.annot) match {
        case (None, _)   => ret
        case (mret, None) => mret
        case (Some(mret), Some(fannot)) =>
          fannot match {
            case annot: FunAnnot =>
              val actuals_annots = actuals map { _.adInfo }
              actuals_annots.length match {
                case 1 => Some(ValueWithAbstraction(mret.value, actuals_annots.head.update(annot, call_point_uid, (actuals.head.value, null), null)/*.join(implFlow)*/)) //@TODO: check correctness of implicit
                case 2 => Some(ValueWithAbstraction(mret.value, actuals_annots.head.update(annot, call_point_uid, (actuals.head.value, actuals(1).value), actuals_annots(1))/*.join(implFlow)*/)) //@TODO: check correctness of implicit
              }
            case lab: LabelAnnot => Some(ValueWithAbstraction(mret.value, CADInfoFactory.fromLabelAnnot(lab)/*.join(implFlow)*/)) //@TODO: check correctness of implicit
            case _               => throw new Unexpected("Unknown annotation type %s." format fannot.toString)
          }
      }
      (new_ret, env update_values fenv)
    }

    /**
     * Create the set of fields in a class, all empty labels
     */
    def createBindVars(env: EvEnv, vars: List[(AnnotatedType, List[string])], implFlow: CADInfo): EvEnv = {
      val vs =
        for ((ty, names) <- vars; name <- names)
          yield (name,
            ty.ty match {
              case TyNum => ValueWithAbstraction(AbstractNumFactory.default, CADInfoFactory.star.join(implFlow)) //@TODO: check correctness of implicit
              case TyBool => ValueWithAbstraction(AbstractBoolFactory.default, CADInfoFactory.star.join(implFlow)) //@TODO: check correctness of implicit
              case TyString => ValueWithAbstraction(AbstractStringFactory.default, CADInfoFactory.star.join(implFlow)) //@TODO: check correctness of implicit
              case TyArray(ity) => ValueWithAbstraction(AbstractArrayFactory.empty(ity), CADInfoFactory.star.join(implFlow)) //@TODO: check correctness of implicit
              case _ => throw new Unexpected("Variable %s has not supported type %s" format (name, ty))
            })
      env binds_new vs
    }

    /**
     * Evaluate the block
     */
    def evaluateBlock(env: EvEnv, block: Block, implFlow: CADInfo): (Option[ValueWithAbstraction], EvEnv) = {
      val nenv: EvEnv = createBindVars(env, block.varDecls map { vd => (vd.ty, vd.ids) }, implFlow)

      val (ret, fenv) = block.stmts.foldLeft(None: Option[ValueWithAbstraction], nenv) {
        case (ret @ (Some(_), _), stmt) => ret
        case ((None, menv), stmt)        => evaluateStmt(menv, stmt, implFlow)
      }
      (ret, env update_values fenv)
    }

    def evaluateStmt(env: EvEnv, stmt: Stmt, implFlow: CADInfo): (Option[ValueWithAbstraction], EvEnv) = {
      stmt match {
        case SSkip => (None, env)
        case SAssign(x, e) =>
          val (res, nenv) = evaluateExpr(env, e, implFlow)
          if (res.value.ty != nenv.lookup(x).value.ty)
            throw new TypeMismatchException("variable %s has type %s, but is given type %s at %s".format(x, nenv.lookup(x).value.ty, res.value.ty, stmt.loc.toString()))
          else
            (None, nenv.update(x) { _ => ValueWithAbstraction(res.value, res.adInfo.join(implFlow)) })
        case SArrayAssign(x, eidxs, eval) => {
          val (idxs, menv) = evaluateExpressions(env, eidxs, implFlow)
          val (v, nenv) = evaluateExpr(menv, eval, implFlow)

throw new Unexpected("Continue here!!")
        }
        case SIf(c, thn, els) => {
          // @FIXME: comment by Gian:
          // we must collect here the difference between under and over approximation


          //@TODO: collect the implicit!!
          //throw new exception.EvaluationException("fix here")
          val (cond, nenv) = evaluateExpr(env, c, implFlow)
          cond.value match {
            case v: AbstractBool =>  // @FIXME: warning on comilation (non-variable type argument in type pattern is since it is eliminated by erasure
              //Per ora dobbiamo assumere che non ci siano return in alcum branch dell'if
              val thn_res =
                if (v.containsTrue)
                  Some(evaluateStmt(nenv, thn, cond.adInfo.asImplicit))
                else None
              val els_res =
                if (v.containsFalse)
                  Some(evaluateStmt(nenv, els, cond.adInfo.asImplicit))
                else None
              (thn_res, els_res) match {
                case (None, None) =>
                  //the guard was BOTTOM Throw an exception...
                  throw new EvaluationException("The evaluation of the guard resulted bottom at %s..." format c.loc)
                case (Some(res), None) =>
                  //the guard was {True} return the result of the evaluation of the than branch (including possible returns)
                  res
                case (None, Some(res)) =>
                  //the guard was {False} return the result of the evaluation of the else branch (including possible returns)
                  res
                case (Some(thn_v), Some(els_v)) =>
                  //the guard was TOP return the join of the evaluation of both than and else branch (without possible returns)
                  (thn_v, els_v) match {
                    case ((None, thn_env), (None, els_env)) =>
                      //None of both branches returned
                      (None, thn_env.union(els_env){(t,e) => ValueWithAbstraction(t.value join e.value, t.adInfo union e.adInfo)  })
                      // @FIXME: insert here the union operation
                    case _ =>
                      //At least one branch returned. Aborting (Throw an exception)
                      throw new EvaluationException("Cannot join different return results in branches...")
                  }

              }
            case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
          }
        }


        case smt @ SWhile(c, body) => //@TODO: collect the implicit!!

          // @FIXME: comment by Gian:
          // we must collect here the difference between under and over approximation
          (None, evaluateWhile(env, smt, implFlow))

        case SBlock(block) => evaluateBlock(env, block, implFlow)
        case SReturn(None) => (Some(ValueWithAbstraction(AbstractUnit, CADInfoFactory.star/*.join(implFlow)*/)), env) //@TODO: check correctness of implicit
        case SReturn(Some(e)) =>
          val (res, nenv) = evaluateExpr(env, e, implFlow)
          (Some(res), nenv)
        case SPrint(ln, actual) =>
          val (vactual, nenv) = evaluateExpr(env, actual, implFlow)
          val newvactual = ValueWithAbstraction(vactual.value, vactual.adInfo.join(implFlow))
          if (!config.value.quiet) {
            if (config.value.verbose) {
              if (ln) println(newvactual) else print(newvactual)
            }
            else
              if (ln) println(newvactual.value) else print(newvactual.value)
          }
          (None, nenv)
        case SLog(actual) =>
          val (vactual , nenv) = evaluateExpr(env, actual, implFlow)
          val newvactual = ValueWithAbstraction(vactual.value, vactual.adInfo.join(implFlow))
          logs = newvactual :: logs
          if (config.value.verbose)
            println(newvactual)
          (None, nenv)
        case scall@SCall(name, actuals) => //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyCall(env, name, actuals, scall.uid, implFlow) match {
            case (Some(_), menv) => (None, menv)
            case (None, menv) => (None, menv) //@FIXME: URGENT!!!
          }
        case scall@SNativeCall(name, actuals) => //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyNativeCall(env, name, actuals, scall.uid, implFlow) match {
            case (Some(_), menv) => (None, menv)
            case (None, menv) => (None, menv) //@FIXME: URGENT!!!
          }
        //case rets @ SReturn(_) => evaluateReturn(env, rets)
        case SMethodCall(_, _) => throw new NotSupportedException("Statement Method Call not supported at %s" format stmt.loc)
        case SSetField(_, _) => throw new NotSupportedException("Set field not supported at %s" format stmt.loc)
      }
    }

    def evaluateWhile(senv: EvEnv, sWhile: SWhile, simpl: CADInfo): EvEnv = {
      val cond = sWhile.cond
      val body = sWhile.body
      val widenings: Map[String, WideningOperator] =
        (for (n <- senv.keys) yield n -> WideningOperator.default) toMap
      // used to mark the minimum amount of iterations
      var strict = true

      def step(env: EvEnv): EvEnv = {
        val (cva @ ValueWithAbstraction(c_val: AbstractBool, _), cenv): (ValueWithAbstraction, EvEnv) =
          evaluateExpr(env, cond, simpl)
        if (c_val <== AbstractBoolFactory.bottom)
          throw new EvaluationException("The evaluation of the guard resulted bottom at %s." format cond.loc)

        if (c_val == AbstractBoolFactory.top)
          strict = false

        if (c_val == AbstractBoolFactory.sFalseAt) {
          cenv
        }
        else {
          val (r, benv): (Option[ValueWithAbstraction], EvEnv) = evaluateStmt(cenv, body, cva.adInfo.asImplicit)

          if (r != None)
            throw new EvaluationException("Return statement is not allowed in while loops at %s." format sWhile.loc)

          benv
        }
      }

      var eq = false
      var v_env = senv
      var repetitions = config.value.widening_threshold

      while (!eq) {
        if (repetitions < 1)
          strict = false

        repetitions = repetitions - 1

        val lenv = step(v_env)

        //FIXME: ORRORE!!
        val pairs = lenv.zip(v_env) map { case (x, y) => (x.value, y.value) }

        //println(utils.pretty_print.prettyList(" &&\n")(pairs))

        if (!pairs.forall{ case (x, y) => x <== y }) {
          if (strict){
            v_env = lenv
          }
          else {
            v_env = v_env.labelled_union(lenv){(k,b,a) =>
              widenings(k).widening(b, a) }
          }
        }
        else {
          eq = true
        }
      }
      v_env
    }

    def applyCall(env: EvEnv, name: String, actuals: List[Expr], call_point_uid: Uid, implFlow: CADInfo): (Option[ValueWithAbstraction], EvEnv) =
      //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
      if (ctx.occurs(name)) {
        val (vacts, nenv) = evaluateExpressions(env, actuals, implFlow)

        val (m, cenv) = ctx lookup name
        val (ret, fcenv) = evaluateCall((m, cenv update_values nenv), vacts, call_point_uid, implFlow)
        (ret, nenv update_values fcenv)

      }
      else
        throw new EvaluationException("Could not find the function named %s at %s." format (name, call_point_uid))

    def applyNativeCall(env: EvEnv, name: String, actuals: List[Expr], call_point_uid: Uid, implFlow: CADInfo): (Option[ValueWithAbstraction], EvEnv) = {
      val (vacts, nenv) = evaluateExpressions(env, actuals, implFlow)
      (Some(ValueWithAbstraction(functConvert.applyNative(name, vacts), CADInfoFactory.star/*.join(implFlow)*/)), nenv) //@TODO: check correctness of implicit
    }

    def evaluateExpressions(env: EvEnv, exprs: List[Expr], implFlow: CADInfo): (List[ValueWithAbstraction], EvEnv) =
      exprs.foldLeft((List[ValueWithAbstraction](), env)) {
        case ((others, menv), expr) =>
          val (v, nenv) = evaluateExpr(menv, expr, implFlow)
          (others ++ List(v), nenv)
      }

    def evaluateExpr(env: EvEnv, expr: Expr, implFlow: CADInfo): (ValueWithAbstraction, EvEnv) =
      expr match {
        case EVariable(x) =>
          (env.lookup(x), env)
        case EArrayNew(ty, dim) =>
          val default =
            ty.ty match {
              case TyArray(ity) => ValueWithAbstraction(AbstractArrayFactory.empty(ity), CADInfoFactory.star.join(implFlow))
              case TyNum => ValueWithAbstraction(AbstractNumFactory.default, CADInfoFactory.star.join(implFlow))
              case TyBool => ValueWithAbstraction(AbstractBoolFactory.default, CADInfoFactory.star.join(implFlow))
              case TyString => ValueWithAbstraction(AbstractStringFactory.default, CADInfoFactory.star.join(implFlow))
              case _ => throw new Unexpected("Array at %s has not supported inner type" format expr.loc)
            }
          (ValueWithAbstraction(AbstractArrayFactory.create(ty.ty, dim.value, default), CADInfoFactory.star.join(implFlow)), env)
        case EBExpr(op, l, r) =>
          val (lv, nenv) = evaluateExpr(env, l, implFlow)
          val (rv, fenv) = evaluateExpr(nenv, r, implFlow)
          try {
            (evaluateBinOp(op, lv, rv, implFlow), fenv)
          }
          catch {
            case EvaluationException(_) =>
              throw new TypeMismatchException("The evaluation of the binary expression has wrong arguments type at %s" format expr.loc) //%d,%d" format (expr.loc.line, expr.loc.column))
          }
        case EUExpr(op, e) =>
          val (v, nenv) = evaluateExpr(env, e, implFlow)
          try {
            (evaluateUnOp(op, v, implFlow), nenv)
          }
          catch {
            case EvaluationException(_) =>
              throw new TypeMismatchException("The evaluation of the unary expression has wrong arguments type at %s" format expr.loc)
          }
        case ecall @ ECall(name, actuals) => //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyCall(env, name, actuals, ecall.uid, implFlow) match {
            case (None, _)                     => throw new EvaluationException("The function %s is void so it cannot be used in an expression call at %s" format (name, expr.loc))
            case (Some(ret: ValueWithAbstraction), menv) => (ret, menv)
          }
        case ecall @ ENativeCall(name, actuals) =>  //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyNativeCall(env, name, actuals, ecall.uid, implFlow) match {
            case (None, _)                     => throw new EvaluationException("The function %s is void so it cannot be used in an expression call at %s" format (name, expr.loc))
            case (Some(ret: ValueWithAbstraction), menv) => (ret, menv)
          }
        case ELit(IntLit(v))            => (ValueWithAbstraction(AbstractNumFactory.fromNum(v), CADInfoFactory.star/*.join(implFlow)*/), env) //@TODO: check correctness of implicit
        case ELit(BoolLit(v))           => (ValueWithAbstraction(AbstractBoolFactory.fromBool(v), CADInfoFactory.star/*.join(implFlow)*/), env) //@TODO: check correctness of implicit
        case ELit(StringLit(v))         => (ValueWithAbstraction(AbstractStringFactory.fromString(v), CADInfoFactory.star/*.join(implFlow)*/), env) //@TODO: check correctness of implicit
        case ELit(NullLit)              => throw new NotSupportedException("Expression \"null\" not supported at %O", expr.loc)
        case ENew(_, _)                 => throw new NotSupportedException("Expression New not supported at %O", expr.loc)
        case EThis                      => throw new NotSupportedException("Expression This not supported at %O", expr.loc)
        case EMethodCall(_, _)          => throw new NotSupportedException("Expression Method Call not supported at %O", expr.loc)
        case EGetField(_)               => throw new NotSupportedException("Get Field Expression not supported at %O", expr.loc)
      }

    // Binary operation evaluation. Return the value + the label
    def evaluateBinOp(op: BOperator, lv: ValueWithAbstraction, rv: ValueWithAbstraction, implFlow: CADInfo): ValueWithAbstraction = {
      val res =
        (lv.value, rv.value) match {
          case (l: AbstractNum, r: AbstractNum) =>  // @FIXME: warning on comilation (non-variable type argument in type pattern is since it is eliminated by erasure
            op match {
              case BOPlus(ann)  => l +^ r
              case BOMinus(ann) => l -^ r
              case BOMul(ann)   => l *^ r
              case BODiv(ann)   => l /^ r
              case BOMod(ann)   => l %^ r
              case BOEq(ann)    => l ==^ r
              case BONeq(ann)   => l !=^ r
              case BOLt(ann)    => l <^ r
              case BOLeq(ann)   => l <=^ r
              case BOGt(ann)    => l >^ r
              case BOGeq(ann)   => l >=^ r
              case _                 => throw new TypeMismatchException("Type mismatch on binary operation at %s" format op.loc)
            }
          case (l: AbstractString, r: AbstractString) => // @FIXME: warning on comilation (non-variable type argument in type pattern is since it is eliminated by erasure
            op match {
              case BOPlusPlus(ann) => l ++^ r
              case BOEq(ann)       => l ==^ r
              case BONeq(ann)      => l !=^ r
              case BOLt(ann)       => l <^ r
              case BOLeq(ann)      => l <=^ r
              case BOGt(ann)       => l >^ r
              case BOGeq(ann)      => l >=^ r
              case _               => throw new TypeMismatchException("Type mismatch on binary operation at %s" format op.loc)
            }
          case (l: AbstractBool, r: AbstractBool) => // @FIXME: warning on compilation (non-variable type argument in type pattern is since it is eliminated by erasure
            op match {
              case BOAnd(ann) => l &&^ r
              case BOOr(ann)  => l ||^ r
              case BOEq(ann)  => l ==^ r
              case BONeq(ann) => l !=^ r
              case _               => throw new TypeMismatchException("Type mismatch on binary operation at %s" format op.loc)
            }
          case _ => throw new TypeMismatchException("Type mismatch on binary operation at %s" format op.loc)
        }
      ValueWithAbstraction(res, lv.adInfo.update(op.annot, op.uid, (lv.value, rv.value), rv.adInfo)/*.join(implFlow)*/) //@TODO: check correctness of implicit
    }

    // Unary operation evaluation. Return the value + the label
    def evaluateUnOp(op: UOperator, v: ValueWithAbstraction, implFlow: CADInfo): ValueWithAbstraction = {
      val res =
        v.value match {
          case n: AbstractNum => // @FIXME: warning on compilation (non-variable type argument in type pattern is since it is eliminated by erasure
            op match {
              case UNeg(ann) => n.negAt //@TODO: check correctness of implicit
              case _ => throw new TypeMismatchException("Type mismatch on unary operation at %s" format op.loc)
            }
          case b: AbstractBool => // @FIXME: warning on compilation (non-variable type argument in type pattern is since it is eliminated by erasure
            op match {
              case UNot(ann) => b.notAt //@TODO: check correctness of implicit
              case _ => throw new TypeMismatchException("Type mismatch on unary operation at %s" format op.loc)
            }
          case _ => throw new TypeMismatchException("Type mismatch on unary operation at %s" format op.loc)
        }
      ValueWithAbstraction(res, v.adInfo.update(op.annot, op.uid, (v.value, null), null)/*.join(implFlow)*/)
    }
  }
}
