package it.unive.dais.yaasa

/**
 * @author esteffin
 * @author gbarbon
 */

import utils.prelude._
import utils.pretty_print._
import utils.env._
import it.unive.dais.yaasa.abstract_values._
import absyn._
import scala.collection.breakOut
import it.unive.dais.yaasa.functConvert._

/**
 *
 */
object analyzer {
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

  //def evaluateLabel()

  case class EvaluationException(_message: string) extends MessageException {
    val message = "Evaluation exception: %s" format _message
    /*def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))*/
  }
  trait ConcreteValue {
    val value: Any
    val ty: Type
  }

  type ValueWAbstr = (ConcreteValue, ADExp)

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

  def evaluateProgram(program: Program) =
    {
      program match {
        case Program(List()) => throw new EvaluationException("Empty class definition.")
        case Program(classes) =>
          val venv: EvEnv =
            Env(
              ((for (Class(name, _, fields, _) <- classes)
                yield createVars(fields map { case FieldDecl(ty, ns) => (ty, ns map { "%s.%s" format (name, _) }) })) flatten)toMap)
          val fenv: MethInfo =
            Env(
              (for (Class(cname, _, _, methods) <- classes; m <- methods)
                yield ("%s.%s" format (cname, m.name), (m, venv))) toMap)
          fenv search_by_key { _ endsWith ".main" } match {
            case Some(main) => evaluateCall(fenv, main, List())
            case None       => throw new EvaluationException("No main found...")
          }
      }
    }
  /**
   * /**
   * @param program
   * @return
   * */
   * def evaluateProgram(program: Program) = {
   * program.classes match {
   * case List() => throw new Unexpected("Empty class definition.")
   * case c :: _ =>
   * //evaluateClass(c)
   * }
   * }
   */

  def evaluateClass(c: Class) =
    {
      val fieldEnv =
        new Env(createVars(c.fields map { fd => (fd.ty, fd.names) }) toMap)
      val funEnv =
        new Env(for (m <- c.methods if m.name != "main") yield (m.name, (m, fieldEnv)))

      c.methods.find { _.name == "main" } match {
        case None => throw new Unexpected("No method main in first class :%s.", c.name)
        case Some(m) =>
          {
            //val env = new Env[String, ConcreteValue]()
            evaluateCall(funEnv, (m, fieldEnv), List[ValueWAbstr]()) //(env)
          }
      }
    }

  def evaluateCall(ctx: MethInfo, call: (MethodDecl, EvEnv), actuals: List[ValueWAbstr]) =
    {
      val (md, env) = call

      //"magic" functions (library functions)
      if (md.name.startsWith("#")) {
        val resv =
          md.name.stripPrefix("#") match {
            //stdlib functions
            case "encrypt" => actuals match {
              case List((StringValue(lab), _), (StringValue(key), _)) => stdlib.encrypt(lab, key)
              case _ => throw new EvaluationException("encrypt function arguments not matched")
            }
            case "substring" => actuals match {
              case List((StringValue(str), _), (IntValue(beg), _), (IntValue(end), _)) => stdlib.substring(str, beg, end)
              case _ => throw new EvaluationException("substring function arguments not matched")
            }
            case "hash" => actuals match {
              case List((StringValue(str), _)) => stdlib.hash(str)
              case _                           => throw new EvaluationException("hash function arguments not matched")
            }
            case "checkpwd" => actuals match {
              case List((StringValue(first), _), (StringValue(second), _)) => stdlib.checkpwd(first, second)
              case _ => throw new EvaluationException("checkpwd function arguments not matched")
            }
            case "strInput"    => stdlib.strInput
            case "boolInput"   => stdlib.boolInput
            case "intInput"    => stdlib.intInput
            case "getDeviceID" => stdlib.getDeviceID
            case "intToString" => actuals match {
              case List((IntValue(v), _)) => stdlib.intToString(v)
              case _                      => throw new EvaluationException("intToString function arguments not matched")
            }
            case "boolToString" => actuals match {
              case List((BoolValue(v), _)) => stdlib.boolToString(v)
              case _                       => throw new EvaluationException("boolToString function arguments not matched")
            }
            case "strToInt" => actuals match {
              case List((StringValue(v), _)) => stdlib.strToInt(v)
              case _                         => throw new EvaluationException("strToInt function arguments not matched")
            }
            case "strToBool" => actuals match {
              case List((StringValue(v), _)) => stdlib.strToBool(v)
              case _                         => throw new EvaluationException("strToBool function arguments not matched")
            }
            case "length" => actuals match {
              case List((StringValue(v), _)) => stdlib.length(v)
              case _                         => throw new EvaluationException("length function arguments not matched")
            }
            case "log" => actuals match {
              case List((StringValue(v), _)) => stdlib.log(v)
              case _                         => throw new EvaluationException("log function arguments not matched")
            }

            //readlib functions
            case "readString" => actuals match {
              case List((StringValue(str), _)) => readlib.readString(str)
              case _                           => throw new EvaluationException("readString function arguments not matched")
            }
            case "readInt" => actuals match {
              case List((StringValue(str), _)) => readlib.readInt(str)
              case _                           => throw new EvaluationException("readInt function arguments not matched")
            }
            case "readBool" => actuals match {
              case List((StringValue(str), _)) => readlib.readBool(str)
              case _                           => throw new EvaluationException("readBool function arguments not matched")
            }
            case "readIMEI" => readlib.readIMEI
            case "readUsrPwd" => actuals match {
              case List((StringValue(str), _)) => readlib.readUsrPwd(str)
              case _                           => throw new EvaluationException("readUsrPwd function arguments not matched")
            }
            case "readGeoLoc" => readlib.readGeoLoc
            case "readPhoneNum" => actuals match {
              case List((StringValue(str), _)) => readlib.readPhoneNum(str)
              case _                           => throw new EvaluationException("readPhoneNum function arguments not matched")
            }
            case _ => throw new EvaluationException("unrecognized library function")
          }
        ((resv, ADExp.empty), env)
      }
      else { //functions defined in the program

        if (md.formals.length != actuals.length)
          throw new EvaluationException("Function %s is called with wrong argument number")

        val form_bind =
          for ((form, act) <- md.formals.zip(actuals))
            yield (
            if (form.ty != act._1.ty)
              throw new EvaluationException("Type error in method %s: formal %s has type %s, but is given type %s at %s".format(md.name, form.ty, act._1.ty, md.loc))
            else
              (form.name, act))
        val (ret, fenv) = evaluateBlock(ctx, env binds_new form_bind, md.body)
        val conVal = ret match {
          case v: ValueWAbstr => v._1
          case _              => None
        }
        //val adexp = actuals.head
        val new_ret = md.annot match {
          case None => ret
          case Some(v) => v match {
            case v: FunAnnot => {
              val list_stm: List[Statement] = actuals.map(x => Statement.sCreator(x._2.label, md.annot.asInstanceOf[FunAnnot]))
              (conVal, list_stm.foreach { actuals.head._2.addExpStm(_) })
            }
            case v: LabelAnnot[LMH.LMHV] => (conVal, ADExp.newADExp(Label.newLabel(v)))
            case _                       => throw new EvaluationException("Unknown annotation type")
          }
        }
        (new_ret, env update_values fenv)
      }
    }

  /**
   * Create the set of fields in a class, all empty labels
   */
  def createVars(vars: List[(Type, List[string])]): List[(string, ValueWAbstr)] =
    for ((ty, names) <- vars; name <- names)
      yield (name,
      ty match {
        case TyInt    => (new IntValue(), ADExp.empty)
        case TyBool   => (new BoolValue(), ADExp.empty)
        case TyString => (new StringValue(), ADExp.empty)
        case _        => throw new Unexpected("Variable %s has not supported type %s", (name, ty))
      })

  /**
   * Evaluate the block
   */
  def evaluateBlock(ctx: MethInfo, env: EvEnv, block: Block): (Option[ValueWAbstr], EvEnv) = //(env: Env[String, ConcreteValue]) =
    {
      val nenv: List[(id, ValueWAbstr)] = createVars(block.varDecls map { vd => (vd.ty, vd.ids) })

      val (ret, fenv) = block.stmts.foldLeft(None: Option[ValueWAbstr], env binds_new nenv) {
        case (ret @ (Some(_), _), stmt) => ret
        case ((None, env), stmt)        => evaluateStmt(ctx, env, stmt)
      }
      (ret, env update_values fenv)
      //evaluateStmts(env, block.stmts)
    }
  /*def evaluateStmts(env: EvEnv, stmts: List[Stmt]): (Option[ConcreteValue], EvEnv) =
    stmts match {
      case List() => (None, env)
      case (retStmt @ SReturn(r)) :: List() =>
        evaluateReturn(env, retStmt)
      case (retStmt @ SReturn(r)) :: _ =>
        //warning: to be improved...
        //throw new EvaluationException("Return should be at end of the block at %O", retStmt.loc)
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
  def evaluateStmt(ctx: MethInfo, env: EvEnv, stmt: Stmt): (Option[ValueWAbstr], EvEnv) =
    stmt match {
      case SSkip => (None, env)
      case SAssign(x, e) =>
        val (res, nenv) = evaluateExpr(ctx, env, e)
        if (res._1.ty != nenv.lookup(x)._1.ty)
          throw new EvaluationException("Type error: variable %s has type %s, but is given type %s at %s".format(x, nenv.lookup(x)._1.ty, res._1.ty, stmt.loc.toString()))
        else
          (None, nenv.update(x) { _ => res })
      case SIf(c, thn, els) => //@TODO: collect the implicit!!
        val (cond, nenv) = evaluateExpr(ctx, env, c)
        cond._1 match {
          case BoolValue(v) =>
            if (v)
              evaluateStmt(ctx, nenv, thn)
            else
              evaluateStmt(ctx, nenv, els)
          case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
        }
      case SWhile(c, body) => //@TODO: collect the implicit!!
        val (cond, nenv) = evaluateExpr(ctx, env, c)
        cond._1 match {
          case BoolValue(v) =>
            if (v) {
              evaluateStmt(ctx, nenv, body) match {
                case (None, wenv)       => evaluateStmt(ctx, wenv, stmt)
                case ret @ (Some(_), _) => ret
              }
            }
            else
              (None, nenv)
          case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
        }
      case SBlock(block) => evaluateBlock(ctx, env, block)
      case SReturn(None) => ((Some(UnitValue(), ADExp.empty)), env) //@FIXME: label.empty is not correct!
      case SReturn(Some(e)) =>
        val (res, nenv) = evaluateExpr(ctx, env, e)
        (Some(res), nenv)
      case SPrint(ln, actual) =>
        val (vactual, nenv) = evaluateExpr(ctx, env, actual)
        if (ln) println(vactual._1.value) else print(vactual._1.value)
        (None, nenv)
      case SCall(name, actuals) =>
        applyCall(ctx, env, name, actuals) match {
          case (Some(_), env) => (None, env)
          case (None, env)    => (None, env) //@FIXME: URGENT!!!
        }

      //case rets @ SReturn(_) => evaluateReturn(env, rets)
      case SMethodCall(_, _) => throw new NotSupportedException("Statement Method Call not supported at %s" format stmt.loc)
      case SSetField(_, _)   => throw new NotSupportedException("Set field not supported at %s" format stmt.loc)
    }

  def applyCall(ctx: MethInfo, env: EvEnv, name: String, actuals: List[Expr]) =
    ctx.search(name) match {
      case None => throw new EvaluationException("Could not find the function named %s." format (name))
      case Some((m, cenv)) =>
        val (vacts, nenv) = evaluateActuals(ctx, env, actuals)
        val (ret, fcenv) = evaluateCall(ctx, (m, cenv update_values nenv), vacts)
        (ret, nenv update_values fcenv)
    }

  def evaluateActuals(ctx: MethInfo, env: EvEnv, actuals: List[Expr]): (List[ValueWAbstr], EvEnv) =
    actuals.foldLeft((List[ValueWAbstr](), env)) {
      case ((others, env), expr) =>
        val (v, nenv) = evaluateExpr(ctx, env, expr)
        (others ++ List(v), nenv)
    }

  def evaluateExpr(ctx: MethInfo, env: EvEnv, expr: Expr): (ValueWAbstr, EvEnv) =
    expr match {
      case EVariable(x) =>
        (env.lookup(x), env)
      case EBExpr(op, l, r) =>
        val (lv, nenv) = evaluateExpr(ctx, env, l)
        val (rv, fenv) = evaluateExpr(ctx, nenv, r)
        try
          ((evaluateBinOp(op, lv, rv), fenv))
        catch {
          case EvaluationException(_) =>
            throw new EvaluationException("The evaluation of the binary expression has wrong arguments type at %s" format expr.loc) //%d,%d" format (expr.loc.line, expr.loc.column))
        }
      case EUExpr(op, e) =>
        val (v, nenv) = evaluateExpr(ctx, env, e)
        try
          ((evaluateUnOp(op, v), nenv))
        catch {
          case EvaluationException(_) =>
            throw new EvaluationException("The evaluation of the unary expression has wrong arguments type at %s" format expr.loc)
        }
      case ECall(name, actuals) =>
        applyCall(ctx, env, name, actuals) match {
          case (None, _)                     => throw new EvaluationException("The function %s is void so it cannot be used in an expression call at %s" format (name, expr.loc))
          case (Some(ret: ValueWAbstr), env) => (ret, env)
        }
      case ELit(IntLit(v))    => ((IntValue(v), ADExp.empty), env)
      case ELit(BoolLit(v))   => ((BoolValue(v), ADExp.empty), env)
      case ELit(StringLit(v)) => ((StringValue(v), ADExp.empty), env)
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
      (res, lv._2.addExpStm(Statement.sCreator(rv._2.label, op.annot))) //@FIXME: we must create the same record for the second label!
    }

  // Unary operation evaluation. Return the value + the label
  def evaluateUnOp(op: UOperator, v: ValueWAbstr): ValueWAbstr =
    v match {
      case (IntValue(i), lab) =>
        op match {
          case UNeg(ann) => (IntValue(-i), lab.addExpStm(Statement.sCreator(lab.label, ann)))
          case _         => throw new EvaluationException("Type mismatch on unary operation")
        }
      case (BoolValue(b), lab) =>
        op match {
          case UNot(ann) => (BoolValue(!b), lab.addExpStm(Statement.sCreator(lab.label, ann)))
          case _         => throw new EvaluationException("Type mismatch on unary operation")
        }
      case _ => throw new EvaluationException("Type mismatch on unary operation")
    }
}
