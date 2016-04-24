package it.unive.dais.dapa.lib_intervals

import numerical._
import it.unive.dais.dapa.utils.prelude.{MessageException, pretty}

object intervals {

  class intervt private[intervals](val inf: numericalty, val sup: numericalty) extends pretty {
    override def clone() = new intervt(inf = this.inf.clone(), sup = this.sup.clone())
    def pretty: String = intervSprint(this)
  }

  object intervt {
    def top = intervTopSet
    def bottom = intervBottomSet
    def open_right(inf: Int) = new intervt(inf = numericalty.num(-inf), sup = numlSetinf(1))
    def open_left(sup: Int) = new intervt(inf = numlSetinf(1), sup = numericalty.num(sup))
    def interval(inf: Int, sup: Int) = new intervt(inf = numericalty.num(-inf), sup = numericalty.num(sup))
    def point(v: Int) = new intervt(inf = numericalty.num(-v), sup = numericalty.num(v))
  }

  def intervInit = new intervt(inf = numlInit, sup = numlInit)
  def intervSet(b: intervt): intervt = new intervt(inf = numlSet(b.inf), sup = numlSet(b.sup))
  def intervBottomSet: intervt = new intervt(inf = numlSetint(-1), sup = numlSetint(-1))
  def intervTopSet: intervt = new intervt(inf = numlSetinf(1), sup = numlSetinf(1))

  def intervCanonic(a: intervt): Boolean = {
    if (numlInf(a.inf) || numlInf(a.sup))
      false
    numlCompare(a.sup, numerical.numlNeg(a.inf)) < 0
  }

  def intervBottomCheck(a: intervt): Boolean = intervCanonic(a)
  def intervTopCheck(a: intervt): Boolean = numlInf(a.inf) && numlInf(a.sup)
  def intervPointCheck(a: intervt): Boolean = {
    if (!numlInf(a.inf) && !numlInf(a.sup)) {
      val n = numlNeg(a.inf)
      numerical.numlEqual(n, a.sup)
    }
    else
      false
  }
  def intervOpenRightCheck(a: intervt): Boolean = numlInf(a.sup)
  def intervOpenLefCheck(a: intervt): Boolean = numlInf(a.inf)
  def intervGetLeft(a: intervt): Int = if (!numlInf(a.inf)) -a.inf.num else throw new MessageException("Value is infinite")
  def intervGetRight(a: intervt): Int = if (!numlInf(a.sup)) a.sup.num else throw new MessageException("Value is infinite")
  def intervZeroCheck(a: intervt): Boolean = numlSign(a.inf) == 0 && numlSign(a.sup) == 0
  def intervLeqCheck(a: intervt, b: intervt): Boolean = numlCompare(a.sup, b.sup) <= 0 && numlCompare(a.inf, b.inf) <= 0
  def intervEqCheck(a: intervt, b: intervt): Boolean = numlEqual(a.sup, b.sup) && numlEqual(a.inf, b.inf)

  def intervMeet(b: intervt, c: intervt): (Boolean, intervt) = {
    if (intervBottomCheck(b) || intervBottomCheck(c)) (true, intervt.bottom)
    else {
      val sup = numlMin(b.sup, c.sup)
      val inf = numlMin(b.inf, c.inf)
      val a = new intervt(inf = inf, sup = sup)
      (intervCanonic(a /*,false*/ ), a)
    }
  }
  def intervJoin(b: intervt, c: intervt): intervt = {
    if (intervBottomCheck(b)) c
    else if (intervBottomCheck(c)) b
    else {
    new intervt(
      inf = numlMax(b.inf, c.inf),
      sup = numlMax(b.sup, c.sup))
    }
  }
  def numlWidening(b: numericalty, c: numericalty): numericalty = {
    if (numlInf(c) ||
      numlCompare(b, c) < 0) {
      numlSetinf(+1)
    }
    else {
      numlSet(b)
    }
  }
  def intervWidening(b: intervt, c: intervt): intervt =
    new intervt(inf = numlWidening(b.inf, c.inf), sup = numlWidening(b.sup, c.sup))
  def intervAdd(b: intervt, c: intervt): intervt = {
    if (intervBottomCheck(b) || intervBottomCheck(c)) intervt.bottom
    else new intervt(sup = numlAdd(b.sup, c.sup),
      inf = numlAdd(b.inf, c.inf))
  }

  def intervTrunc(b: intervt): intervt = {
    if (intervBottomCheck(b)) intervt.bottom
    else new intervt(sup = numlTrunc(b.sup), inf = numlTrunc(b.inf))
  }

  def intervPosCheck(a: intervt): Boolean = numlSign(a.inf) <= 0
  def intervNegCheck(a: intervt): Boolean = numlSign(a.sup) <= 0

  def intervMagnitude(b: intervt): numericalty = {
    if (numlSign(b.inf) <= 0) numlSet(b.sup)
    else if (numlSign(b.sup) <= 0) numlSet(b.inf)
    else numlMax(b.inf, b.sup)
  }

  def intervRangeAbs(b: intervt): numericalty = numlAdd(b.sup, b.inf)
  def intervContains(b: intervt, n: Int): Boolean = numlGeq(numlNeg(b.inf), n) && numlLeq(b.sup, n)

  def intervEqat(a: intervt, b: intervt): Set[Boolean] = {
    if (intervBottomCheck(a) || intervBottomCheck(b)) Set.empty[Boolean]
    if (intervPointCheck(a) && intervPointCheck(b)) Set(intervEqCheck(a, b))
    else {
      val (is_bot, v) = intervMeet(a, b)
      if (is_bot) Set(false)
      else Set(true, false)
    }
  }

  def intervNeqat(a: intervt, b: intervt): Set[Boolean] = intervEqat(a, b).map { x => !x }
  def intervLtat(a: intervt, b: intervt): Set[Boolean] = {
    if (intervBottomCheck(a) || intervBottomCheck(b)) Set.empty[Boolean]
    if (numlCompare(a.sup, numlNeg(b.inf)) < 0) Set(true)
    else if (numlCompare(numlNeg(a.inf), b.sup) >= 0) Set(false)
    else Set(true, false)
  }

  def intervContains(a: intervt, b: intervt): Boolean =
    numlCompare(numlNeg(a.inf), numlNeg(b.inf)) <= 0 && numlCompare(a.sup, b.sup) >= 0

  def intervLeqat(a: intervt, b: intervt): Set[Boolean] = {
    if (intervBottomCheck(a) || intervBottomCheck(b)) Set.empty[Boolean]
    if (numlCompare(a.sup, numlNeg(b.inf)) < 0) Set(true)
    else if (numlCompare(numlNeg(a.inf), b.sup) > 0) Set(false)
    else {
      if (numlCompare(a.sup, numlNeg(b.inf)) == 0) Set(true)
      else Set(true, false)
    }
  }

  def intervGtat(a: intervt, b: intervt): Set[Boolean] = intervLeqat(a, b).map { x => !x }
  def intervGeqat(a: intervt, b: intervt): Set[Boolean] = intervLtat(a, b).map { x => !x }

  def intervSub(b: intervt, c: intervt): intervt = {
    if (intervBottomCheck(b) || intervBottomCheck(c)) intervt.bottom
    else new intervt(inf = numlAdd(b.inf, c.sup), numlAdd(b.sup, c.inf))
  }
  def intervNeg(b: intervt): intervt = {
    if (intervBottomCheck(b)) intervt.bottom
    else new intervt(inf = numlSet(b.sup), sup = numlSet(b.inf))
  }
  def intervAbs(b: intervt): intervt = {
    if (intervBottomCheck(b)) intervt.bottom
    else if (numlSign(b.inf) <= 0)
      intervSet(b)
    else if (numlSign(b.sup) <= 0)
      intervNeg(b)
    else {
      new intervt(sup = numlMax(b.inf, b.sup), inf = numlSetint(0))
    }
  }

  def intervMod(b: intervt, c: intervt, is_int: Boolean = true): intervt = {
    if (intervBottomCheck(b) || intervBottomCheck(c))
      intervt.bottom
    else {
      val intern_eval_interv = intervAbs(c)
      var intern_eval_interv_sup = intern_eval_interv.sup
      var intern_eval_interv_inf = intern_eval_interv.inf
      if (numlSign(intern_eval_interv.inf) == 0) intervTopSet
      else {
        var intern_eval_other_interv = intervDiv(b, intern_eval_interv)
        intern_eval_other_interv = intervTrunc(intern_eval_other_interv)
        intern_eval_other_interv = intervMul(intern_eval_other_interv, intern_eval_interv)
        if (is_int)
          intern_eval_interv_sup = numlSubInt(intern_eval_interv_sup, 1)
        if (numlSign(b.sup) < 0) {
          intern_eval_interv_inf = numlSet(intern_eval_interv_sup)
          intern_eval_interv_sup = numlSetint(0)
        }
        else if (numlSign(b.inf) > 0)
          intern_eval_interv_inf = numlSet(intern_eval_interv_sup)
        else
          intern_eval_interv_inf = numlSetint(0)
        val a = intervSub(b, intern_eval_other_interv)
        val intern = new intervt(inf = intern_eval_interv_inf, sup = intern_eval_interv_sup)
        intervMeet(a, intern)._2
      }
    }
  }

  def intervMulPos(b: intervt, c: intervt): intervt = {
    assert(numlSign(b.inf) <= 0 && numlSign(c.inf) <= 0)
    val intern_mul_bound = numlNeg(c.inf)
    val a_inf = numlMul(b.inf, intern_mul_bound)
    val a_sup = numlMul(b.sup, c.sup)
    new intervt(inf = a_inf, sup = a_sup)
  }
  def intervMulNeg(b: intervt, c: intervt): intervt = {
    assert(numlSign(b.sup) <= 0 && numlSign(c.sup) <= 0)
    var intern_mul_bound = numlNeg(c.sup)
    intern_mul_bound = numlMul(b.sup, intern_mul_bound)
    val a_sup = numlMul(b.inf, c.inf)
    val a_inf = numlSet(intern_mul_bound)
    new intervt(inf = a_inf, sup = a_sup)
  }
  def intervMulPosNeg(b: intervt, c: intervt): intervt = {
    assert(numlSign(b.inf) <= 0 && numlSign(c.sup) <= 0)
    val intern_mul_bound = numlNeg(b.inf)
    val a_inf = numlMul(b.sup, c.inf)
    val a_sup = numlMul(intern_mul_bound, c.sup)
    new intervt(inf = a_inf, sup = a_sup)
  }
  def intervMulPosSingle(b: intervt, c: intervt): intervt = {
    assert(numlSign(c.inf) <= 0)
    if (numlSign(b.inf) <= 0) {
      intervMulPos(b, c)
    }
    else if (numlSign(b.sup) <= 0) {
      intervMulPosNeg(c, b)
    }
    else {
      val a_inf = numlMul(b.inf, c.sup)
      val a_sup = numlMul(b.sup, c.sup)
      new intervt(inf = a_inf, sup = a_sup)
    }
  }
  def intervMulNegSingle(b: intervt, c: intervt): intervt = {
    assert(numlSign(c.sup) <= 0)
    if (numlSign(b.inf) <= 0) {
      intervMulPosNeg(b, c)
    }
    else if (numlSign(b.sup) <= 0) {
      intervMulNeg(b, c)
    }
    else {
      val a_sup = numlMul(b.sup, c.inf)
      val a_inf = numlMul(b.inf, c.inf)
      new intervt(inf = a_sup, sup = a_inf)
    }
  }

  def intervMul(b: intervt, c: intervt): intervt = {
    if (intervBottomCheck(b) || intervBottomCheck(c)) intervt.bottom
    else if (numlSign(c.inf) <= 0) {
      intervMulPosSingle(b, c)
    }
    else if (numlSign(c.sup) <= 0) {
      intervMulNegSingle(b, c)
    }
    else if (numlSign(b.inf) <= 0) {
      intervMulPosSingle(c, b)
    }
    else if (numlSign(c.sup) <= 0) {
      intervMulNegSingle(c, b)
    }
    else {
      var intern_mul_interv_inf = numlSet(c.inf)
      var intern_mul_interv_sup = numlSetint(0)
      val intern_mul_interv = intervMulNegSingle(b, new intervt(inf = intern_mul_interv_inf, sup = intern_mul_interv_sup))
      intern_mul_interv_inf = numlSetint(0)
      intern_mul_interv_sup = numlSet(c.sup)
      val tmp = intervMulPosSingle(b, new intervt(inf = intern_mul_interv_inf, sup = intern_mul_interv_sup))
      intervJoin(tmp, intern_mul_interv)
    }
  }
  def intervDivPosPos(b: intervt, c: intervt): intervt = {
    assert(numlSign(b.inf) <= 0 && numlSign(c.inf) < 0)
    val intern_mul_bound = numlNeg(c.inf)
    new intervt(inf = numlDiv(b.inf, c.sup), sup = numlDiv(b.sup, intern_mul_bound))
  }
  def intervDivNegNeg(b: intervt, c: intervt): intervt = {
    assert(numlSign(b.sup) <= 0 && numlSign(c.sup) < 0)
    val intern_mul_bound = numlNeg(b.inf)
    new intervt(inf = numlDiv(b.sup, c.inf), sup = numlDiv(intern_mul_bound, c.sup))
  }
  def intervDivPosNeg(b: intervt, c: intervt): intervt = {
    assert(numlSign(b.inf) <= 0 && numlSign(c.sup) < 0)
    var intern_mul_bound = numlNeg(b.sup)
    intern_mul_bound = numlDiv(intern_mul_bound, c.sup)
    val a_sup = numlDiv(b.inf, c.inf)
    val a_inf = numlSet(intern_mul_bound)
    new intervt(sup = a_sup, inf = a_inf)
  }
  def intervDivNegPos(b: intervt, c: intervt): intervt = {
    assert(numlSign(b.sup) <= 0 && numlSign(c.inf) < 0)
    val intern_mul_bound = numlNeg(b.inf)
    val a_inf = numlDiv(intern_mul_bound, c.inf)
    val a_sup = numlDiv(b.sup, c.sup)
    new intervt(inf = a_inf, sup = a_sup)
  }

  def intervDivPosSingle(b: intervt, c: intervt): intervt = {
    assert(numlSign(c.inf) < 0)
    if (numlSign(b.inf) <= 0) {
      intervDivPosPos(b, c)
    }
    else if (numlSign(b.sup) <= 0) {
      intervDivNegPos(b, c)
    }
    else {
      val intern_mul_bound = numlNeg(c.inf)
      val a_inf = numlDiv(b.inf, intern_mul_bound)
      val a_sup = numlDiv(b.sup, intern_mul_bound)
      new intervt(inf = a_inf, sup = a_sup)
    }
  }
  def intervDivNegSingle(b: intervt, c: intervt): intervt = {
    assert(numlSign(c.sup) < 0)
    if (numlSign(b.inf) <= 0) {
      intervDivPosNeg(b, c)
    }
    else if (numlSign(b.sup) <= 0) {
      intervDivNegNeg(b, c)
    }
    else {
      val a_inf = numlDiv(b.sup, c.sup)
      val a_sup = numlDiv(b.inf, c.sup)
      new intervt(inf = a_inf, sup = a_sup)
    }
  }

  def intervDiv(b: intervt, c: intervt): intervt = {
    if (intervBottomCheck(b) || intervBottomCheck(c)) intervt.bottom
    else if (intervZeroCheck(c) && !intervContains(b, 0)) intervt.bottom
    else if (numlSign(c.inf) < 0) {
      intervDivPosSingle(b, c)
    }
    else if (numlSign(c.sup) < 0) {
      intervDivNegSingle(b, c)
    }
    else if (numlSign(b.inf) == 0 && numlSign(b.sup) == 0) {
      intervSet(b)
    }
    else {
      intervTopSet
    }
  }

  def intervPrint(a: intervt): Unit = print(intervSprint(a))
  def intervSprint(a: intervt): String = {
    if (intervBottomCheck(a)) "BOTTOM"
    else {
      var res = ""
      res = res + "["
      if (numlInf(a.inf))
        res += "-oo"
      else {
        res += -a.inf.numlRef
      }
      res += ","
      res += numlSprint(a.sup)
      res += "]"
      res
    }
  }
}
