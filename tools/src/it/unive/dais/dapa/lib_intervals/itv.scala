/*
 * This code is derived from the numerical abstract domain library APRON.
 * Here is the APRON library copyright notice (LGPL). 
 * The Apron Numerical Abstract Domain Library 
 * (https://github.com/antoinemine/apron and 
 *  https://antoinemine.github.io/Apron/doc/).
 * The original licence of APRON is included in "lib_interval/APRON_LICENCE".
 */

package it.unive.dais.dapa.lib_intervals

import bound._
import it.unive.dais.dapa.utils.prelude.{MessageException, pretty}

/**
  * @author esteffin
  */
object itv {

  /* ********************************************************************** */
  /* itv.h: (unidimensional) intervals */
  /* ********************************************************************** */

  /* Be cautious: interval [a,b] is represented by [-a,b].  This is because
   bound quantities are always rounded toward +infty */

  class itv_t private[itv](val inf: bound_t, val sup: bound_t) extends pretty {
    /* negation of the inf bound */
    /* sup bound */

/*    override def equals(o: Any) = o match {
      case that: itv_t => (that.inf == this.inf) && (that.sup)
      case _ => false
    }
    override def hashCode = name.toUpperCase.hashCode*/

    override def clone() = new itv_t(inf = this.inf.clone(), sup = this.sup.clone())

    def pretty: String = itv_sprint(this)
  }

  object itv_t {
    def top = itv_set_top
    def bottom = itv_set_bottom
    def open_right(inf: Int) = new itv_t(inf = bound_t.num(-inf), sup = bound_set_infty(1))
    def open_left(sup: Int) = new itv_t(inf = bound_set_infty(1), sup = bound_t.num(sup))
    def interval(inf: Int, sup: Int) = new itv_t(inf = bound_t.num(-inf), sup = bound_t.num(sup))
    def point(v: Int) = new itv_t(inf = bound_t.num(-v), sup = bound_t.num(v))
  }

  /* ********************************************************************** */
  /* itv.h: (unidimensional) intervals */
  /* ********************************************************************** */

  /* Be cautious: interval [a,b] is represented by [-a,b].  This is because
   bound quantities are always rounded toward +infty */

  /* ********************************************************************** */
  /* itv */
  /* ********************************************************************** */

  /* Canonicalize an interval:
     - if integer is true, narrows bound to integers
     - return true if the interval is bottom
     - return false otherwise
  */

  def itv_init = new itv_t(inf = bound_init, sup = bound_init)
  //def itv_init_set(b : itv_t ) : itv_t = new itv_t(inf = bound_init_set(b.inf), sup = bound_init_set(b.sup))
  def itv_set(b: itv_t): itv_t = new itv_t(inf = bound_set(b.inf), sup = bound_set(b.sup))
  def itv_set_bottom: itv_t = new itv_t(inf = bound_set_int(-1), sup = bound_set_int(-1))
  def itv_set_top: itv_t = new itv_t(inf = bound_set_infty(1), sup = bound_set_infty(1))


  /* ********************************************************************** */
  /* Normalization and tests */
  /* ********************************************************************** */

  /* If integer is true, narrow the interval to integer bounds.
   In any case, return true if the interval is bottom*/
  def itv_canonicalize(a: itv_t /*, integer : Boolean*/ ): Boolean =
  {
    /*if (integer){
  bound_floor(a.inf,a.inf);
  bound_floor(a.sup,a.sup);
}*/
    if (bound_infty(a.inf) || bound_infty(a.sup))
      false

    /* Check that it is not bottom */
    /*
exc = false;
num_neg(intern.canonicalize_num,bound_numref(a.inf));
 */
    bound_cmp(a.sup, bound.bound_neg(a.inf)) < 0
  }

  def itv_is_bottom(a: itv_t): Boolean =
  {
    itv_canonicalize(a)
  }
  def itv_is_top(a: itv_t): Boolean = bound_infty(a.inf) && bound_infty(a.sup)
  def itv_is_point(a: itv_t): Boolean = {
    if (!bound_infty(a.inf) && !bound_infty(a.sup)) {
      val n = bound_neg(a.inf)
      bound.bound_equal(n, a.sup)
    }
    else
      false
  }
  def itv_is_open_right(a: itv_t): Boolean = bound_infty(a.sup)
  def itv_is_open_left(a: itv_t): Boolean = bound_infty(a.inf)
  def itv_get_left(a: itv_t): Int = if (!bound_infty(a.inf)) -a.inf.num else throw new MessageException("Value is infinite")
  def itv_get_right(a: itv_t): Int = if (!bound_infty(a.sup)) a.sup.num else throw new MessageException("Value is infinite")
  def itv_is_zero(a: itv_t): Boolean = {
    bound_sgn(a.inf) == 0 && bound_sgn(a.sup) == 0
  }
  def itv_is_leq(a: itv_t, b: itv_t): Boolean = {
    bound_cmp(a.sup, b.sup) <= 0 && bound_cmp(a.inf, b.inf) <= 0
  }
  def itv_is_eq(a: itv_t, b: itv_t): Boolean = {
    bound_equal(a.sup, b.sup) && bound_equal(a.inf, b.inf)
  }

  def itv_meet(b: itv_t, c: itv_t): (Boolean, itv_t) = {
    if (itv_is_bottom(b) || itv_is_bottom(c)) (true, itv_t.bottom)
    else {
      val sup = bound_min(b.sup, c.sup)
      val inf = bound_min(b.inf, c.inf)
      val a = new itv_t(inf = inf, sup = sup)
      (itv_canonicalize(a /*,false*/ ), a)
    }
  }
  def itv_join(b: itv_t, c: itv_t): itv_t = {
    if (itv_is_bottom(b)) c
    else if (itv_is_bottom(c)) b
    else {
    new itv_t(
      inf = bound_max(b.inf, c.inf),
      sup = bound_max(b.sup, c.sup))
    }
  }
  def bound_widening(b: bound_t, c: bound_t): bound_t =
  {
    if (bound_infty(c) ||
      bound_cmp(b, c) < 0) {
      bound_set_infty(+1)
    }
    else {
      bound_set(b)
    }
  }
  def itv_widening(b: itv_t, c: itv_t): itv_t = {
    new itv_t(inf = bound_widening(b.inf, c.inf),
      sup = bound_widening(b.sup, c.sup))
  }
  def itv_add(b: itv_t, c: itv_t): itv_t = {
    if (itv_is_bottom(b) || itv_is_bottom(c)) itv_t.bottom
    else new itv_t(sup = bound_add(b.sup, c.sup),
      inf = bound_add(b.inf, c.inf))
  }
  

  def itv_trunc(b: itv_t): itv_t =
  {
    if (itv_is_bottom(b)) itv_t.bottom
    else new itv_t(sup = bound_trunc(b.sup), inf = bound_trunc(b.inf))
  }

  def itv_is_pos(a: itv_t): Boolean =
  { bound_sgn(a.inf) <= 0 }

  def itv_is_neg(a: itv_t): Boolean =
  { bound_sgn(a.sup) <= 0 }

  def itv_magnitude(b: itv_t): bound_t = {
    if (bound_sgn(b.inf) <= 0) bound_set(b.sup)
    else if (bound_sgn(b.sup) <= 0) bound_set(b.inf)
    else bound_max(b.inf, b.sup)
  }

  def itv_range_abs(b: itv_t): bound_t =
  { bound_add(b.sup, b.inf) }

  def itv_contains(b: itv_t, n: Int): Boolean =
    bound_geq(bound_neg(b.inf), n) && bound_leq(b.sup, n)

  def itv_eqat(a: itv_t, b: itv_t): Set[Boolean] = {
    if (itv_is_bottom(a) || itv_is_bottom(b)) Set.empty[Boolean]
    if (itv_is_point(a) && itv_is_point(b)) Set(itv_is_eq(a, b))
    else {
      val (is_bot, v) = itv_meet(a, b)
      if (is_bot) Set(false)
      else Set(true, false)
    }
  }

  def itv_neqat(a: itv_t, b: itv_t): Set[Boolean] = {
    itv_eqat(a, b).map { x => !x }
  }

  def itv_ltat(a: itv_t, b: itv_t): Set[Boolean] = {
    if (itv_is_bottom(a) || itv_is_bottom(b)) Set.empty[Boolean]
    if (bound_cmp(a.sup, bound_neg(b.inf)) < 0) Set(true)
    else if (bound_cmp(bound_neg(a.inf), b.sup) >= 0) Set(false)
    else Set(true, false)
  }

  def itv_contains(a: itv_t, b: itv_t): Boolean = {
    bound_cmp(bound_neg(a.inf), bound_neg(b.inf)) <= 0 && bound_cmp(a.sup, b.sup) >= 0
  }

  def itv_leqat(a: itv_t, b: itv_t): Set[Boolean] = {
    if (itv_is_bottom(a) || itv_is_bottom(b)) Set.empty[Boolean]
    if (bound_cmp(a.sup, bound_neg(b.inf)) < 0) Set(true)
    else if (bound_cmp(bound_neg(a.inf), b.sup) > 0) Set(false)
    else {
      if (bound_cmp(a.sup, bound_neg(b.inf)) == 0) Set(true)
      else Set(true, false)
    }
  }

  def itv_gtat(a: itv_t, b: itv_t): Set[Boolean] = {
    itv_leqat(a, b).map { x => !x }
  }

  def itv_geqat(a: itv_t, b: itv_t): Set[Boolean] = {
    itv_ltat(a, b).map { x => !x }
  }


  /* ********************************************************************** */
  /* Arithmetic operations */
  /* ********************************************************************** */

  /* We assume no aliasing between

   - an itv and a num or a bound,
  */

  def itv_sub(b: itv_t, c: itv_t): itv_t =
  {
    if (itv_is_bottom(b) || itv_is_bottom(c)) itv_t.bottom
    //TODO: really??
    //if (a!=c){
    else new itv_t(inf = bound_add(b.inf, c.sup), bound_add(b.sup, c.inf))
    /*} else if (a!=b) { /* a=c */
  bound_swap(c.sup,c.inf);
  itv_add(a,b,c);
} else { /* a=b=c */
  bound_add(a.sup,b.sup,c.inf);
  bound_set(a.inf,a.sup);
}*/
  }
  def itv_neg(b: itv_t): itv_t = {
    if (itv_is_bottom(b)) itv_t.bottom
    else new itv_t(inf = bound_set(b.sup), sup = bound_set(b.inf))
  }

  def itv_abs(b: itv_t): itv_t =
  {
    if (itv_is_bottom(b)) itv_t.bottom
    else if (bound_sgn(b.inf) <= 0)
    /* positive interval */
      itv_set(b)
    else if (bound_sgn(b.sup) <= 0)
    /* negative interval */
      itv_neg(b)
    else {
      new itv_t(sup = bound_max(b.inf, b.sup), inf = bound_set_int(0))
    }
  }

  /* x mod y = x - y*trunc(x/y) */
  def itv_mod(b: itv_t, c: itv_t, is_int: Boolean = true): itv_t =
  {
    if (itv_is_bottom(b) || itv_is_bottom(c)) itv_t.bottom
    /* b-|c|*trunc(b/|c|) */
    /*val fst = itv_sub(b, itv_abs(c)) // b-|c|
    val arg = itv_div(b, itv_abs(c)) //b/|c|
    itv_mul(fst, itv_trunc(arg))*/

    /* due errori:
     * 1. intern vale 0 - 2 anzichè 0 - 3 (anche se 0 - 2 è più giusto)
     * 2. a vale 1 - 4 e non -1 - 4       (come invece dovrebbe fare... E fa se si usa b-|c|*trunc(b/|c|) secca)
     *
     * idea:
     * usare b-|c|*trunc(b/|c|)
     * fixare intern...
     */
    else {
      val intern_eval_itv = itv_abs(c)
      var intern_eval_itv_sup = intern_eval_itv.sup
      var intern_eval_itv_inf = intern_eval_itv.inf
      if (bound_sgn(intern_eval_itv.inf) == 0) itv_set_top
      else {
        var intern_eval_itv2 = itv_div(b, intern_eval_itv)
        intern_eval_itv2 = itv_trunc(intern_eval_itv2)
        intern_eval_itv2 = itv_mul(intern_eval_itv2, intern_eval_itv)
        if (is_int)
          intern_eval_itv_sup = bound_sub_uint(intern_eval_itv_sup, 1)
        if (bound_sgn(b.sup) < 0) {
          /* [-max|c|,0] */
          intern_eval_itv_inf = bound_set(intern_eval_itv_sup)
          intern_eval_itv_sup = bound_set_int(0)
        }
        else if (bound_sgn(b.inf) > 0)
        /* [-max|c|,max|c|] */
          intern_eval_itv_inf = bound_set(intern_eval_itv_sup)
        else
        /* [0,max|c|] */
          intern_eval_itv_inf = bound_set_int(0)
        val a = itv_sub(b, intern_eval_itv2)
        //println("PRE MEET: " + a)
        val intern = new itv_t(inf = intern_eval_itv_inf, sup = intern_eval_itv_sup)
        //println(intern)
        itv_meet(a, intern)._2
      }
    }
  }

  /* ====================================================================== */
  /* Multiplication */
  /* ====================================================================== */

  /* Assume that both intervals are positive */
  def itv_mulpp(b: itv_t, c: itv_t): itv_t =
  {
    assert(bound_sgn(b.inf) <= 0 && bound_sgn(c.inf) <= 0)
    val intern_mul_bound = bound_neg(c.inf)
    val a_inf = bound_mul(b.inf, intern_mul_bound)
    val a_sup = bound_mul(b.sup, c.sup)
    new itv_t(inf = a_inf, sup = a_sup)
  }
  /* Assume that both intervals are negative */
  def itv_mulnn(b: itv_t, c: itv_t): itv_t =
  {
    assert(bound_sgn(b.sup) <= 0 && bound_sgn(c.sup) <= 0)
    var intern_mul_bound = bound_neg(c.sup)
    intern_mul_bound = bound_mul(b.sup, intern_mul_bound)
    val a_sup = bound_mul(b.inf, c.inf)
    val a_inf = bound_set(intern_mul_bound)
    new itv_t(inf = a_inf, sup = a_sup)
  }
  /* Assume that b is positive and c negative */
  def itv_mulpn(b: itv_t, c: itv_t): itv_t =
  {
    assert(bound_sgn(b.inf) <= 0 && bound_sgn(c.sup) <= 0)
    val intern_mul_bound = bound_neg(b.inf)
    val a_inf = bound_mul(b.sup, c.inf)
    val a_sup = bound_mul(intern_mul_bound, c.sup)
    new itv_t(inf = a_inf, sup = a_sup)
  }
  /* Assume that interval c is positive */
  def itv_mulp(b: itv_t, c: itv_t): itv_t =
  {
    assert(bound_sgn(c.inf) <= 0)

    if (bound_sgn(b.inf) <= 0) {
      /* b is positive */
      itv_mulpp(b, c)
    }
    else if (bound_sgn(b.sup) <= 0) {
      /* b is negative */
      itv_mulpn(c, b)
    }
    else {
      /* 0 is in the middle of b: one multiplies b by c.sup */
      val a_inf = bound_mul(b.inf, c.sup)
      val a_sup = bound_mul(b.sup, c.sup)
      new itv_t(inf = a_inf, sup = a_sup)
    }
  }
  /* Assume that interval c is negative */
  def itv_muln(b: itv_t, c: itv_t): itv_t =
  {
    assert(bound_sgn(c.sup) <= 0)

    if (bound_sgn(b.inf) <= 0) {
      /* b is positive */
      itv_mulpn(b, c)
    }
    else if (bound_sgn(b.sup) <= 0) {
      /* b is negative */
      itv_mulnn(b, c)
    }
    else {
      /* 0 is in the middle of b: one multiplies b by c.inf */
      val a_sup = bound_mul(b.sup, c.inf)
      val a_inf = bound_mul(b.inf, c.inf)
      new itv_t(inf = a_sup, sup = a_inf)
    }
  }

  def itv_mul(b: itv_t, c: itv_t): itv_t =
  {
    if (itv_is_bottom(b) || itv_is_bottom(c)) itv_t.bottom
    else if (bound_sgn(c.inf) <= 0) {
      //println("mulp b c")
      /* c is positive, */
      itv_mulp(b, c)
    }
    else if (bound_sgn(c.sup) <= 0) {
      //println("muln b c")
      /* c is negative */
      itv_muln(b, c)
    }
    else if (bound_sgn(b.inf) <= 0) {
      //println("mulp c b")
      /* b is positive, */
      itv_mulp(c, b)
    }
    else if (bound_sgn(c.sup) <= 0) {
      //println("muln c b")
      /* b is negative */
      itv_muln(c, b)
    }
    else {
      //println("divide c")
      /* divide c */
      var intern_mul_itv_inf = bound_set(c.inf)
      var intern_mul_itv_sup = bound_set_int(0)
      val intern_mul_itv2 = itv_muln(b, new itv_t(inf = intern_mul_itv_inf, sup = intern_mul_itv_sup))

      intern_mul_itv_inf = bound_set_int(0)
      intern_mul_itv_sup = bound_set(c.sup)
      val a = itv_mulp(b, new itv_t(inf = intern_mul_itv_inf, sup = intern_mul_itv_sup))

      itv_join(a, intern_mul_itv2)
    }
  }

  /* ====================================================================== */
  /* Division */
  /* ====================================================================== */

  /* Assume that both intervals are positive */
  def itv_divpp(b: itv_t, c: itv_t): itv_t = {
    assert(bound_sgn(b.inf) <= 0 && bound_sgn(c.inf) < 0)
    val intern_mul_bound = bound_neg(c.inf)
    new itv_t(inf = bound_div(b.inf, c.sup), sup = bound_div(b.sup, intern_mul_bound))
  }
  /* Assume that both intervals are negative */
  def itv_divnn(b: itv_t, c: itv_t): itv_t = {
    assert(bound_sgn(b.sup) <= 0 && bound_sgn(c.sup) < 0)
    val intern_mul_bound = bound_neg(b.inf)
    new itv_t(inf = bound_div(b.sup, c.inf), sup = bound_div(intern_mul_bound, c.sup))
  }
  /* Assume that b is positive and c negative */
  def itv_divpn(b: itv_t, c: itv_t): itv_t = {
    assert(bound_sgn(b.inf) <= 0 && bound_sgn(c.sup) < 0)
    var intern_mul_bound = bound_neg(b.sup)
    intern_mul_bound = bound_div(intern_mul_bound, c.sup)
    val a_sup = bound_div(b.inf, c.inf)
    val a_inf = bound_set(intern_mul_bound)
    new itv_t(sup = a_sup, inf = a_inf)
  }
  /* Assume that b is negative and c positive */
  def itv_divnp(b: itv_t, c: itv_t): itv_t = {
    assert(bound_sgn(b.sup) <= 0 && bound_sgn(c.inf) < 0)
    val intern_mul_bound = bound_neg(b.inf)
    val a_inf = bound_div(intern_mul_bound, c.inf)
    val a_sup = bound_div(b.sup, c.sup)
    new itv_t(inf = a_inf, sup = a_sup)
  }

  /* Assume that interval c is positive */
  def itv_divp(b: itv_t, c: itv_t): itv_t = {
    assert(bound_sgn(c.inf) < 0)

    if (bound_sgn(b.inf) <= 0) {
      /* b is positive */
      itv_divpp(b, c)
    }
    else if (bound_sgn(b.sup) <= 0) {
      /* b is negative */
      itv_divnp(b, c)
    }
    else {
      /* 0 is in the middle of b: one divides b by c.inf */
      val intern_mul_bound = bound_neg(c.inf)
      val a_inf = bound_div(b.inf, intern_mul_bound)
      val a_sup = bound_div(b.sup, intern_mul_bound)
      new itv_t(inf = a_inf, sup = a_sup)
    }
  }
  /* Assume that interval c is negative */
  def itv_divn(b: itv_t, c: itv_t): itv_t = {
    assert(bound_sgn(c.sup) < 0)

    if (bound_sgn(b.inf) <= 0) {
      /* b is positive */
      itv_divpn(b, c)
    }
    else if (bound_sgn(b.sup) <= 0) {
      /* b is negative */
      itv_divnn(b, c)
    }
    else {
      /* 0 is in the middle of b: one cross-divide b by c.sup */

      val a_inf = bound_div(b.sup, c.sup)
      val a_sup = bound_div(b.inf, c.sup)
      new itv_t(inf = a_inf, sup = a_sup)

      /*/* 0 is in the middle of b: one cross-divide b by c.sup */
      if (a != b) {
        bound_div(a.inf, b.sup, c.sup);
        bound_div(a.sup, b.inf, c.sup);
      }
      else {
        bound_div(intern.mul_bound, b.sup, c.sup);
        bound_div(a.sup, b.inf, c.sup);
        bound_set(a.inf, intern.mul_bound);
      }*/
    }
  }

  def itv_div(b: itv_t, c: itv_t): itv_t = {
    if (itv_is_bottom(b) || itv_is_bottom(c)) itv_t.bottom
    else if (itv_is_zero(c) && !itv_contains(b, 0)) itv_t.bottom
    else if (bound_sgn(c.inf) < 0) {
      /* c is positive */
      itv_divp(b, c)
    }
    else if (bound_sgn(c.sup) < 0) {
      /* c is negative */
      itv_divn(b, c)
    }
    else if (bound_sgn(b.inf) == 0 && bound_sgn(b.sup) == 0) {
      /* b is [0,0] */
      itv_set(b)
    }
    else {
      itv_set_top
    }
  }

  /* ********************************************************************** */
  /* Printing */
  /* ********************************************************************** */

  def itv_print(a: itv_t): Unit = { print(itv_sprint(a)) }

  def itv_sprint(a: itv_t): String = {
    if (itv_is_bottom(a)) "BOTTOM"
    else {
      var res = ""
      res = res + "["
      if (bound_infty(a.inf))
        res += "-oo"
      else {
        res += -a.inf.bound_numref
      }
      res += ","
      res += bound_sprint(a.sup)
      res += "]"
      res
    }
  }
}
