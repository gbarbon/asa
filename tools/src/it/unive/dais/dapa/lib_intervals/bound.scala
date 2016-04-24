package it.unive.dais.dapa.lib_intervals

import it.unive.dais.dapa.utils.prelude.pretty

/**
  * @author esteffin
  */
object bound {

  def sign(x: Int) = if (x > 0) 1 else if (x < 0) -1 else 0


  class bound_t(val num: Int, val inf: Boolean) extends pretty {
    def bound_numref = num
    override def clone() = new bound_t(num = this.num, inf = this.inf)
    override def pretty: String = bound_sprint(this)
  }

  object bound_t {
    def zero: bound_t = new bound_t(0, false)
    def num(n: Int): bound_t = new bound_t(n, false)
  }

  def bound_infty(a: bound_t): Boolean = { a.inf }

  def bound_set_infty(sgn: Int): bound_t = {
    if (sgn == 0) throw new RuntimeException("Sign must be different than 0")
    new bound_t(if (sgn > 0) 1 else -1, true)
  }

  def bound_init_set_infty(a: bound_t, sgn: Int): bound_t = { bound_set_infty(sgn) }
  def bound_sgn(a: bound_t): Int = sign(a.num)
  def bound_set(b: bound_t): bound_t = new bound_t(b.num, b.inf)
  def bound_set_int(i: Int): bound_t = new bound_t(i, false)
  def bound_set_num(i: Int): bound_t = new bound_t(i, false)

  /* ====================================================================== */
  /* Constructors and Destructors */
  /* ====================================================================== */

  def bound_init: bound_t = bound_t.zero

  /* ====================================================================== */
  /* Arithmetic Operations */
  /* ====================================================================== */

  /* +oo + -oo  \
   -oo + +oo  | undefined
   +oo - +oo  |
   -oo - -oo  /

   +oo + x = +oo - x = x - -oo = +oo
   -oo + x = -oo - x = x - +oo = -oo

   0 * +oo = +oo * 0 = 0 * -oo = -oo * 0 = 0
   x * +oo = +oo * x =  sign(x) * oo  if x!=0
   x * -oo = -oo * x = -sign(x) * oo  if x!=0

   0 / x = x / +oo = x / -oo = 0
   x / 0 = sign(x) * oo     if x!=0
   +oo / x =  sign(x) * oo  if x!=0,+oo,-oo
   -oo / x = -sign(x) * oo  if x!=0,+oo,-oo
  */

  def bound_neg(b: bound_t): bound_t = {
    if (bound_infty(b)) bound_set_infty(-bound_sgn(b))
    else bound_t.num(-b.num)
  }

  def bound_abs(b: bound_t): bound_t = { new bound_t(scala.math.abs(b.num), b.inf) }

  def bound_add(b: bound_t, c: bound_t): bound_t = {
    if (bound_infty(b)) bound_set_infty(bound_sgn(b))
    else if (bound_infty(c)) bound_set_infty(bound_sgn(c))
    else new bound_t(b.num + c.num, false)
  }

  def bound_lt(b: bound_t, c: Int): Boolean = {
    if (b.inf)
      bound_sgn(b) > 0
    else
      c < b.num
  }

  def bound_leq(b: bound_t, c: Int): Boolean = {
    if (b.inf)
      bound_sgn(b) > 0
    else
      c <= b.num
  }

  def bound_gt(b: bound_t, c: Int): Boolean = {
    if (b.inf)
      bound_sgn(b) < 0
    else
      c > b.num
  }

  def bound_geq(b: bound_t, c: Int): Boolean = {
    if (b.inf)
      bound_sgn(b) < 0
    else
      c >= b.num
  }

  def bound_sub(b: bound_t, c: bound_t): bound_t = {
    if (bound_infty(b)) bound_set_infty(bound_sgn(b))
    else if (bound_infty(c)) bound_set_infty(-bound_sgn(c))
    else new bound_t(b.num - c.num, false)
  }

  def bound_sub_uint(b: bound_t, c: Int): bound_t = {
    if (bound_infty(b)) new bound_t(inf = b.inf, num = b.num)
    else { new bound_t(inf = false, num = b.num - c) }
  }

  def bound_mul(b: bound_t, c: bound_t): bound_t = {
    if (bound_sgn(b) == 0 || bound_sgn(c) == 0) bound_set_int(0)
    else if (bound_infty(b) || bound_infty(c)) bound_set_infty(bound_sgn(b) * bound_sgn(c))
    else new bound_t(b.num * c.num, false)
  }

  def bound_div(b: bound_t, c: bound_t): bound_t = {
    if (bound_sgn(b) == 0 || bound_infty(c)) bound_set_int(0)
    else if (bound_sgn(c) == 0) bound_set_infty(bound_sgn(b))
    else if (bound_infty(b)) bound_set_infty(bound_sgn(b) * bound_sgn(c))
    else new bound_t(b.num / c.num, false)
  }

  def bound_min(b: bound_t, c: bound_t): bound_t = {
    if (bound_infty(b)) if (bound_sgn(b) > 0) bound_set(c) else bound_set(b)
    else if (bound_infty(c)) if (bound_sgn(c) > 0) bound_set(b) else bound_set(c)
    else new bound_t(math.min(b.num, c.num), false)
  }
  def bound_max(b: bound_t, c: bound_t): bound_t = {
    if (bound_infty(b)) if (bound_sgn(b) > 0) bound_set(b) else bound_set(c)
    else if (bound_infty(c)) if (bound_sgn(c) > 0) bound_set(c) else bound_set(b)
    else new bound_t(math.max(b.num, c.num), false)
  }

  def bound_trunc(b: bound_t): bound_t = {
    if (bound_infty(b)) bound_set_infty(bound_sgn(b))
    else { new bound_t(num = b.num, inf = false) }
  }

  /* ====================================================================== */
  /* Arithmetic Tests */
  /* ====================================================================== */

  def bound_cmp(a: bound_t, b: bound_t): Int = {
    if (bound_infty(a)) {
      if (bound_infty(b)) (bound_sgn(a) - bound_sgn(b)) / 2
      else bound_sgn(a)
    }
    else {
      if (bound_infty(b)) -bound_sgn(b)
      else a.num.compare(b.num)
    }
  }

  def bound_equal(a: bound_t, b: bound_t): Boolean = {
    if (bound_infty(a)) {
      bound_infty(b) && bound_sgn(a) == bound_sgn(b)
    }
    else {
      if (bound_infty(b)) false
      else a.num == b.num
    }
  }

  def bound_print(a: bound_t): Unit = {
    if (bound_infty(a)) print("%coo" format (if (bound_sgn(a) > 0) '+' else '-'))
    else print("%d" format a.num)
  }

  def bound_sprint(a: bound_t): String =
  {
    if (bound_infty(a)) "%coo".format(if (bound_sgn(a) > 0) '+' else '-')
    else "%d" format a.num
  }
}
