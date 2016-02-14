package it.unive.dais.yaasa.lib_intervals

import bound._
import it.unive.dais.yaasa.utils.prelude.{MessageException, pretty}

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

  //static inline void itv_range_abs(bound_t a, itv_t b);
  /* a=(max b - min b) */
  //static inline void itv_range_rel(itv_internal_t* intern, bound_t a, itv_t b);
  /* a=(max b - min b) / (|a+b|/2) */

  /* Lattice operations */
  //static inline bool itv_meet(itv_internal_t* intern, itv_t a, itv_t b, itv_t c);
  /* Assign a with the intersection of b and c */
  //static inline void itv_join(itv_t a, itv_t b, itv_t c);
  /* Assign a with the union of b and c */
  //static inline void itv_widening(itv_t a, itv_t b, itv_t c);
  /* Assign a with the standard interval widening of b by c */

  /* Arithmetic operations */
  /*
static inline void itv_add(itv_t a, itv_t b, itv_t c);
static inline void itv_sub(itv_t a, itv_t b, itv_t c);
static inline void itv_neg(itv_t a, itv_t b);
static inline void itv_mul(itv_internal_t* intern, itv_t a, itv_t b, itv_t c);
static inline void itv_div(itv_internal_t* intern, itv_t a, itv_t b, itv_t c);
static inline void itv_add_num(itv_t a, itv_t b, num_t c);
static inline void itv_sub_num(itv_t a, itv_t b, num_t c);
static inline void itv_mul_num(itv_t a, itv_t b, num_t c);
static inline void itv_div_num(itv_t a, itv_t b, num_t c);
static inline void itv_add_bound(itv_t a, itv_t b, bound_t c);
static inline void itv_sub_bound(itv_t a, itv_t b, bound_t c);
static inline void itv_mul_bound(itv_t a, itv_t b, bound_t c);
static inline void itv_div_bound(itv_t a, itv_t b, bound_t c);
static inline bool itv_sqrt(itv_internal_t* intern, itv_t a, itv_t b);
static inline void itv_abs(itv_t a, itv_t b);
static inline void itv_mul_2exp(itv_t a, itv_t b, int c);

static inline void itv_magnitude(bound_t a, itv_t b);
  /* get the absolute value of maximal bound */

static inline void itv_mod(itv_internal_t* intern, itv_t a, itv_t b, itv_t c, bool is_int);
  /* x mod y = x - y*trunc(x/y) */

/* Integer casts (rounding towards +oo, -oo, 0, or worst-case) */
static inline void itv_ceil(itv_t a, itv_t b);
static inline void itv_floor(itv_t a, itv_t b);
static inline void itv_trunc(itv_t a, itv_t b);
static inline void itv_to_int(itv_t a, itv_t b);

/* Floating-point casts (worst cases) */
static inline void itv_to_float(itv_t a, itv_t b);
static inline void itv_to_double(itv_t a, itv_t b);

  /* Inverse of the above functions */
static inline void itv_unceil(itv_t a, itv_t b);
static inline void itv_unfloor(itv_t a, itv_t b);
static inline void itv_untrunc(itv_t a, itv_t b);
static inline void itv_from_int(itv_t a, itv_t b);
static inline void itv_from_float(itv_t a, itv_t b);
static inline void itv_from_double(itv_t a, itv_t b);

/* Printing */
static inline int itv_snprint(char* s, size_t size, itv_t a);
static inline void itv_fprint(FILE* stream, itv_t a);
static inline void itv_print(itv_t a);

/* All these functions return true if the conversion is exact */
static inline bool itv_set_ap_scalar(itv_internal_t* intern, itv_t a, ap_scalar_t* b);
  /* Convert a ap_scalar_t into a itv_t.
     Assumes the scalar is finite.
     If it returns true, the interval is a single point */
static inline bool itv_set_ap_interval(itv_internal_t* intern, itv_t a, ap_interval_t* b);
  /* Convert a ap_interval_t into a itv_t */
static inline bool itv_set_ap_coeff(itv_internal_t* intern, itv_t a, ap_coeff_t* b);
  /* Convert a ap_coeff_t into a itv_t. */

static inline bool ap_interval_set_itv(itv_internal_t* intern, ap_interval_t* a, itv_t b);
  /* Convert a itv_t into a ap_interval_t */

static inline bool ap_coeff_set_itv(itv_internal_t* intern, ap_coeff_t* a, itv_t b);
  /* Convert a itv_t into a ap_coeff_t */

static inline bool itv_array_set_ap_interval_array(itv_internal_t* intern, itv_t** ptitv, ap_interval_t** array, size_t size);
  /* Convert an array of ap_interval_t into an array of itv_t.
     The paramater ptitv is a result parameter.
     The result is to be found in *ptitv */
*/

  def itv_init = new itv_t(inf = bound_init, sup = bound_init)

  //def itv_init_set(b : itv_t ) : itv_t = new itv_t(inf = bound_init_set(b.inf), sup = bound_init_set(b.sup))

  /*
static inline void itv_clear(itv_t a)
{
  bound_clear(a.inf);
  bound_clear(a.sup);
}
static inline void itv_clear_array(itv_t* a, size_t size)
{
  (void)a;
  (void)size;
#if !defined(NUM_NATIVE)
  size_t i;
  for (i=0; i<size; i++) itv_clear(a[i]);
#endif
}
static inline itv_t* itv_array_alloc(size_t size)
{
  itv_t* res = (itv_t*)malloc(size*sizeof(itv_t));
  itv_init_array(res,size);
  return res;
}
static inline void itv_array_free(itv_t* a, size_t size)
{
  itv_clear_array(a,size);
  free(a);
}*/

  def itv_set(b: itv_t): itv_t = new itv_t(inf = bound_set(b.inf), sup = bound_set(b.sup))

  /*
static inline void itv_set_num(itv_t a, num_t b)
{
  bound_set_num(a.sup,b);
  bound_neg(a.inf,a.sup);
}
static inline void itv_set_num2(itv_t a, num_t b, num_t c)
{
  num_neg(b,b);
  bound_set_num(a.inf,b);
  num_neg(b,b);
  bound_set_num(a.sup,c);
}
static inline void itv_set_unit_num(itv_t a, num_t b)
{
  bound_set_num(a.inf,b);
  bound_set_num(a.sup,b);
}
static inline void itv_set_unit_bound(itv_t a, bound_t b)
{
  bound_set(a.inf,b);
  bound_set(a.sup,b);
}
static inline void itv_enlarge_bound(itv_t a, itv_t b, bound_t c)
{
  bound_add(a.inf,b.inf,c);
  bound_add(a.sup,b.sup,c);
}
static inline void itv_set_int(itv_t a, long int b)
{
  bound_set_int(a.inf,-b);
  bound_set_int(a.sup,b);
}
static inline void itv_set_int2(itv_t a, long int b, long int c)
{
  bound_set_int(a.inf,-b);
  bound_set_int(a.sup,c);
}
*/

  def itv_set_bottom: itv_t = new itv_t(inf = bound_set_int(-1), sup = bound_set_int(-1))

  def itv_set_top: itv_t = new itv_t(inf = bound_set_infty(1), sup = bound_set_infty(1))

  /*
static inline void itv_swap(itv_t a, itv_t b)
{ itv_t t; *t=*a;*a=*b;*b=*t; }*/

  /* ********************************************************************** */
  /* Normalization and tests */
  /* ********************************************************************** */

  /* If integer is true, narrow the interval to integer bounds.
   In any case, return true if the interval is bottom
*/
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
  /*
static inline int itv_hash(itv_t a)
{
  return (5*bound_hash(a.inf) + 7*bound_hash(a.sup));
}*/

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
  /*
static inline void itv_add_num(itv_t a, itv_t b, num_t c)
{
  bound_add_num(a.sup,b.sup,c);
  bound_sub_num(a.inf,b.inf,c);
}
static inline void itv_sub_num(itv_t a, itv_t b, num_t c)
{
  bound_sub_num(a.sup,b.sup,c);
  bound_add_num(a.inf,b.inf,c);
}
static inline void itv_add_bound(itv_t a, itv_t b, bound_t c)
{
  bound_add(a.sup,b.sup,c);
  bound_sub(a.inf,b.inf,c);
}
static inline void itv_sub_bound(itv_t a, itv_t b, bound_t c)
{
  bound_sub(a.sup,b.sup,c);
  bound_add(a.inf,b.inf,c);
}
static inline bool itv_sqrt(itv_internal_t* intern, itv_t a, itv_t b)
{ return ITVFUN(itv_sqrt)(intern,a,b); }

static inline void itv_abs(itv_t a, itv_t b)
{ ITVFUN(itv_abs)(a,b); }

static inline void itv_mod(itv_internal_t* intern, itv_t a, itv_t b, itv_t c, bool is_int)
{ ITVFUN(itv_mod)(intern,a,b,c,is_int); }*/
  /*
def itv_ceil(b : itv_t) : itv_t =
{ new itv_t(sup = bound_ceil(b.sup), inf =  bound_floor(b.inf)) }

def itv_floor(b : itv_t) : itv_t =
{ new itv_t(sup = bound_floor(b.sup), inf = bound_ceil(b.inf)) }

static inline void itv_to_int(itv_t a, itv_t b)
{ bound_ceil(a.sup,b.sup); bound_ceil(a.inf,b.inf); }

static inline void itv_to_float(itv_t a, itv_t b)
{ bound_to_float(a.sup,b.sup); bound_to_float(a.inf,b.inf); }

static inline void itv_to_double(itv_t a, itv_t b)
{ bound_to_double(a.sup,b.sup); bound_to_double(a.inf,b.inf); }

static inline void itv_mul_2exp(itv_t a, itv_t b, int c)
{ bound_mul_2exp(a.sup,b.sup,c); bound_mul_2exp(a.inf,b.inf,c); }
*/

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

  /*
static inline void itv_range_rel(itv_internal_t* intern, bound_t a, itv_t b)
{
  bound_add(a,b.sup,b.inf);
  if (!bound_infty(a)) {
    itv_magnitude(intern.muldiv_bound,b);
    bound_div_2(intern.muldiv_bound,intern.muldiv_bound);
    bound_div(a,a,intern.muldiv_bound);
  }
}*/

  //TODO: curiose funzioni...
  /*
static inline bool itv_is_int(itv_internal_t* intern, itv_t a)
{
  bound_trunc(intern.muldiv_bound,a.sup);
  if (bound_cmp(intern.muldiv_bound,a.sup)) return false;
  bound_trunc(intern.muldiv_bound,a.inf);
  return !bound_cmp(intern.muldiv_bound,a.inf);
}

static inline void itv_unceil(itv_t a, itv_t b)
{
  /* [a,b] .  [ceil(a)-1,floor(b)] */
  bound_floor(a.sup,b.sup);
  bound_floor(a.inf,b.inf);
  bound_add_uint(a.inf,a.inf,1);
}

static inline void itv_unfloor(itv_t a, itv_t b)
{
  /* [a,b] . [ceil(a),floor(b)+1] */
  bound_floor(a.inf,b.inf);
  bound_floor(a.sup,b.sup);
  bound_add_uint(a.sup,a.sup,1);
}

static inline void itv_untrunc(itv_t a, itv_t b)
{
  /* trunc(x) = ceil(x) if x < 0, floor(x) if x > 0 */
  bound_floor(a.inf,b.inf);
  bound_floor(a.sup,b.sup);
  if (bound_sgn(a.inf) > 0) bound_add_uint(a.inf,a.inf,1);
  if (bound_sgn(a.sup) > 0) bound_add_uint(a.sup,a.sup,1);
}

static inline void itv_from_int(itv_t a, itv_t b)
{
  /* [a,b] . [floor(a),ceil(b)] */
  bound_ceil(a.sup,b.sup);
  bound_ceil(a.inf,b.inf);
}

static inline void itv_from_float(itv_t a, itv_t b)
{
  /* special case to ensure that signs are respected */
  if (bound_sgn(b.sup)==0) bound_set(a.sup,b.sup);
  else bound_next_float(a.sup,b.sup);
  if (bound_sgn(b.inf)==0) bound_set(a.inf,b.inf);
  else bound_next_float(a.inf,b.inf);
}

static inline void itv_from_double(itv_t a, itv_t b)
{
  if (bound_sgn(b.sup)==0) bound_set(a.sup,b.sup);
  else bound_next_double(a.sup,b.sup);
  if (bound_sgn(b.inf)==0) bound_set(a.inf,b.inf);
  else bound_next_double(a.inf,b.inf);
}
*/

  /* ********************************************************************** */
  /* itv.c: (unidimensional) intervals */
  /* ********************************************************************** */

  /*
static void make_float_const(int frac_bits, int exp_bits, int exp_bias,
			     float_const* cst)
{
  bound_t b,c;
  bound_init(b); bound_init(c);
  itv_init(cst.ulp); itv_init(cst.min); itv_init(cst.min_normal);
  itv_init(cst.max); itv_init(cst.max_exact);

  bound_set_int(b,1);
  bound_mul_2exp(b,b,-frac_bits);
  itv_set_unit_bound(cst.ulp,b);

  bound_set_int(b,1);
  bound_mul_2exp(b,b,1-exp_bias-frac_bits);
  itv_set_unit_bound(cst.min,b);

  bound_set_int(b,1);
  bound_mul_2exp(b,b,1-exp_bias);
  itv_set_unit_bound(cst.min_normal,b);

  bound_set_int(b,2);
  bound_set_int(c,1);
  bound_mul_2exp(c,c,-frac_bits);
  bound_sub(b,b,c);
  bound_mul_2exp(b,b,(1<<exp_bits)-2-exp_bias);
  itv_set_unit_bound(cst.max,b);

  bound_set_int(b,1);
  bound_mul_2exp(b,b,frac_bits);
  itv_set_unit_bound(cst.max_exact,b);

  bound_clear(b); bound_clear(c);
}

static void float_const_clear(float_const* cst)
{
  itv_clear(cst.ulp); itv_clear(cst.min); itv_clear(cst.min_normal); itv_clear(cst.max); itv_clear(cst.max_exact);
}
void ITVFUN(itv_internal_init)(itv_internal_t* intern)
{
  num_init(intern.canonicalize_num);
  bound_init(intern.muldiv_bound);
  bound_init(intern.mul_bound);
  bound_init(intern.sqrt_bound);
  bound_init(intern.linear_bound);
  bound_init(intern.linear_bound2);
  bound_init(intern.linear_bound3);
  itv_init(intern.mul_itv);
  itv_init(intern.mul_itv2);
  intern.ap_conversion_scalar = ap_scalar_alloc();
  bound_init(intern.ap_conversion_bound);
  itv_init(intern.eval_itv);
  itv_init(intern.eval_itv2);
  itv_init(intern.eval_itv3);
  num_init(intern.quasi_num);
  itv_init(intern.boxize_lincons_itv);
  itv_init(intern.boxize_lincons_eval);
  bound_init(intern.boxize_lincons_bound);
  mpz_init(intern.reduce_lincons_gcd);
  mpz_init(intern.reduce_lincons_mpz);

  make_float_const(10,5,15,&intern.cst_half);         /* 16-bit */
  make_float_const(23,8,127,&intern.cst_single);      /* 32-bit */
  make_float_const(52,11,1023,&intern.cst_double);    /* 64-bit */
  make_float_const(63,15,16383,&intern.cst_extended); /* 80-bit, no hidden bit */
  make_float_const(112,15,16383,&intern.cst_quad);    /* 128-bit */
  itv_init(intern.itv_half);
  itv_set_int2(intern.itv_half,-1,1);
  itv_mul_2exp(intern.itv_half,intern.itv_half,-1);
}
void ITVFUN(itv_internal_clear)(itv_internal_t* intern)
{
  num_clear(intern.canonicalize_num);
  bound_clear(intern.muldiv_bound);
  bound_clear(intern.mul_bound);
  bound_clear(intern.sqrt_bound);
  bound_clear(intern.linear_bound);
  bound_clear(intern.linear_bound2);
  bound_clear(intern.linear_bound3);
  itv_clear(intern.mul_itv);
  itv_clear(intern.mul_itv2);
  ap_scalar_free(intern.ap_conversion_scalar); intern.ap_conversion_scalar = NULL;
  bound_clear(intern.ap_conversion_bound);
  itv_clear(intern.eval_itv);
  itv_clear(intern.eval_itv2);
  itv_clear(intern.eval_itv3);
  num_clear(intern.quasi_num);
  itv_clear(intern.boxize_lincons_itv);
  itv_clear(intern.boxize_lincons_eval);
  bound_clear(intern.boxize_lincons_bound);
  mpz_clear(intern.reduce_lincons_gcd);
  mpz_clear(intern.reduce_lincons_mpz);
  float_const_clear(&intern.cst_half);
  float_const_clear(&intern.cst_single);
  float_const_clear(&intern.cst_double);
  float_const_clear(&intern.cst_extended);
  float_const_clear(&intern.cst_quad);
  itv_clear(intern.itv_half);
}

itv_internal_t* ITVFUN(itv_internal_alloc)(void)
{
  itv_internal_t* intern = malloc(sizeof(itv_internal_t));
  itv_internal_init(intern);
  return intern;
}
void ITVFUN(itv_internal_free)(itv_internal_t* intern)
{
  itv_internal_clear(intern);
  free(intern);
}*/

  /* ********************************************************************** */
  /* Arithmetic operations */
  /* ********************************************************************** */

  /* We assume no aliasing between

   - an itv and a num or a bound,
*/

  /*def itv_mul_num(itv_t a, itv_t b, num_t c)
{
  if (num_sgn(c)>=0){
    bound_mul_num(a.sup,b.sup,c);
    bound_mul_num(a.inf,b.inf,c);
  } else {
    num_neg(c,c);
    bound_mul_num(a.sup,b.sup,c);
    bound_mul_num(a.inf,b.inf,c);
    bound_swap(a.inf,a.sup);
    num_neg(c,c);
  }
}

void ITVFUN(itv_mul_bound)(itv_t a, itv_t b, bound_t c)
{
  assert (c!=a.inf && c!=a.sup && c!=b.inf && c!=b.sup);
  if (bound_sgn(c)>=0){
    bound_mul(a.sup,b.sup,c);
    bound_mul(a.inf,b.inf,c);
  } else {
    bound_neg(c,c);
    bound_mul(a.sup,b.sup,c);
    bound_mul(a.inf,b.inf,c);
    bound_swap(a.inf,a.sup);
    bound_neg(c,c);
  }
}

void ITVFUN(itv_div_num)(itv_t a, itv_t b, num_t c)
{
  if (num_sgn(c)>=0){
    bound_div_num(a.sup,b.sup,c);
    bound_div_num(a.inf,b.inf,c);
  } else {
    num_neg(c,c);
    bound_div_num(a.sup,b.sup,c);
    bound_div_num(a.inf,b.inf,c);
    bound_swap(a.inf,a.sup);
    num_neg(c,c);
  }
}
void ITVFUN(itv_div_bound)(itv_t a, itv_t b, bound_t c)
{
  assert (c!=a.inf && c!=a.sup && c!=b.inf && c!=b.sup);
  if (bound_sgn(c)>=0){
    bound_div(a.sup,b.sup,c);
    bound_div(a.inf,b.inf,c);
  } else {
    bound_neg(c,c);
    bound_div(a.sup,b.sup,c);
    bound_div(a.inf,b.inf,c);
    bound_swap(a.inf,a.sup);
    bound_neg(c,c);
  }
}*/
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
  def itv_neg(b: itv_t): itv_t =
  {
    if (itv_is_bottom(b)) itv_t.bottom
    //if (a!=b){
    else new itv_t(inf = bound_set(b.sup), sup = bound_set(b.inf))
    /*} else {
  bound_swap(a.inf,a.sup);
}*/
  }
  /*
bool ITVFUN(itv_sqrt)(itv_internal_t* intern, itv_t a, itv_t b)
{
  bool exact = true;
  if (itv_is_bottom(intern,b) || bound_sgn(b.sup)<0) {
    /* empty result */
    itv_set_bottom(a);
    return true;
  }
  /* lower bound */
  if (bound_sgn(b.inf)>=0) {
    bound_set_int(a.inf,0);
  }
  else {
    bound_neg(b.inf,b.inf);
    bound_sqrt(intern.sqrt_bound,a.inf,b.inf);
    exact = exact && bound_equal(intern.sqrt_bound,a.inf);
    bound_neg(b.inf,b.inf);
    if (a!=b) bound_neg(a.inf,a.inf);
  }
  /* upper bound */
  bound_sqrt(a.sup,intern.sqrt_bound,b.sup);
  exact = exact && bound_equal(intern.sqrt_bound,a.sup);
  return exact;
}*/

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

  /*
/* ********************************************************************** */
/* Power */
/* ********************************************************************** */

void ITVFUN(itv_pow)(itv_internal_t* intern, itv_t a, itv_t b, itv_t n)
{
  long x;
  if (itv_is_bottom(intern, b) || itv_is_bottom(intern, n)) {
    itv_set_bottom(a);
    return;
  }
  /* ensures that the exponent is a singleton */
  bound_neg(intern.mul_bound, n.inf);
  if (bound_infty(n.sup) || bound_cmp(intern.mul_bound, n.sup)) {
    itv_set_top(a);
    return;
  }
  /* ensures that the exponent is a positive integer, stores it in x */
  int_set_num(&x, bound_numref(n.sup));
  bound_set_int(intern.mul_bound, x);
  if (bound_cmp(intern.mul_bound, n.sup) || x < 0) {
    itv_set_top(a);
    return;
  }
  if (x & 1) itv_set(intern.mul_itv, b);
  else itv_abs(intern.mul_itv, b);
  bound_neg(intern.mul_itv.inf, intern.mul_itv.inf);
  bound_pow(a.sup, intern.mul_bound, intern.mul_itv.sup, x);
  bound_pow(intern.mul_bound, a.inf, intern.mul_itv.inf, x);
  bound_neg(a.inf, a.inf);
}

/* inverse of pow, uses the sign of orga to determine the sign of a */
void ITVFUN(itv_inv_pow)(itv_internal_t* intern, itv_t a, itv_t orga, itv_t b, itv_t n)
{
  long x;
  assert(orga != a);
  if (itv_is_bottom(intern, b) || itv_is_bottom(intern, orga) || itv_is_bottom(intern, n)) {
    itv_set_bottom(a);
    return;
  }
  /* ensures that the exponent is a singleton */
  bound_neg(intern.mul_bound, n.inf);
  if (bound_infty(n.sup) || bound_cmp(intern.mul_bound, n.sup)) {
    itv_set_top(a);
    return;
  }
  /* ensures that the exponent is a positive integer, stores it in x */
  int_set_num(&x, bound_numref(n.sup));
  bound_set_int(intern.mul_bound, x);
  if (bound_cmp(intern.mul_bound, n.sup) || x < 0) {
    itv_set_top(a);
    return;
  }
  if (!x) {
    /* special case: 0-th root is undefined */
    itv_set_top(a);
    return;
  }
  if ((x & 1) || (bound_sgn(b.inf) <= 0)) {
    /* keep bound sign */
    bound_set(intern.mul_itv.sup, b.sup);
    bound_neg(intern.mul_itv.inf, b.inf);
  }
  else {
    /* intersect with [0;+oo] */
    if (bound_sgn(b.sup) < 0) {
      itv_set_bottom(a);
      return;
    }
    bound_set(intern.mul_itv.sup, b.sup);
    bound_set_int(intern.mul_itv.inf, 0);
  }
  bound_root(a.sup, intern.mul_bound, intern.mul_itv.sup, x);
  bound_root(intern.mul_bound, a.inf, intern.mul_itv.inf, x);
  bound_neg(a.inf, a.inf);
  if (!(x & 1)) {
    /* fix the sign of a depending on the sign of orga */
    if ((bound_sgn(orga.sup) <= 0)) {
      /* orga is negative . the result is negative */
      bound_swap(a.inf,a.sup);
    }
    else if (bound_sgn(orga.inf) >= 0) {
      /* orga has unknown sign . result has unknown sign */
      bound_set(a.inf, a.sup);
    }
  }
}*/

  /* ********************************************************************** */
  /* Printing */
  /* ********************************************************************** */

  def itv_print(a: itv_t): Unit =
  {
    print(itv_sprint(a))
    /*print("[")
    if (bound_infty(a.inf))
      print("-oo")
    else {
      print(-a.inf.bound_numref)
    }
    print(",")
    bound_print(a.sup)
    print("]");*/
  }

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

  /* ********************************************************************** */
  /* Conversions */
  /* ********************************************************************** */
  /*
bool ITVFUN(itv_set_ap_scalar)(itv_internal_t* intern,
			   itv_t a, ap_scalar_t* b)
{
  assert (ap_scalar_infty(b)==0);
  bool exact = bound_set_ap_scalar(a.sup,b);
  if (exact){
    bound_neg(a.inf,a.sup);
    return true;
  }
  else {
    ap_scalar_neg(intern.ap_conversion_scalar, b);
    bound_set_ap_scalar(a.inf,intern.ap_conversion_scalar);
    return false;
  }
}
bool ITVFUN(itv_set_ap_interval)(itv_internal_t* intern,
			     itv_t a, ap_interval_t* b)
{
  ap_scalar_neg(intern.ap_conversion_scalar, b.inf);
  bool b1 = bound_set_ap_scalar(a.inf,intern.ap_conversion_scalar);
  bool b2 = bound_set_ap_scalar(a.sup,b.sup);
  return b1 && b2;
}
bool ITVFUN(itv_set_ap_coeff)(itv_internal_t* intern,
			  itv_t itv, ap_coeff_t* coeff)
{
  switch(coeff.discr){
  case AP_COEFF_SCALAR:
    return itv_set_ap_scalar(intern,itv,coeff.val.scalar);
    break;
  case AP_COEFF_INTERVAL:
    return itv_set_ap_interval(intern, itv, coeff.val.interval);
    break;
  default:
    abort();
  }
}

bool ITVFUN(ap_interval_set_itv)(itv_internal_t* intern,
				   ap_interval_t* a, itv_t b)
{
  (void)intern;
  bool b1 = ap_scalar_set_bound(a.inf,b.inf);
  ap_scalar_neg(a.inf,a.inf);
  bool b2 = ap_scalar_set_bound(a.sup,b.sup);
  return b1 && b2;
}
bool ITVFUN(ap_coeff_set_itv)(itv_internal_t* intern,
				ap_coeff_t* a, itv_t b)
{
  bool exact;

  if (itv_is_point(intern,b)){
    exact = ap_scalar_set_bound(intern.ap_conversion_scalar, b.sup);
    if (exact){
      ap_coeff_set_scalar(a, intern.ap_conversion_scalar);
      return true;
    }
    else {
      ap_coeff_reinit(a,AP_COEFF_INTERVAL,
		      intern.ap_conversion_scalar.discr);
      ap_scalar_set(a.val.interval.sup, intern.ap_conversion_scalar);
      bound_neg(intern.ap_conversion_bound, b.inf); /* we now it is finite */
      ap_scalar_set_bound(a.val.interval.inf, intern.ap_conversion_bound);
      return false;
    }
  }
  else {
    ap_coeff_reinit(a,AP_COEFF_INTERVAL,NUM_AP_SCALAR);
    return ap_interval_set_itv(intern,a.val.interval,b);
  }
}
bool ITVFUN(itv_array_set_ap_interval_array)(itv_internal_t* intern,
					 itv_t** ptitv,
					 ap_interval_t** array,
					 size_t size)
{
  bool exact, res;
  itv_t* titv;
  size_t i;

  titv = itv_array_alloc(size);
  res = true;
  for (i=0; i<size; i++){
    exact = itv_set_ap_interval(intern,titv[i],array[i]);
    res = res && exact;
  }
  *ptitv = titv;
  return res;
}*/

}
