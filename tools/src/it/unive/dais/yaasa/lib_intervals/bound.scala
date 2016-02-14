package it.unive.dais.yaasa.lib_intervals

import it.unive.dais.yaasa.utils.prelude.pretty

/**
  * @author esteffin
  */
object bound {

  def sign(x: Int) = if (x > 0) 1 else if (x < 0) -1 else 0

  /* ********************************************************************** */
  /* bound_def.h: numbers used for bounds */
  /* ********************************************************************** */

  class bound_t(val num: Int, val inf: Boolean) extends pretty {
    //val num : Int /* always allocated, even if inf=1 */
    //val inf : Boolean;  /* 1 => +/-oo; the sign of num decides the sign of the oo 0 => >-oo, <+oo */
    def bound_numref = num
    override def clone() = new bound_t(num = this.num, inf = this.inf)

    override def pretty: String = bound_sprint(this)
  }

  object bound_t {
    //def _bound_inf(a : bound_t) : bound_t = new bound_t(a.bound_numref, false)
    def zero: bound_t = new bound_t(0, false)
    def num(n: Int): bound_t = new bound_t(n, false)
  }

  /* ---------------------------------------------------------------------- */
  def bound_infty(a: bound_t): Boolean =
  { a.inf }

  /* ---------------------------------------------------------------------- */
  def bound_set_infty(sgn: Int): bound_t = {
    if (sgn == 0) throw new RuntimeException("Sign must be different than 0")
    new bound_t(if (sgn > 0) 1 else -1, true)
  }

  /* ---------------------------------------------------------------------- */
  def bound_init_set_infty(a: bound_t, sgn: Int): bound_t = {
    bound_set_infty(sgn)
  }
  /*
def bound_swap(a : bound_t , b : bound_t ) : bound_t
{
  num_swap(bound_numref(a),bound_numref(b));
}*/

  def bound_sgn(a: bound_t): Int = {
    sign(a.num)
  }

  /* ====================================================================== */
  /* Assignement */
  /* ====================================================================== */

  def bound_set(b: bound_t): bound_t = new bound_t(b.num, b.inf)
  /*
static inline void bound_set_array(bound_t* a, bound_t* b, size_t size)
{
  size_t i;
  for (i=0; i<size; i++) bound_set(a[i],b[i]);
}
*/

  def bound_set_int(i: Int): bound_t = new bound_t(i, false)

  def bound_set_num(i: Int): bound_t = new bound_t(i, false)

  /* ====================================================================== */
  /* Constructors and Destructors */
  /* ====================================================================== */

  def bound_init: bound_t = bound_t.zero

  /*static inline void bound_init_set_int(bound_t a, long int i)
{ num_init_set_int(bound_numref(a),i); _bound_inf(a); }
static inline void bound_clear(bound_t a)
{ num_clear(bound_numref(a)); }*/

  /*
static inline void bound_init_array(bound_t* a, size_t size)
{
  size_t i;
  for (i=0;i<size;i++) bound_init(a[i]);
}
static inline void bound_init_set(bound_t a, bound_t b)
{
  if (bound_infty(b)){
    bound_init_set_infty(a,bound_sgn(b));
  } else {
    num_init_set(bound_numref(a),bound_numref(b));
    _bound_inf(a);
  }
}
static inline void bound_clear_array(bound_t* a, size_t size)
{
#if !defined(NUM_NATIVE)
  size_t i;
  for (i=0;i<size;i++) bound_clear(a[i]);
#endif
}
*/

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
  /*static inline void bound_add_uint(bound_t a, bound_t b, unsigned long int c)
{
  if (bound_infty(b)) bound_set_infty(a,bound_sgn(b));
  else { num_add_uint(bound_numref(a),bound_numref(b),c); _bound_inf(a); }
}
static inline void bound_add_num(bound_t a, bound_t b, num_t c)
{
  if (bound_infty(b)) bound_set_infty(a,bound_sgn(b));
  else { num_add(bound_numref(a),bound_numref(b),c); _bound_inf(a); }
}*/
  def bound_sub(b: bound_t, c: bound_t): bound_t = {
    if (bound_infty(b)) bound_set_infty(bound_sgn(b))
    else if (bound_infty(c)) bound_set_infty(-bound_sgn(c))
    else new bound_t(b.num - c.num, false)
  }

  def bound_sub_uint(b: bound_t, c: Int): bound_t = {
    if (bound_infty(b)) new bound_t(inf = b.inf, num = b.num)
    else { new bound_t(inf = false, num = b.num - c) }
  }
  /*
static inline void bound_sub_num(bound_t a, bound_t b, num_t c)
{
  if (bound_infty(b)) bound_set_infty(a,bound_sgn(b));
  else { num_sub(bound_numref(a),bound_numref(b),c); _bound_inf(a); }
}*/

  def bound_mul(b: bound_t, c: bound_t): bound_t = {
    if (bound_sgn(b) == 0 || bound_sgn(c) == 0) bound_set_int(0)
    else if (bound_infty(b) || bound_infty(c)) bound_set_infty(bound_sgn(b) * bound_sgn(c))
    else new bound_t(b.num * c.num, false)
  }
  /*
static inline void bound_mul_num(bound_t a, bound_t b, num_t c)
{
  if (!bound_sgn(b) || !num_sgn(c)) bound_set_int(a,0);
  else if (bound_infty(b)) bound_set_infty(a,bound_sgn(b)*num_sgn(c));
  else { num_mul(bound_numref(a),bound_numref(b),c); _bound_inf(a); }
}
static inline void bound_mul_2(bound_t a, bound_t b)
{
  if (bound_infty(b)) bound_set_infty(a,bound_sgn(b));
  else { num_mul_2(bound_numref(a),bound_numref(b)); _bound_inf(a); }
}*/
  def bound_div(b: bound_t, c: bound_t): bound_t = {
    if (bound_sgn(b) == 0 || bound_infty(c)) bound_set_int(0)
    else if (bound_sgn(c) == 0) bound_set_infty(bound_sgn(b))
    else if (bound_infty(b)) bound_set_infty(bound_sgn(b) * bound_sgn(c))
    else new bound_t(b.num / c.num, false)
  }
  /*
static inline void bound_div_num(bound_t a, bound_t b, num_t c)
{
  if (!bound_sgn(b)) bound_set_int(a,0);
  else if (!num_sgn(c)) bound_set_infty(a,bound_sgn(b));
  else if (bound_infty(b))  bound_set_infty(a,bound_sgn(b)*num_sgn(c));
  else { num_div(bound_numref(a),bound_numref(b),c); _bound_inf(a); }
}
static inline void bound_div_2(bound_t a, bound_t b)
{
  if (bound_infty(b)) bound_set_infty(a,bound_sgn(b));
  else { num_div_2(bound_numref(a),bound_numref(b)); _bound_inf(a); }
}
*/

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
    //{ num_trunc(bound_numref(a), bound_numref(b)); _bound_inf(a); }
  }

  /*
static inline void bound_floor(bound_t a, bound_t b)
{
  if (bound_infty(b)) bound_set_infty(a,bound_sgn(b));
  else { num_floor(bound_numref(a),bound_numref(b)); _bound_inf(a); }
}
static inline void bound_ceil(bound_t a, bound_t b)
{
  if (bound_infty(b)) bound_set_infty(a,bound_sgn(b));
  else { num_ceil(bound_numref(a),bound_numref(b)); _bound_inf(a); }
}
static inline void bound_sqrt(bound_t up, bound_t down, bound_t b)
{
  if (bound_infty(b)) {
    bound_set_infty(up,1);
    bound_set_infty(down,1);
  }
  else {
    num_sqrt(bound_numref(up),bound_numref(down),bound_numref(b));
    _bound_inf(up);
    _bound_inf(down);
  }
}
static inline void bound_pow(bound_t up, bound_t down, bound_t b, unsigned long n)
{
  if (bound_infty(b)) {
    bound_set_infty(up, 1);
    bound_set_infty(down, 1);
  }
  else if (num_pow(bound_numref(up), bound_numref(down), bound_numref(b), n)) {
    bound_set_infty(up,1);
    if (n & 1) bound_set_infty(down,-1);
    else bound_set_int(down, 0);
  }
  else {
    _bound_inf(up);
    _bound_inf(down);
  }
}
static inline void bound_root(bound_t up, bound_t down, bound_t b, unsigned long n)
{
  if (bound_infty(b)) {
    bound_set_infty(up, 1);
    bound_set_infty(down, 1);
  }
  else {
    num_root(bound_numref(up), bound_numref(down), bound_numref(b), n);
    _bound_inf(up);
    _bound_inf(down);
  }
}
static inline void bound_to_float(bound_t a, bound_t b)
{
  if (bound_infty(b) || !num_fits_float(bound_numref(b)))
    bound_set_infty(a,bound_sgn(b));
  else {
    double d;
    double_set_num(&d,bound_numref(b));
    num_set_double(bound_numref(a),(double)((float)d));
    _bound_inf(a);
  }
}
static inline void bound_to_double(bound_t a, bound_t b)
{
  if (bound_infty(b) || !num_fits_double(bound_numref(b)))
    bound_set_infty(a,bound_sgn(b));
  else {
    double d;
    double_set_num(&d,bound_numref(b));
    num_set_double(bound_numref(a),d);
    _bound_inf(a);
  }
}

static inline void bound_next_float(bound_t a, bound_t b)
{
  if (bound_infty(b) || !num_fits_float(bound_numref(b))) {
    /* +oo overapproximates nextfloat(-oo) and nextfloat(+oo) */
    bound_set_infty(a,1);
  }
  else {
    double d;
    double_set_num(&d,bound_numref(b));
    d = nextafterf(d,d+1);
    if (d == 1./0.) {
      bound_set_infty(a,1);
    }
    else {
      num_set_double(bound_numref(a),d);
      _bound_inf(a);
    }
  }
}

static inline void bound_next_double(bound_t a, bound_t b)
{
  if (bound_infty(b) || !num_fits_double(bound_numref(b))) {
    bound_set_infty(a,1);
  }
  else {
    double d;
    double_set_num(&d,bound_numref(b));
    d = nextafter(d,d+1);
    if (d == 1./0.) {
      bound_set_infty(a,1);
    }
    else {
      num_set_double(bound_numref(a),d);
      _bound_inf(a);
    }
  }
}

static inline void bound_mul_2exp(bound_t a, bound_t b, int c)
{
  if (bound_infty(b)) bound_set_infty(a,bound_sgn(b));
  else { num_mul_2exp(bound_numref(a),bound_numref(b),c); }
}
*/

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
  /*
static inline int bound_cmp_int(bound_t a, long int b)
{
  if (bound_infty(a)) return bound_sgn(a);
  else return num_cmp_int(bound_numref(a),b);
}
static inline int bound_cmp_num(bound_t a, num_t b)
{
  if (bound_infty(a)) return bound_sgn(a);
  else return num_cmp(bound_numref(a),b);
}*/
  def bound_equal(a: bound_t, b: bound_t): Boolean = {
    if (bound_infty(a)) {
      bound_infty(b) && bound_sgn(a) == bound_sgn(b)
    }
    else {
      if (bound_infty(b)) false
      else a.num == b.num
    }
  }

  /*

static inline int bound_hash(bound_t a)
{ if (bound_infty(a))
    return bound_sgn(a)>0 ? INT_MAX : INT_MIN;
  else {
    long int hash;
    int_set_num(&hash,bound_numref(a));
    return hash;
  }
}*/

  /* ====================================================================== */
  /* Printing */
  /* ====================================================================== */
  /*
static inline void bound_fprint(FILE* stream, bound_t a)
{
  if (bound_infty(a)) fprintf(stream,"%coo",bound_sgn(a)>0 ? '+' : '-');
  else num_fprint(stream,bound_numref(a));
}*/
  def bound_print(a: bound_t): Unit = {
    if (bound_infty(a)) print("%coo" format (if (bound_sgn(a) > 0) '+' else '-'))
    else print("%d" format a.num)
  }

  def bound_sprint(a: bound_t): String =
  {
    if (bound_infty(a)) "%coo".format(if (bound_sgn(a) > 0) '+' else '-')
    else "%d" format a.num
  }

  /* ====================================================================== */
  /* Conversions */
  /* ====================================================================== */

  /*
/* Convert an ap_scalar_t into a bound_t */
static inline
bool bound_set_ap_scalar(bound_t a, ap_scalar_t* b)
{
  switch (b->discr){
  case AP_SCALAR_MPQ:
    if (mpz_sgn(mpq_denref(b->val.mpq))==0){
      bound_set_infty(a,mpz_sgn(mpq_numref(b->val.mpq)));
      return true;
    }
    else {
      _bound_inf(a);
      return num_set_mpq(bound_numref(a),b->val.mpq);
    }
    break;
  case AP_SCALAR_DOUBLE:
    if (isinf(b->val.dbl)) {
      if (b->val.dbl>0) bound_set_infty(a,1);
      else bound_set_infty(a,-1);
      return true;
    }
    else {
      _bound_inf(a);
      return num_set_double(bound_numref(a),b->val.dbl);
    }
    break;
  case AP_SCALAR_MPFR:
    if (mpfr_inf_p(b->val.mpfr)) {
      if (mpfr_sgn(b->val.mpfr)>0) bound_set_infty(a,1);
      else bound_set_infty(a,-1);
      return true;
    }
    else {
      _bound_inf(a);
      return num_set_mpfr(bound_numref(a),b->val.mpfr);
    }
    break;
  default:
    abort();
    return false;
  }
}
/* Convert a bound_t into an ap_scalar_t */
static inline bool ap_scalar_set_bound(ap_scalar_t* a, bound_t b)
{
  ap_scalar_reinit(a,NUM_AP_SCALAR);
  if (bound_infty(b)) { ap_scalar_set_infty(a,bound_sgn(b)); return true; }
  else {
    switch (NUM_AP_SCALAR) {
    case AP_SCALAR_DOUBLE: return double_set_num(&a->val.dbl,bound_numref(b));
    case AP_SCALAR_MPQ:    return mpq_set_num(a->val.mpq,bound_numref(b));
    case AP_SCALAR_MPFR:   return mpfr_set_num(a->val.mpfr,bound_numref(b));
    default:               abort(); return false;
    }
  }
}

/* ====================================================================== */
/* Serialization */
/* ====================================================================== */

static inline size_t bound_serialize(void* dst, bound_t src)
{
#if defined(BOUND_NUM)
  return num_serialize(dst,src);
#else
  *(char*)dst = src->inf;
  return num_serialize((char*)dst+1,bound_numref(src))+1;
#endif
}

static inline size_t bound_deserialize(bound_t dst, const void* src)
{
#if defined(BOUND_NUM)
  return num_deserialize(dst,src);
#else
  dst->inf = *(const char*)src;
  return num_deserialize(bound_numref(dst),(const char*)src+1)+1;
#endif
}

static inline size_t bound_serialized_size(bound_t a)
{
#if defined(BOUND_NUM)
  return num_serialized_size(a);
#else
  return num_serialized_size(bound_numref(a))+1;
#endif
}

static inline size_t bound_serialize_array(void* dst, bound_t* src, size_t size)
{
  size_t i,n=0;
  for (i=0;i<size;i++)
    n += bound_serialize((char*)dst+n,src[i]);
  return n;
}

static inline size_t bound_deserialize_array(bound_t* dst, const void* src, size_t size)
{
  size_t i,n=0;
  for (i=0;i<size;i++)
    n += bound_deserialize(dst[i],(const char*)src+n);
  return n;
}

static inline size_t bound_serialized_size_array(bound_t* src, size_t size)
{
  size_t i,n=0;
  for (i=0;i<size;i++)
    n += bound_serialized_size(src[i]);
  return n;
}

static inline bool bound_integer(bound_t a)
{ return !bound_infty(a) && num_integer(bound_numref(a)); }

#ifdef __cplusplus
}
#endif

#endif
* */

}
