package it.unive.dais.yaasa.lib_intervals

/**
  * Created by esteffin on 05/01/16.
  */
import itv._
import bound._

/**
  * @author esteffin
  */
object Main {

  //#include "ap_manager.h"
  //#include "num.h"
  //#include "bound.h"
  //#include "itv.h"

  def arith(b: itv_t, c: itv_t): Unit =
  {
    //itv_t bb,cc;

    printf("********************\n");
    printf("b="); itv_print(b);
    printf(" c="); itv_print(c); printf("\n")
    //printf(" bound="); bound_print(bound); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_swap(b,c);
    //printf("itv_swap(b,c): "); printf("b="); itv_print(b); printf(" c="); itv_print(c); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_neg(a,b);
    printf(s"itv_neg($b)="); itv_print(itv_neg(b)); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_abs(a,b);
    printf(s"itv_abs($b)="); itv_print(itv_abs(b)); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_add(a,b,c);
    printf(s"$b + $c ="); itv_print(itv_add(b, c)); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_sub(a,b,c);
    printf(s"$b - $c ="); itv_print(itv_sub(b, c)); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_mul(intern,a,b,c);
    printf(s"$b * $c ="); itv_print(itv_mul(b, c)); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_div(intern,a,b,c);
    printf(s"$b / $c ="); itv_print(itv_div(b, c)); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_mod(intern,a,b,c);
    printf(s"$b mod $c ="); itv_print(itv_mod(b, c)); printf("\n");

    printf(s"$b JOIN $c ="); itv_print(itv_join(b, c)); printf("\n");

    printf(s"$b MEET $c ="); itv_print(itv_meet(b, c)._2); printf("\n");

    printf(s"$b WIDENING $c ="); itv_print(itv_widening(b, c)); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_add_bound(a,b,bound);
    //printf("itv_add_bound(b,bound)="); itv_print(a); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_sub_bound(a,b,bound);
    //printf("itv_sub_bound(b,bound)="); itv_print(a); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_mul_bound(a,b,bound);
    //printf("itv_mul_bound(b,bound)="); itv_print(a); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);
    //itv_div_bound(a,b,bound);
    //printf("itv_div_bound(b,bound)="); itv_print(a); printf("\n");

    //itv_set(b,bb); itv_set(c,cc);

    //itv_clear(bb);
    //itv_clear(cc);
  }

  def itv_main(): Unit =
  {
    var b = itv_t.bottom
    var c = itv_t.bottom

    /* Positive or negative intervals */
    b = itv_t.interval(inf = 3, sup = 5) //bound_set_int(b->inf,-3); bound_set_int(b->sup,5);
    c = itv_t.interval(inf = 1, sup = 5) //bound_set_int(c->inf,-1); bound_set_int(c->sup,5);
    //bound_set_int(bound,4);
    arith(b, c);
    c = itv_neg(c);
    arith(b, c);
    b = itv_neg(b);
    arith(b, c);
    c = itv_neg(c);
    arith(b, c);
    /* general intervals */
    b = itv_t.interval(inf = -3, sup = 5) //bound_set_int(b->inf,3); bound_set_int(b->sup,5);
    c = itv_t.interval(inf = -7, sup = 11) //bound_set_int(c->inf,7); bound_set_int(c->sup,11);
    //bound_set_int(bound,3);
    arith(b, c);
    //bound_set_int(bound,-3);
    //arith(intern,a,b,c,bound);

    /* aliases */
    b = itv_t.interval(inf = -3, sup = 5) //bound_set_int(b->inf,3); bound_set_int(b->sup,5);
    c = itv_t.interval(inf = -7, sup = 11) //bound_set_int(c->inf,7); bound_set_int(c->sup,11);
    //bound_set_int(bound,3);
    arith(b, b);
    //bound_set_int(bound,-3);
    //arith(intern,b,b,b,bound);

    /* opened */
    b = itv_t.open_left(sup = 5) //bound_set_int(b->inf,3); bound_set_int(b->sup,5);
    c = itv_t.interval(inf = 7, sup = 11) //bound_set_int(c->inf,7); bound_set_int(c->sup,11);
    arith(b, c);

    b = itv_t.open_left(sup = 5) //bound_set_int(b->inf,3); bound_set_int(b->sup,5);
    c = itv_t.open_left(sup = 11) //bound_set_int(c->inf,7); bound_set_int(c->sup,11);
    arith(b, c);

    b = itv_t.open_right(inf = 5) //bound_set_int(b->inf,3); bound_set_int(b->sup,5);
    c = itv_t.open_left(sup = -11) //bound_set_int(c->inf,7); bound_set_int(c->sup,11);
    arith(b, c);

    b = itv_t.point(0) //bound_set_int(b->inf,3); bound_set_int(b->sup,5);
    c = itv_t.bottom //bound_set_int(c->inf,7); bound_set_int(c->sup,11);
    arith(b, c);

    b = itv_t.point(0) //bound_set_int(b->inf,3); bound_set_int(b->sup,5);
    c = itv_t.point(0) //bound_set_int(c->inf,7); bound_set_int(c->sup,11);
    arith(b, c);

  }
  def main(args: Array[String]) {

    //itv.itv_print(itv_set_top)

    val a = itv_t.interval(3, 6)
    val b = itv_t.interval(5, 9)

    // a - (floor(a / b) * b)
    /*
    println("b: " + a)
    println("c: " + b)
    val arg = itv_div(a, itv_abs(b)) //b/|c|
    println("b/c: " + arg)
    println("trunc(b/c): " + itv_trunc(arg))
    val dif = itv_mul(itv_trunc(arg), b) //b/|c|
    println("(floor(b / c) * c): " + dif)
    println("mod(b,c): " + itv_sub(a, dif))
*/
    /*println("b: " + a)
    println("c: " + b)
    val fst = itv_sub(a, itv_abs(b)) // b-|c|
    println("b-|c|: " + fst)
    val arg = itv_div(a, itv_abs(b)) //b/|c|
    println("b/|c|: " + arg)
    println("trunc(b/|c|): " + itv_trunc(arg))
    println("mod(b,c): " + itv_mul(fst, itv_trunc(arg)))*/

    //itv_main()
    println(itv_leqat(a, b))

    //println("or" + itv_mod(a, b))

    //println("\nHI there!")
  }
}

