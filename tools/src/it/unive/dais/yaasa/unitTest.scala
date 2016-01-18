package it.unive.dais.yaasa

//import it.unive.dais.yaasa.datatype.ABSValue._
import it.unive.dais.yaasa.abstract_types._

/**
  * @author gbarbon
  */
object unitTest {

  def unitMain = {
    println("\n*** Starting abstract boolean test ***")
    booleanUnitTest
    println("\n\n*** Starting abstract numerical test ***")
    numUnitTest
    println("\n\n*** Starting abstract string test ***")
    stringUnitTest
  }

  def booleanUnitTest = {
    val trueBool: AbstractBool = AbstractBoolFactory.fromBool(true)
    val falseBool: AbstractBool = AbstractBoolFactory.fromBool(false)
    println("(checking also toString) trueBool: " + trueBool.toString() + ", falseBool: " + falseBool.toString())

    println("\n*** BOOLEAN TEST: Checking generation **")
    val shouldBeFalse: AbstractBool = AbstractBoolFactory.sFalseAt
    val shouldBeTrue: AbstractBool = AbstractBoolFactory.sTrueAt
    println("Is sTrueAt true? " + trueBool.==^(shouldBeTrue))
    println("Is sFalseAt false? (a true abs val is ok) " + falseBool.==^(shouldBeFalse))

    println("\n*** BOOLEAN TEST: Checking basic op. with boolean abstract values (from concrete) ***")
    println("== (equality test) between trueBool and falseBool is: " +  trueBool.==^(falseBool))
    println("== (equality test) between trueBool and trueBool is: " + trueBool.==^(trueBool))
    println("== (equality test) between falseBool and falseBool is: " + falseBool.==^(falseBool))
    println("|| (OR) between trueBool and falseBool is: " + trueBool.||^(falseBool))
    println("|| (OR) between trueBool and trueBool is: " + trueBool.||^(trueBool))
    println("|| (OR) between falseBool and falseBool is: " + falseBool.||^(falseBool))
    println("!= (inequality) between trueBool and falseBool is: " + trueBool.!=^(falseBool))
    println("!= (inequality) between trueBool and trueBool is: " + trueBool.!=^(trueBool))
    println("!= (inequality) between falseBool and falseBool is: " + falseBool.!=^(falseBool))
    println("&& (AND) between trueBool and falseBool is: " + trueBool.&&^(falseBool))
    println("&& (AND) between trueBool and trueBool is: " + trueBool.&&^(trueBool))
    println("&& (AND) between falseBool and falseBool is: " + falseBool.&&^(falseBool))
    println("not (negation) applied to trueBool is: " + trueBool.notAt)
    println("not (negation) applied to falseBool is: " + falseBool.notAt)

    println("\n*** BOOLEAN TEST: checking contains methods ***")
    println("containsTrue applied to trueBool: " + trueBool.containsTrue)
    println("containsFalse applied to trueBool: " + trueBool.containsFalse)

    println("\n*** BOOLEAN TEST: conversion to string ***")
    val stringBool: AbstractString = shouldBeTrue.toStringAt
    println("The boolean abs value " + trueBool + " has been converted to the string abs value: " + stringBool)

    println("\n*** BOOLEAN TEST: top and bottom generation ***")
    val topBool: AbstractBool = AbstractBoolFactory.top
    val bottomBool: AbstractBool = AbstractBoolFactory.bottom
    println("topBool: " + topBool.toString() + ", bottomBool: " + bottomBool.toString())

    println("\n*** BOOLEAN TEST: operations with top and bottom ***")
    println("== (equality test) between topBool and bottomBool is: " +  topBool.==^(bottomBool))
    println("== (equality test) between topBool and trueBool is: " + topBool.==^(topBool))
    println("== (equality test) between bottomBool and bottomBool is: " + bottomBool.==^(bottomBool))
    println("|| (OR) between topBool and bottomBool is: " + topBool.||^(bottomBool))
    println("|| (OR) between topBool and topBool is: " + topBool.||^(topBool))
    println("|| (OR) between bottomBool and bottomBool is: " + bottomBool.||^(bottomBool))
    println("!= (inequality) between topBool and falseBool is: " + topBool.!=^(bottomBool))
    println("!= (inequality) between topBool and topBool is: " + topBool.!=^(topBool))
    println("!= (inequality) between bottomBool and bottomBool is: " + bottomBool.!=^(bottomBool))
    println("&& (AND) between topBool and falseBool is: " + topBool.&&^(bottomBool))
    println("&& (AND) between topBool and topBool is: " + topBool.&&^(topBool))
    println("&& (AND) between bottomBool and bottomBool is: " + bottomBool.&&^(bottomBool))
    println("not (negation) applied to topBool is: " + topBool.notAt)
    println("not (negation) applied to bottomBool is: " + bottomBool.notAt)

  }

  def numUnitTest: Unit = {


    val x: AbstractNum = AbstractNumFactory.fromNum(5)
    val y: AbstractNum = AbstractNumFactory.open_left(12)

    val z: AbstractNum = x +^ y

    println(AbstractStringFactory.top == z.toStringAt)
  }
  def stringUnitTest {}
  /*
  def unitMain = {
    aBool.!=^(aBool)
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
*/
}

