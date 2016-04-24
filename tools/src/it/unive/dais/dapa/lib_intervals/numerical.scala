package it.unive.dais.dapa.lib_intervals

/**
  * @author esteffin
  * @author gbarbon
  */

import it.unive.dais.dapa.utils.prelude.pretty

object numerical {

  def sign(x: Int) = if (x > 0) 1 else if (x < 0) -1 else 0

  class numericalty(val num: Int, val inf: Boolean) extends pretty {
    def numlRef = num
    override def clone() = new numericalty(num = this.num, inf = this.inf)
    override def pretty: String = numlSprint(this)
  }

  object numericalty {
    def zero: numericalty = new numericalty(0, false)
    def num(n: Int): numericalty = new numericalty(n, false)
  }

  def numlInf(a: numericalty): Boolean = { a.inf }

  def numlSetinf(sgn: Int): numericalty = {
    if (sgn == 0) throw new RuntimeException("Sign must be different than 0")
    new numericalty(if (sgn > 0) 1 else -1, true)
  }

  def numlSetinfInit(a: numericalty, sgn: Int): numericalty = numlSetinf(sgn)
  def numlSign(a: numericalty): Int = sign(a.num)
  def numlSet(b: numericalty): numericalty = new numericalty(b.num, b.inf)
  def numlSetint(i: Int): numericalty = new numericalty(i, false)
  def numlSetnum(i: Int): numericalty = new numericalty(i, false)
  def numlInit: numericalty = numericalty.zero
  def numlNeg(b: numericalty): numericalty = {
    if (numlInf(b)) numlSetinf(-numlSign(b))
    else numericalty.num(-b.num)
  }

  def numlAbs(b: numericalty): numericalty = new numericalty(scala.math.abs(b.num), b.inf)
  def numlAdd(b: numericalty, c: numericalty): numericalty = {
    if (numlInf(b)) numlSetinf(numlSign(b))
    else if (numlInf(c)) numlSetinf(numlSign(c))
    else new numericalty(b.num + c.num, false)
  }

  def numlLt(b: numericalty, c: Int): Boolean = {
    if (b.inf)
      numlSign(b) > 0
    else
      c < b.num
  }

  def numlLeq(b: numericalty, c: Int): Boolean = {
    if (b.inf)
      numlSign(b) > 0
    else
      c <= b.num
  }

  def numlGt(b: numericalty, c: Int): Boolean = {
    if (b.inf)
      numlSign(b) < 0
    else
      c > b.num
  }

  def numlGeq(b: numericalty, c: Int): Boolean = {
    if (b.inf)
      numlSign(b) < 0
    else
      c >= b.num
  }

  def numlSub(b: numericalty, c: numericalty): numericalty = {
    if (numlInf(b)) numlSetinf(numlSign(b))
    else if (numlInf(c)) numlSetinf(-numlSign(c))
    else new numericalty(b.num - c.num, false)
  }

  def numlSubInt(b: numericalty, c: Int): numericalty = {
    if (numlInf(b)) new numericalty(inf = b.inf, num = b.num)
    else { new numericalty(inf = false, num = b.num - c) }
  }

  def numlMul(b: numericalty, c: numericalty): numericalty = {
    if (numlSign(b) == 0 || numlSign(c) == 0) numlSetint(0)
    else if (numlInf(b) || numlInf(c)) numlSetinf(numlSign(b) * numlSign(c))
    else new numericalty(b.num * c.num, false)
  }

  def numlDiv(b: numericalty, c: numericalty): numericalty = {
    if (numlSign(b) == 0 || numlInf(c)) numlSetint(0)
    else if (numlSign(c) == 0) numlSetinf(numlSign(b))
    else if (numlInf(b)) numlSetinf(numlSign(b) * numlSign(c))
    else new numericalty(b.num / c.num, false)
  }

  def numlMin(b: numericalty, c: numericalty): numericalty = {
    if (numlInf(b)) if (numlSign(b) > 0) numlSet(c) else numlSet(b)
    else if (numlInf(c)) if (numlSign(c) > 0) numlSet(b) else numlSet(c)
    else new numericalty(math.min(b.num, c.num), false)
  }
  def numlMax(b: numericalty, c: numericalty): numericalty = {
    if (numlInf(b)) if (numlSign(b) > 0) numlSet(b) else numlSet(c)
    else if (numlInf(c)) if (numlSign(c) > 0) numlSet(c) else numlSet(b)
    else new numericalty(math.max(b.num, c.num), false)
  }

  def numlTrunc(b: numericalty): numericalty = {
    if (numlInf(b)) numlSetinf(numlSign(b))
    else { new numericalty(num = b.num, inf = false) }
  }

  def numlCompare(a: numericalty, b: numericalty): Int = {
    if (numlInf(a)) {
      if (numlInf(b)) (numlSign(a) - numlSign(b)) / 2
      else numlSign(a)
    }
    else {
      if (numlInf(b)) -numlSign(b)
      else a.num.compare(b.num)
    }
  }

  def numlEqual(a: numericalty, b: numericalty): Boolean = {
    if (numlInf(a)) {
      numlInf(b) && numlSign(a) == numlSign(b)
    }
    else {
      if (numlInf(b)) false
      else a.num == b.num
    }
  }

  def numlPrint(a: numericalty): Unit = {
    if (numlInf(a)) print("%coo" format (if (numlSign(a) > 0) '+' else '-'))
    else print("%d" format a.num)
  }

  def numlSprint(a: numericalty): String = {
    if (numlInf(a)) "%coo".format(if (numlSign(a) > 0) '+' else '-')
    else "%d" format a.num
  }
}
