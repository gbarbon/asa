package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.lib_intervals
import it.unive.dais.yaasa.lib_intervals.itv._
import it.unive.dais.yaasa.utils.prelude.pretty

/**
  * @author gbarbon
  * @author esteffin
  */
object SimpleInterval {

  class Interval (private val value: lib_intervals.itv.itv_t) extends pretty {

    def +^(y: Interval): Interval = new Interval(itv_add(this.value, y.value))

    def <==(y: Interval): Boolean =
    //@FIXME: Fatto dopo mezzanotte... UNSTABLE!!!
      lib_intervals.itv.itv_contains(y.value, this.value)
    def meet(y: Interval): Interval = new Interval(lib_intervals.itv.itv_meet(this.value, y.value)._2)
    def join(y: Interval): Interval = new Interval(lib_intervals.itv.itv_join(this.value, y.value))
    def widening(y: Interval): Interval = new Interval(itv_widening(this.value, y.value))

    def isBottom: Boolean = itv_is_bottom(this.value)
    def isTop: Boolean = itv_is_top(this.value)
    def isPoint: Boolean = itv_is_point(this.value)
    def isOpenLeft: Boolean = itv_is_open_left(this.value)
    def isOpenRight: Boolean = itv_is_open_right(this.value)
    def getLeft: Int = itv_get_left(this.value)
    def getRight: Int = itv_get_right(this.value)

    override def equals(o: Any) = o match {
      case that: Interval => itv_is_eq(value, that.value)
      case _ => false
    }
    override def hashCode = Interval.hashCode + value.hashCode
    override def pretty: String = "%s" format value.toString
  }
  object Interval {
    def fromNum(b: Int): Interval = new Interval(itv_t.point(b))
    def interval(a: Int, b: Int): Interval = {
      if (a > b)
        println/*throw new EvaluationException*/("Interval bounds are worng. %d should be greather than %d" format (a, b))
      new Interval(itv_t.interval(a, b))
    }
    def open_left(a: Int): Interval = new Interval(itv_t.open_left(a))
    def open_right(b: Int): Interval = new Interval(itv_t.open_right(b))
    def top: Interval = new Interval(itv_t.top)
    def bottom: Interval = new Interval(itv_t.bottom)
  }
}
