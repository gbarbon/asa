package it.unive.dais.dapa.datatype

/**
  * @author gbarbon
  * @author esteffin
  */

import it.unive.dais.dapa.lib_intervals
import it.unive.dais.dapa.lib_intervals.intervals._
import it.unive.dais.dapa.utils.prelude.pretty

object SimpleInterval {

  class Interval (private val value: lib_intervals.intervals.intervt) extends pretty {

    def +^(y: Interval): Interval = new Interval(intervAdd(this.value, y.value))

    def <==(y: Interval): Boolean = lib_intervals.intervals.intervContains(y.value, this.value)
    def meet(y: Interval): Interval = new Interval(lib_intervals.intervals.intervMeet(this.value, y.value)._2)
    def join(y: Interval): Interval = new Interval(lib_intervals.intervals.intervJoin(this.value, y.value))
    def widening(y: Interval): Interval = new Interval(intervWidening(this.value, y.value))

    def isBottom: Boolean = intervBottomCheck(this.value)
    def isTop: Boolean = intervTopCheck(this.value)
    def isPoint: Boolean = intervPointCheck(this.value)
    def isOpenLeft: Boolean = intervOpenLefCheck(this.value)
    def isOpenRight: Boolean = intervOpenRightCheck(this.value)
    def getLeft: Int = intervGetLeft(this.value)
    def getRight: Int = intervGetRight(this.value)

    override def equals(o: Any) = o match {
      case that: Interval => intervEqCheck(value, that.value)
      case _ => false
    }
    override def hashCode = Interval.hashCode + value.hashCode
    override def pretty: String = "%s" format value.toString
  }
  object Interval {
    def fromNum(b: Int): Interval = new Interval(intervt.point(b))
    def interval(a: Int, b: Int): Interval = {
      if (a > b)
        println("Interval bounds are worng. %d should be greather than %d" format (a, b))
      new Interval(intervt.interval(a, b))
    }
    def open_left(a: Int): Interval = new Interval(intervt.open_left(a))
    def open_right(b: Int): Interval = new Interval(intervt.open_right(b))
    def top: Interval = new Interval(intervt.top)
    def bottom: Interval = new Interval(intervt.bottom)
  }
}
