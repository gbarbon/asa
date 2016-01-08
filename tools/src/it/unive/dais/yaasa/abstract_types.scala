package it.unive.dais.yaasa

import datatype.lattice._
import it.unive.dais.yaasa.abstract_types.NumAtVal.NumAt
import utils.pretty_print._
import utils.prelude._

/**
 * @author esteffin
 */
object abstract_types {

  trait AbstractFactory[A] {
    def top: A

    def bottom: A
  }

  case class BoolAt(value: Set[Boolean]) {
    def pretty = prettySet(value)

    def &&^(other: BoolAt) =
      BoolAt(for (x <- this.value; y <- other.value) yield x && y)

    def ||^(other: BoolAt) =
      BoolAt(for (x <- this.value; y <- other.value) yield x || y)

    def !^ =
      BoolAt(for (x <- this.value) yield !x)

    def ==^(other: BoolAt) =
      BoolAt(for (x <- this.value; y <- other.value) yield x == y)

    def !=^(other: BoolAt) =
      BoolAt(for (x <- this.value; y <- other.value) yield x != y)

    override def toString() = pretty
  }

  implicit def latticeBoolAt(l: BoolAt): Lattice[BoolAt] = new Lattice[BoolAt] {
    def <==(r: BoolAt): Boolean =
      l.value subsetOf r.value

    def join(r: BoolAt): BoolAt =
      if (this <== r) r else l

    def meet(r: BoolAt): BoolAt =
      if (this <== r) l else r

    override def toString() = l.toString()
  }

  object BoolAt extends AbstractFactory[BoolAt] {
    def fromBool(b: Boolean): BoolAt = BoolAt(Set(b))

    def sTrueAt = fromBool(true)

    def sFalseAt = fromBool(false)

    def top = BoolAt(Set(true, false))

    def bottom: BoolAt = BoolAt(Set.empty[Boolean])
  }

  object NumAtVal {

    case object Plus extends NumAtVal

    case object Zero extends NumAtVal

    case object Minus extends NumAtVal

    object NumAtVal {
      def from_num(n: Int) =
        n match {
          case _ if n == 0 => Zero
          case _ if n > 0 => Plus
          case _ if n < 0 => Minus
        }
    }

    case class NumAt(value: Set[NumAtVal]) {
      def op(f: (NumAtVal, NumAtVal) => NumAtVal, x: NumAt, y: NumAt): NumAt = {
        val res = for (v1 <- x.value; v2 <- y.value) yield (v1 + v2).value
        NumAt(res flatten)
      }

      def +(y: NumAt): NumAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 + v2).value
        NumAt(res flatten)
      }

      def *(y: NumAt): NumAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 * v2).value
        NumAt(res flatten)
      }

      def /(y: NumAt): NumAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 / v2).value
        NumAt(res flatten)
      }

      def inv: NumAt = {
        NumAt((this.value map {
          _.inv.value
        }) flatten)
      }

      def -(y: NumAt) = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 - v2).value
        NumAt(res flatten)
      }

      def %(y: NumAt): NumAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 % v2).value
        NumAt(res flatten)
      }

      def ==^(y: NumAt): BoolAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 ==^ v2)
        res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }

      def !=^(y: NumAt): BoolAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 !=^ v2)
        res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }

      def >^(y: NumAt): BoolAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 >^ v2)
        res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }

      def <^(y: NumAt): BoolAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 <=^ v2)
        res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }

      def <=^(y: NumAt): BoolAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 <=^ v2)
        res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }

      def >=^(y: NumAt): BoolAt = {
        val res = for (v1 <- this.value; v2 <- y.value) yield (v1 >=^ v2)
        res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }
    }

    object NumAt extends AbstractFactory[NumAt] {
      def from_num_s(n: Int) =
        n match {
          case _ if n == 0 => Set(Zero)
          case _ if n > 0 => Set(Plus)
          case _ if n < 0 => Set(Minus)
          case _ => bottom //set [Zero; Plus; Minus]
        }

      def top = NumAt(Set(Plus, Minus, Zero))

      def bottom = NumAt(Set.empty[NumAtVal])
    }

    implicit def latticeBoolAt(l: NumAt): Lattice[NumAt] = new Lattice[NumAt] {
      def <==(r: NumAt): Boolean =
        l.value subsetOf r.value

      def join(r: NumAt): NumAt =
        if (this <== r) r else l

      def meet(r: NumAt): NumAt =
        if (this <== r) l else r

      override def toString() = l.toString()
    }

    class NumAtVal {
      def pretty =
        this match {
          case Plus => "(+)"
          case Zero => "(0)"
          case Minus => "(-)"
        }

      def +(y: NumAtVal): NumAt =
        (this, y) match {
          case (Zero, n) => NumAt(Set(n))
          case (n, Zero) => NumAt(Set(n))
          case (Plus, Plus) => NumAt(Set(Plus))
          case (Minus, Minus) => NumAt(Set(Minus))
          case (Plus, Minus) => NumAt(Set(Plus, Minus, Zero))
          case (Minus, Plus) => NumAt(Set(Plus, Minus, Zero))
        }

      def *(y: NumAtVal): NumAt =
        (this, y) match {
          case (Zero, _) => NumAt(Set(Zero))
          case (_, Zero) => NumAt(Set(Zero))
          case (Plus, Plus) => NumAt(Set(Plus))
          case (Minus, Minus) => NumAt(Set(Plus))
          case (Plus, Minus) => NumAt(Set(Minus))
          case (Minus, Plus) => NumAt(Set(Minus))
        }

      def /(y: NumAtVal): NumAt =
        (this, y) match {
          case (_, Zero) => NumAt.bottom
          case (Plus, Plus) => NumAt(Set(Plus))
          case (Minus, Minus) => NumAt(Set(Plus))
          case (Plus, Minus) => NumAt(Set(Minus))
          case (Minus, Plus) => NumAt(Set(Minus))
          case (Zero, _) => NumAt(Set(Zero))
        }

      def inv: NumAt =
        this match {
          case Zero => NumAt(Set(Zero))
          case Plus => NumAt(Set(Minus))
          case Minus => NumAt(Set(Plus))
        }

      def -(y: NumAtVal) = {
        NumAt(Set(this)) + (y.inv)
      }

      def %(y: NumAtVal): NumAt =
        (this, y) match {
          case (_, Zero) => NumAt.bottom
          case (Zero, _) => NumAt(Set(Zero))
          case (Plus, _) => NumAt(Set(Plus, Zero))
          case (Minus, _) => NumAt(Set(Minus, Zero))
        }

      def ==^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Zero, Zero) => BoolAt.sTrueAt
          case (Minus, Minus) => BoolAt.top
          case (Plus, Plus) => BoolAt.top
          case (_, _) => BoolAt.sFalseAt
        }

      def !=^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Zero, Zero) => BoolAt.sFalseAt
          case (Minus, Minus) => BoolAt.top
          case (Plus, Plus) => BoolAt.top
          case (_, _) => BoolAt.sTrueAt
        }

      def >^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Plus, Plus) => BoolAt.top
          case (Minus, Minus) => BoolAt.top
          case (Zero, Zero) => BoolAt.sFalseAt
          case (Minus, _) => BoolAt.sFalseAt
          case (Zero, Plus) => BoolAt.sFalseAt
          case (_, _) => BoolAt.sTrueAt
        }

      def <^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Plus, Plus) => BoolAt.top
          case (Minus, Minus) => BoolAt.top
          case (Zero, Zero) => BoolAt.sFalseAt
          case (Minus, _) => BoolAt.sTrueAt
          case (Zero, Plus) => BoolAt.sTrueAt
          case (_, _) => BoolAt.sFalseAt
        }

      def <=^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Plus, Plus) => BoolAt.top
          case (Minus, Minus) => BoolAt.top
          case (Zero, Zero) => BoolAt.sTrueAt
          case (Minus, _) => BoolAt.sTrueAt
          case (Zero, Plus) => BoolAt.sTrueAt
          case (_, _) => BoolAt.sFalseAt
        }

      def >=^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Plus, Plus) => BoolAt.top
          case (Minus, Minus) => BoolAt.top
          case (Zero, Zero) => BoolAt.sTrueAt
          case (Minus, _) => BoolAt.sFalseAt
          case (Zero, Plus) => BoolAt.sFalseAt
          case (_, _) => BoolAt.sTrueAt
        }
    }

  }

  //type NumAt = NumAtVal.NumAt

  object Intervals_old {

    /**
      * Operations taken from:
      * http://grouper.ieee.org/groups/1788/PositionPapers/ARITHYY.pdf
      * http://grouper.ieee.org/groups/1788/email/pdfBZjRrPv9JU.pdf
      * http://stackoverflow.com/questions/31057473/calculating-the-modulus-of-two-intervals
      */
    abstract class Interval {
      val exact: Boolean

      def negative: Interval

      def positive: Interval

      def contains(n: Int): Boolean

      def pretty: String

      def +(r: Interval): Interval =
        (this, r) match {
          case (TopInterval(), _) => TopInterval()
          case (_, TopInterval()) => TopInterval()
          case (LeftInf(a2), LeftInf(b2)) => LeftInf(a2 + b2)
          case (LeftInf(a2), ConcInterval(b1, b2)) => LeftInf(a2 + b2)
          case (LeftInf(a2), RightInf(b1)) => TopInterval()
          case (ConcInterval(a1, a2), LeftInf(b2)) => LeftInf(a2 + b2)
          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) => ConcInterval(a1 + b1, a2 + b2)
          case (ConcInterval(a1, a2), RightInf(b1)) => RightInf(a1 + b1)
          case (RightInf(_), LeftInf(_)) => TopInterval()
          case (RightInf(a1), ConcInterval(b1, b2)) => RightInf(a1 + b1)
          case (RightInf(a1), RightInf(b1)) => RightInf(a1 + b1)
        }

      def -(r: Interval): Interval =
        this + (r.neg)

      def neg =
        this match {
          case TopInterval() => TopInterval()
          case BottomInterval() => BottomInterval()
          case LeftInf(l) => RightInf(-l)
          case RightInf(l) => LeftInf(-l)
          case ConcInterval(a, b) => ConcInterval(-b, -a) // fixed interval (before was [-a, -b])
        }

      def *(r: Interval): Interval =
        (this, r) match {
          case (ConcInterval(0, 0), _) => ConcInterval(0, 0)
          case (_, ConcInterval(0, 0)) => ConcInterval(0, 0)
          case (TopInterval(), _) => TopInterval()
          case (_, TopInterval()) => TopInterval()

          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a2 <= 0 && b2 <= 0) => ConcInterval(a2 * b2, a1 * b1)
          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a2 <= 0 && b1 < 0 && 0 < b2) => ConcInterval(a1 * b2, a1 * b1)
          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a2 <= 0 && b1 >= 0) => ConcInterval(a1 * b2, a2 * b1)
          case (ConcInterval(a1, a2), LeftInf(b2)) if (a2 <= 0 && b2 <= 0) => RightInf(a2 * b2)
          case (ConcInterval(a1, a2), LeftInf(b2)) if (a2 <= 0 && b2 >= 0) => RightInf(a1 * b2)
          case (ConcInterval(a1, a2), RightInf(b1)) if (a2 <= 0 && b1 <= 0) => LeftInf(a1 * b1)
          case (ConcInterval(a1, a2), RightInf(b1)) if (a2 <= 0 && b1 >= 0) => LeftInf(a2 * b1)

          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a1 < 0 && 0 < a2 && b2 <= 0) => ConcInterval(a2 * b1, a1 * b1)
          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a1 < 0 && 0 < a2 && b1 < 0 && 0 < b2) => ConcInterval(min(a1 * b2, a2 * b1), max(a1 * b1, a2 * b2))
          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a1 < 0 && 0 < a2 && b1 <= 0) => ConcInterval(a1 * b2, a2 * b2)
          case (ConcInterval(a1, a2), LeftInf(b2)) if (a1 < 0 && 0 < a2 && b2 <= 0) => TopInterval()
          case (ConcInterval(a1, a2), LeftInf(b2)) if (a1 < 0 && 0 < a2 && b2 >= 0) => TopInterval()
          case (ConcInterval(a1, a2), RightInf(b1)) if (a1 < 0 && 0 < a2 && b1 <= 0) => TopInterval()
          case (ConcInterval(a1, a2), RightInf(b1)) if (a1 < 0 && 0 < a2 && b1 >= 0) => TopInterval()

          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a2 >= 0 && b2 <= 0) => ConcInterval(a2 * b1, a1 * b2)
          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a2 >= 0 && b1 < 0 && 0 < b2) => ConcInterval(a2 * b1, a2 * b2)
          case (ConcInterval(a1, a2), ConcInterval(b1, b2)) if (a2 >= 0 && b1 >= 0) => ConcInterval(a1 * b1, a2 * b2)
          case (ConcInterval(a1, a2), LeftInf(b2)) if (a2 >= 0 && b2 <= 0) => LeftInf(a1 * b2)
          case (ConcInterval(a1, a2), LeftInf(b2)) if (a2 >= 0 && b2 >= 0) => LeftInf(a2 * b2)
          case (ConcInterval(a1, a2), RightInf(b1)) if (a2 >= 0 && b1 <= 0) => RightInf(a2 * b1)
          case (ConcInterval(a1, a2), RightInf(b1)) if (a2 >= 0 && b1 >= 0) => RightInf(a1 * b1)

          case (LeftInf(a2), ConcInterval(b1, b2)) if (a2 <= 0 && b2 <= 0) => RightInf(a2 * b2)
          case (LeftInf(a2), ConcInterval(b1, b2)) if (a2 <= 0 && b1 < 0 && 0 < b2) => TopInterval()
          case (LeftInf(a2), ConcInterval(b1, b2)) if (a2 <= 0 && b1 >= 0) => LeftInf(a2 * b1)
          case (LeftInf(a2), LeftInf(b2)) if (a2 <= 0 && b2 <= 0) => RightInf(a2 * b2)
          case (LeftInf(a2), LeftInf(b2)) if (a2 <= 0 && b2 >= 0) => TopInterval()
          case (LeftInf(a2), RightInf(b1)) if (a2 <= 0 && b1 <= 0) => TopInterval()
          case (LeftInf(a2), RightInf(b1)) if (a2 <= 0 && b1 >= 0) => LeftInf(a2 * b1)

          case (LeftInf(a2), ConcInterval(b1, b2)) if (a2 >= 0 && b2 <= 0) => RightInf(a2 * b1)
          case (LeftInf(a2), ConcInterval(b1, b2)) if (a2 >= 0 && b1 < 0 && 0 < b2) => TopInterval()
          case (LeftInf(a2), ConcInterval(b1, b2)) if (a2 >= 0 && b1 >= 0) => LeftInf(a2 * b2)
          case (LeftInf(a2), LeftInf(b2)) if (a2 >= 0 && b2 <= 0) => TopInterval()
          case (LeftInf(a2), LeftInf(b2)) if (a2 >= 0 && b2 >= 0) => TopInterval()
          case (LeftInf(a2), RightInf(b1)) if (a2 >= 0 && b1 <= 0) => TopInterval()
          case (LeftInf(a2), RightInf(b1)) if (a2 >= 0 && b1 >= 0) => TopInterval()

          case (RightInf(a1), ConcInterval(b1, b2)) if (a1 <= 0 && b2 <= 0) => RightInf(a1 * b1)
          case (RightInf(a1), ConcInterval(b1, b2)) if (a1 <= 0 && b1 < 0 && 0 < b2) => TopInterval()
          case (RightInf(a1), ConcInterval(b1, b2)) if (a1 <= 0 && b1 >= 0) => LeftInf(a1 * b2)
          case (RightInf(a1), LeftInf(b2)) if (a1 <= 0 && b2 <= 0) => TopInterval()
          case (RightInf(a1), LeftInf(b2)) if (a1 <= 0 && b2 >= 0) => TopInterval()
          case (RightInf(a1), RightInf(b1)) if (a1 <= 0 && b1 <= 0) => TopInterval()
          case (RightInf(a1), RightInf(b1)) if (a1 <= 0 && b1 >= 0) => TopInterval()

          case (RightInf(a1), ConcInterval(b1, b2)) if (a1 >= 0 && b2 <= 0) => RightInf(a1 * b2)
          case (RightInf(a1), ConcInterval(b1, b2)) if (a1 >= 0 && b1 < 0 && 0 < b2) => TopInterval()
          case (RightInf(a1), ConcInterval(b1, b2)) if (a1 >= 0 && b1 >= 0) => LeftInf(a1 * b1)
          case (RightInf(a1), LeftInf(b2)) if (a1 >= 0 && b2 <= 0) => LeftInf(a1 * b2)
          case (RightInf(a1), LeftInf(b2)) if (a1 >= 0 && b2 >= 0) => TopInterval()
          case (RightInf(a1), RightInf(b1)) if (a1 >= 0 && b1 <= 0) => TopInterval()
          case (RightInf(a1), RightInf(b1)) if (a1 >= 0 && b1 >= 0) => RightInf(a1 * b1)
        }

      //      def % (r:Interval) : Interval =

      override def toString(): String = pretty
    }

    implicit def latticeBoolAt(l: Interval): Lattice[Interval] = new Lattice[Interval] {
      //TODO: doublecheck
      def <==(r: Interval): Boolean =
        (l, r) match {
          case (BottomInterval(), _) => true
          case (_, BottomInterval()) => false
          case (_, TopInterval()) => true
          case (TopInterval(), _) => false
          case (LeftInf(_), RightInf(_)) => false
          case (RightInf(_), LeftInf(_)) => false
          case (LeftInf(ml), LeftInf(mr)) => ml <= mr
          case (RightInf(ml), RightInf(mr)) => ml >= mr
          case (LeftInf(_), ConcInterval(_, _)) => false
          case (RightInf(_), ConcInterval(_, _)) => false
          case (ConcInterval(_, ml), LeftInf(mr)) => ml <= mr
          case (ConcInterval(ml, _), RightInf(mr)) => ml >= mr
          case (ConcInterval(mil, mal), ConcInterval(mir, mar)) => mil >= mir && mal <= mar
          case _ =>
            throw new Unexpected("Cannot apply <== on intervals: %s, %s" format(l, r))
        }

      def join(r: Interval): Interval =
      //if (this <== r) r else
        (l, r) match {
          case (BottomInterval(), r) => r
          case (l, BottomInterval()) => l
          case (l, TopInterval()) => TopInterval()
          case (TopInterval(), _) => TopInterval()
          case (LeftInf(_), RightInf(_)) => TopInterval()
          case (RightInf(_), LeftInf(_)) => TopInterval()
          case (LeftInf(ml), LeftInf(mr)) => LeftInf(max(ml, mr))
          case (RightInf(ml), RightInf(mr)) => RightInf(min(ml, mr))
          case (LeftInf(ml), ConcInterval(_, mr)) => LeftInf(max(ml, mr))
          case (RightInf(ml), ConcInterval(mr, _)) => RightInf(min(ml, mr))
          case (ConcInterval(_, ml), LeftInf(mr)) => LeftInf(max(ml, mr))
          case (ConcInterval(ml, _), RightInf(mr)) => RightInf(min(ml, mr))
          case (ConcInterval(mil, mal), ConcInterval(mir, mar)) => ConcInterval(min(mil, mir), max(mal, mar))
          case _ =>
            throw new Unexpected("Cannot apply JOIN on intervals: %s, %s" format(l, r))
        }

      def meet(r: Interval): Interval =
        throw new Unexpected("Meet of interval not implemented...")

      //FIXME: implement this using factory for bottom interval
      //if (this <== r) l else r
      /*(l, r) match {
        case (BottomInterval(), r) => BottomInterval()
        case (l, BottomInterval()) => BottomInterval()
        case (l, TopInterval())    => l
        case (TopInterval(), r)    => r
        case (LeftInf(l), RightInf(r)) =>
          if (l >= r) Factory.concInterval(r, l)
          else BottomInterval()
        case (RightInf(l), LeftInf(r)) =>
          if (l <= r) Factory.concInterval(l, r)
          else BottomInterval()
        case (LeftInf(ml), LeftInf(mr))                       => LeftInf(min(ml, mr))
        case (RightInf(ml), RightInf(mr))                     => RightInf(max(ml, mr))
        case (LeftInf(ml), ConcInterval(mir, mar))            =>
          ConcInterval(max(ml, mir), min(ml, mar))
        case (RightInf(ml), ConcInterval(mr, _))              => RightInf(min(ml, mr))
        case (ConcInterval(_, ml), LeftInf(mr))               => LeftInf(max(ml, mr))
        case (ConcInterval(ml, _), RightInf(mr))              => RightInf(min(ml, mr))
        case (ConcInterval(mil, mal), ConcInterval(mir, mar)) => ConcInterval(min(mil, mir), max(mal, mar))
        case _ =>
          throw new Unexpected("Cannot apply MEET on intervals: %s, %s" format (l, r))
      }*/
      override def toString() = l.toString()
    }

    object Factory extends AbstractFactory[Interval] {
      def top: Interval = TopInterval()

      def bottom: Interval = BottomInterval()

      def leftInf(max: Int): Interval = LeftInf(max)

      def rightInf(min: Int): Interval = LeftInf(min)

      def concInterval(min: Int, max: Int) =
        if (min <= max) ConcInterval(min, max)
        else BottomInterval()
    }

    case class BottomInterval() extends Interval {
      val exact = false

      def negative: Interval = BottomInterval()

      def positive: Interval = BottomInterval()

      def contains(n: Int): Boolean = false

      /*def <==(r: Interval): Boolean =
        true
      def join(r: Interval): Interval =
        r
      def meet(r: Interval): Interval =
        this*/
      def pretty = "[]"
    }

    case class TopInterval() extends Interval {
      val exact = false

      def negative: Interval = LeftInf(0)

      def positive: Interval = RightInf(0)

      def contains(n: Int): Boolean = true

      /*def <==(r: Interval): Boolean =
        r.isInstanceOf[TopInterval]
      def join(r: Interval): Interval =
        this
      def meet(r: Interval): Interval =
        r*/
      def pretty = "[-INF:INF]"
    }

    case class LeftInf(max: Int) extends Interval {
      val exact = false

      def negative: Interval = LeftInf(if (max > 0) 0 else max)

      def positive: Interval = if (max > 0) ConcInterval(0, max) else BottomInterval()

      def contains(n: Int): Boolean = n <= max

      def pretty = "[-INF:%s]" format max
    }

    case class RightInf(min: Int) extends Interval {
      val exact = false

      def negative: Interval = if (min < 0) ConcInterval(0, min) else BottomInterval()

      def positive: Interval = RightInf(if (min < 0) 0 else min)

      def contains(n: Int): Boolean = min <= n

      def pretty = "[%s:INF]" format min
    }

    case class ConcInterval(min: Int, max: Int) extends Interval {
      val exact = min == max

      def negative: Interval = if (min < 0) ConcInterval(0, min) else BottomInterval()

      def positive: Interval = if (min < 0) ConcInterval(0, min) else BottomInterval()

      def contains(n: Int): Boolean = min <= n && n <= max

      def pretty = "[%s:%s]" format(min, max)
    }

  }

  class NumAt private(private val value: lib_intervals.itv.itv_t) {

    import lib_intervals.itv._

    def pretty = itv_sprint(value)

    def +^(y: NumAt): NumAt = new NumAt(itv_add(this.value, y.value))

    def *^(y: NumAt): NumAt = new NumAt(itv_mul(this.value, y.value))

    def /^(y: NumAt): NumAt = new NumAt(itv_div(this.value, y.value))

    def -^(y: NumAt): NumAt = new NumAt(itv_sub(this.value, y.value))

    def %^(y: NumAt): NumAt = new NumAt(itv_mod(this.value, y.value))

    def negAt: NumAt = new NumAt(itv_neg(this.value))

    def absAt: NumAt = new NumAt(itv_abs(this.value))

    def ==^(y: NumAt): BoolAt = new BoolAt(itv_eqat(this.value, y.value))

    def !=^(y: NumAt): BoolAt = new BoolAt(itv_neqat(this.value, y.value))

    def >^(y: NumAt): BoolAt = new BoolAt(itv_gtat(this.value, y.value))

    def <^(y: NumAt): BoolAt = new BoolAt(itv_ltat(this.value, y.value))

    def <=^(y: NumAt): BoolAt = new BoolAt(itv_leqat(this.value, y.value))

    def >=^(y: NumAt): BoolAt = new BoolAt(itv_geqat(this.value, y.value))

    def widening(y: NumAt): NumAt = new NumAt(itv_widening(this.value, y.value))

    def <==(y: NumAt): Boolean = lib_intervals.itv.itv_contains(this.value, y.value)

    def meet(y: NumAt): NumAt = new NumAt(lib_intervals.itv.itv_meet(this.value, y.value)._2)

    def join(y: NumAt): NumAt = new NumAt(lib_intervals.itv.itv_join(this.value, y.value))

    override def equals(o: Any) = o match {
      case that: NumAt => itv_is_eq(value, that.value)
      case _ => false
    }

    override def hashCode = NumAt.hashCode + value.hashCode

    //TODO: Maybe add other comparison here
    override def toString = this.pretty
  }

  object NumAt extends AbstractFactory[NumAt] {

    import lib_intervals.itv._

    def fromNum(b: Int): NumAt = new NumAt(itv_t.point(b))

    def interval(a: Int, b: Int): NumAt = new NumAt(itv_t.interval(a, b))

    def open_left(a: Int): NumAt = new NumAt(itv_t.open_left(a))

    def open_right(b: Int): NumAt = new NumAt(itv_t.open_left(b))

    def top: NumAt = new NumAt(itv_t.top)

    def bottom: NumAt = new NumAt(itv_t.bottom)
  }

  implicit def latticeNumAt(l: NumAt): Lattice[NumAt] = new Lattice[NumAt] {
    def <==(r: NumAt): Boolean = l <== r

    def join(r: NumAt): NumAt = l join r

    def meet(r: NumAt): NumAt = l meet r

    override def toString() = l.toString()
  }


  object StringAt {

    object StringAt {
      def top = StringAt("")
    }

    case class StringAt(value: String) {

      override def toString() = pretty

      def pretty =
        "\"%s*\"" format value

      def length =
        NumAt.open_right(0)

      def Substring(startIndex: Int, length: Int) = {
        val l = value.length
        if (startIndex > l)
          StringAt.top
        else if (startIndex <= l && length > l)
          value.substring(startIndex, l - startIndex)
        else
          value.substring(startIndex, length)
      }

      /*
  member self.Contains c  =
    //WARNING da controllare come fa Giulia
    match self, c with
    | Exact s, Exact c -> BoolAt.s_from_bool <| s.Contains c
    | Exact s, Pref c  -> if s.Contains c then BoolAt.Top else BoolAt.SFalseAt
    | Pref s, Exact c  ->
      BoolAt.Top
      //if c.StartsWith s then BoolAt.Top else BoolAt.SFalseAt
    | Pref s, Pref c   ->
        BoolAt.Top
  static member from_e_string s = Exact s
  static member from_string_s s = sset <| Exact s
  //static member from_p_string s = Pref s
  static member Top = Pref ""
  static member (+) (x, y : StringAt)   =
    match x with
    | Exact x
    | Pref x -> Pref x
  static member (=^) (x, y)             =
    match x, y with
    | Exact x, Exact y  ->
      if x = y then
        BoolAt.STrueAt
      else
        BoolAt.SFalseAt
    | Exact x, Pref y   ->
      if x.StartsWith y then
        BoolAt.Top
      else
        BoolAt.SFalseAt
    | Pref y, Exact x   ->
      if x.StartsWith y then
        BoolAt.Top
      else
        BoolAt.SFalseAt
    | Pref x, Pref y    ->
      if x.StartsWith y || y.StartsWith x then
        BoolAt.Top
      else
        BoolAt.SFalseAt
  static member (<>^) (x : StringAt, y) =
      let r = x =^ y
      seq{ for c in r do yield !^c } |> Set.ofSeq
  static member (=?) (x : StringAt, y)  =
    x =^ y |> Set.contains TrueAt
  static member (<^) (x : StringAt, y)  =
    match x, y with
    | Exact x, Exact y -> x < y |> BoolAt.from_bool |> sset
    | _ ->
      //WARNING metto sempre top perche' devo controllare le disuguaglianze su stringhe
      BoolAt.Top
  /*let m_dim = min (x.Length) (y.Length)
      let x = x.Substring(0, m_dim)
      let y = y.Substring(0, m_dim)
      if x <> y then
        BoolAt.STrueAt
      else
        BoolAt.Top*/*/
    }
  }
}
