package it.unive.dais.yaasa

/**
  * Created by esteffin on 12/01/16.
  */
private object graveyard {
  /*
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

    class NumAtVal extends pretty {
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

   */
}
