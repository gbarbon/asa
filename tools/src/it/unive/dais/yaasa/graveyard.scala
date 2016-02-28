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

  /*

  file: analyzer

  * package it.unive.dais.yaasa

/**
 * @author esteffin
 * @author gbarbon
 */

import utils.prelude._
//import utils.pretty_print._
import utils.env._
import absyn._
//import scala.collection.breakOut
//import functConvert._
import it.unive.dais.yaasa.datatype.ADType._
import it.unive.dais.yaasa.datatype.CADInfo.CADInfo
import it.unive.dais.yaasa.datatype.CADInfo.CADInfoFactory
import it.unive.dais.yaasa.datatype.FortyTwo._

/**
 *
 */
object analyzer {

  case class EvaluationException(_message: string) extends MessageException("Evaluation exception: %s" format _message) {
    /*def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))*/
  }
  trait ConcreteValue {
    val value: Any
    val ty: Type
  }

  type ValueWAbstr = (ConcreteValue, CADInfo)

  type EvEnv = Env[String, ValueWAbstr]

  type MethInfo = Env[String, (MethodDecl, EvEnv)]

  case class IntValue(value: Int) extends ConcreteValue {
    def this() = this(0)
    val ty = TyInt
    override def toString = "%d" format value
  }
  case class BoolValue(value: Boolean) extends ConcreteValue {
    def this() = this(false)
    val ty = TyBool
    override def toString = "%b" format value
  }
  case class StringValue(value: String) extends ConcreteValue {
    def this() = this(null) //Only for compatibility with the horrendous java
    val ty = TyString
    override def toString = "%s" format value
  }
  case class UnitValue() extends ConcreteValue {
    val ty = TyType("Unit")
    val value = throw new EvaluationException("Cannot access unit value")
    override def toString = "()"
  }

  class Analyzer(program: Program) {
    /**
     * Must require the insertion of the confidential labels before the execution.
     *
     * 1) The user must manually insert all the confidential label used in the program.
     * 2) Or we must introduce a way to locate them in the code.
     *    But, maybe it is faster to insert the list of all the confidential labels before,
     *    rather than searching them inside the code and enrich the code with something that locate the label.
     * 3) We can think to load both the code file with a label file.
     * 4) Or, we can modify the code and insert at the beginning the list of confidential labels. <---
     * 5) or we can recognize them with a function readLabel <---
     * --> OR GIVE ALL THE OPTIONS TO THE USER <--
     *
     * So at the begin, all the label objects are created.
     */

    var logs: List[ValueWAbstr] = List.empty[ValueWAbstr]

    private val ctx: MethInfo =
      program match {
        case Program(List()) => throw new EvaluationException("Empty class definition.")
        case Program(classes) =>
          val venv: EvEnv =
            Env(
              ((for (Class(name, _, fields, _) <- classes)
                yield createVars(fields map { case FieldDecl(ty, ns) => (ty, ns.map({x => "%s.%s" format (name, x) })) }, CADInfoFactory.empty)) flatten)toMap) //@FIXME: cosa passiamo come implFlow??

          Env(
            (for (Class(cname, _, _, methods) <- classes; m <- methods)
              yield ("%s.%s" format (cname, m.name), (m, venv))) toMap)
      }

    /**
     *
     * @return
     */
    def evaluateProgram(): (Option[(ConcreteValue, CADInfo)], EvEnv) =
      {
        ctx search_by_key { _ endsWith ".main" } match {
          case Some(main) => evaluateCall(main, List(), "MAIN", CADInfoFactory.empty) //@FIXME: cosa passiamo come implFlow??
          case None       => throw new EvaluationException("No main found...")
        }
      }

    def evaluateCall(call: (MethodDecl, EvEnv), actuals: List[ValueWAbstr], call_point_uid: Uid, implFlow: CADInfo): (Option[ValueWAbstr], EvEnv) = {
      val (md, env) = call

      if (md.formals.length != actuals.length)
        throw new EvaluationException("Function %s is called with wrong argument number")

      val form_bind =
        for ((form, act) <- md.formals.zip(actuals))
          yield
            if (form.ty != act._1.ty)
              throw new EvaluationException("Type error in method %s: formal %s has type %s, but is given type %s at %s".format(md.name, form.ty, act._1.ty, md.loc))
            else
              (form.name, act)
      val (ret, fenv) = evaluateBlock(env binds_new form_bind, md.body, implFlow)
      //val adexp = actuals.head
      val new_ret = (ret, md.annot) match {
        case (None, _)   => ret
        case (ret, None) => ret
        case (Some((retv, retLab)), Some(fannot)) =>
          fannot match {
            case annot: FunAnnot =>
              val actuals_annots = actuals map { _._2 }
              actuals_annots.length match {
                // @FIXME: fix theUid; actuals(1)_1 is a ConcreteValue, but we need abstract! (are we sure that actuals contains the parameters?)
                case 1 => Some((retv, actuals_annots.head.update(annot, call_point_uid, null /*actuals(1)._1*/).join(implFlow))) //@TODO: check correctness of implicit
                case 2 => Some((retv, actuals_annots.head.update(annot, call_point_uid, (null, null) /*(actuals(0)._1, actuals(1)._1)*/, actuals_annots(1)).join(implFlow))) //@TODO: check correctness of implicit
                //case _ => Some((retv, actuals_annots.head.update(annot, call_point_uid, List.empty[AbstractValue] /*actuals.map(_._1).toList*/, actuals_annots.tail).join(implFlow))) //@TODO: check correctness of implicit
              }
            case lab: LabelAnnot => Some((retv, CADInfoFactory.fromLabelAnnot(lab).join(implFlow))) //@TODO: check correctness of implicit
            case _               => throw new Unexpected("Unknown annotation type %s." format fannot.toString)
          }
      }
      (new_ret, env update_values fenv)
    }

    /**
     * Create the set of fields in a class, all empty labels
     */
    def createVars(vars: List[(Type, List[string])], implFlow: CADInfo): List[(string, ValueWAbstr)] = {
      for ((ty, names) <- vars; name <- names)
        yield (name,
          ty match {
            case TyInt => (new IntValue(), CADInfoFactory.star.join(implFlow)) //@TODO: check correctness of implicit
            case TyBool => (new BoolValue(), CADInfoFactory.star.join(implFlow)) //@TODO: check correctness of implicit
            case TyString => (new StringValue(), CADInfoFactory.star.join(implFlow)) //@TODO: check correctness of implicit
            case _ => throw new Unexpected("Variable %s has not supported type %s", (name, ty))
          })
    }

    /**
     * Evaluate the block
     */
    def evaluateBlock(env: EvEnv, block: Block, implFlow: CADInfo): (Option[ValueWAbstr], EvEnv) = {
      val nenv: List[(id, ValueWAbstr)] = createVars(block.varDecls map { vd => (vd.ty, vd.ids) }, implFlow)

      val (ret, fenv) = block.stmts.foldLeft(None: Option[ValueWAbstr], env binds_new nenv) {
        case (ret @ (Some(_), _), stmt) => ret
        case ((None, env), stmt)        => evaluateStmt(env, stmt, implFlow)
      }
      (ret, env update_values fenv)
    }

    def evaluateStmt(env: EvEnv, stmt: Stmt, implFlow: CADInfo): (Option[ValueWAbstr], EvEnv) = {
      stmt match {
        case SSkip => (None, env)
        case SAssign(x, e) =>
          val (res, nenv) = evaluateExpr(env, e, implFlow)
          if (res._1.ty != nenv.lookup(x)._1.ty)
            throw new EvaluationException("Type error: variable %s has type %s, but is given type %s at %s".format(x, nenv.lookup(x)._1.ty, res._1.ty, stmt.loc.toString()))
          else
            (None, nenv.update(x) { _ => res })
        case SIf(c, thn, els) => //@TODO: collect the implicit!!
          val (cond, nenv) = evaluateExpr(env, c, implFlow)
          cond._1 match {
            case BoolValue(v) =>
              if (v)
                evaluateStmt(nenv, thn, cond._2.asImplicit)
              else
                evaluateStmt(nenv, els, cond._2.asImplicit)
            case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
          }
        case SWhile(c, body) => //@TODO: collect the implicit!!
          val (cond, nenv) = evaluateExpr(env, c, implFlow)
          cond._1 match {
            case BoolValue(v) =>
              if (v) {
                evaluateStmt(nenv, body, implFlow) match {
                  case (None, wenv) => evaluateStmt(wenv, stmt, implFlow)
                  case ret@(Some(_), _) => ret
                }
              }
              else
                (None, nenv)
            case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
          }
        case SBlock(block) => evaluateBlock(env, block, implFlow)
        case SReturn(None) => (Some(UnitValue(), CADInfoFactory.star.join(implFlow)), env) //@TODO: check correctness of implicit
        case SReturn(Some(e)) =>
          val (res, nenv) = evaluateExpr(env, e, implFlow)
          (Some(res), nenv)
        case SPrint(ln, actual) =>
          val (vactual, nenv) = evaluateExpr(env, actual, implFlow)
          logs = vactual :: logs
          if (config.value.verbose)
            if (ln) println(vactual._1.value) else print(vactual._1.value)
          (None, nenv)
        case scall@SCall(name, actuals) => //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyCall(env, name, actuals, scall.uid, implFlow) match {
            case (Some(_), env) => (None, env)
            case (None, env) => (None, env) //@FIXME: URGENT!!!
          }
        case scall@SNativeCall(name, actuals) => //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyNativeCall(env, name, actuals, scall.uid, implFlow) match {
            case (Some(_), env) => (None, env)
            case (None, env) => (None, env) //@FIXME: URGENT!!!
          }
        //case rets @ SReturn(_) => evaluateReturn(env, rets)
        case SMethodCall(_, _) => throw new NotSupportedException("Statement Method Call not supported at %s" format stmt.loc)
        case SSetField(_, _) => throw new NotSupportedException("Set field not supported at %s" format stmt.loc)
      }
    }

    def applyCall(env: EvEnv, name: String, actuals: List[Expr], call_point_uid: Uid, implFlow: CADInfo): (Option[ValueWAbstr], EvEnv) =
      //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
      if (ctx.occurs(name)) {
        val (vacts, nenv) = evaluateActuals(env, actuals, implFlow)

        val (m, cenv) = ctx lookup name
        val (ret, fcenv) = evaluateCall((m, cenv update_values nenv), vacts, call_point_uid, implFlow)
        (ret, nenv update_values fcenv)

      }
      else
        throw new EvaluationException("Could not find the function named %s." format name)

    def applyNativeCall(env: EvEnv, name: String, actuals: List[Expr], call_point_uid: Uid, implFlow: CADInfo): (Option[ValueWAbstr], EvEnv) = {
      val (vacts, nenv) = evaluateActuals(env, actuals, implFlow)
      (Some((functConvert.applyNative(name, vacts), CADInfoFactory.star.join(implFlow))), nenv) //@TODO: check correctness of implicit
    }

    def evaluateActuals(env: EvEnv, actuals: List[Expr], implFlow: CADInfo): (List[ValueWAbstr], EvEnv) =
      actuals.foldLeft((List[ValueWAbstr](), env)) {
        case ((others, env), expr) =>
          val (v, nenv) = evaluateExpr(env, expr, implFlow)
          (others ++ List(v), nenv)
      }

    def evaluateExpr(env: EvEnv, expr: Expr, implFlow: CADInfo): (ValueWAbstr, EvEnv) =
      expr match {
        case EVariable(x) =>
          (env.lookup(x), env)
        case EBExpr(op, l, r) =>
          val (lv, nenv) = evaluateExpr(env, l, implFlow)
          val (rv, fenv) = evaluateExpr(nenv, r, implFlow)
          try
            ((evaluateBinOp(op, lv, rv, implFlow), fenv))
          catch {
            case EvaluationException(_) =>
              throw new EvaluationException("The evaluation of the binary expression has wrong arguments type at %s" format expr.loc) //%d,%d" format (expr.loc.line, expr.loc.column))
          }
        case EUExpr(op, e) =>
          val (v, nenv) = evaluateExpr(env, e, implFlow)
          try
            ((evaluateUnOp(op, v, implFlow), nenv))
          catch {
            case EvaluationException(_) =>
              throw new EvaluationException("The evaluation of the unary expression has wrong arguments type at %s" format expr.loc)
          }
        case ecall @ ECall(name, actuals) => //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyCall(env, name, actuals, ecall.uid, implFlow) match {
            case (None, _)                     => throw new EvaluationException("The function %s is void so it cannot be used in an expression call at %s" format (name, expr.loc))
            case (Some(ret: ValueWAbstr), env) => (ret, env)
          }
        case ecall @ ENativeCall(name, actuals) =>  //FIXME: change signature to applycall to forward all none, and not just name, actuals and uid...
          applyCall(env, name, actuals, ecall.uid, implFlow) match {
            case (None, _)                     => throw new EvaluationException("The function %s is void so it cannot be used in an expression call at %s" format (name, expr.loc))
            case (Some(ret: ValueWAbstr), env) => (ret, env)
          }
        case ELit(IntLit(v))            => ((IntValue(v), CADInfoFactory.star.join(implFlow)), env) //@TODO: check correctness of implicit
        case ELit(BoolLit(v))           => ((BoolValue(v), CADInfoFactory.star.join(implFlow)), env) //@TODO: check correctness of implicit
        case ELit(StringLit(v))         => ((StringValue(v), CADInfoFactory.star.join(implFlow)), env) //@TODO: check correctness of implicit
        case ELit(NullLit)              => throw new NotSupportedException("Expression \"null\" not supported at %O", expr.loc)
        case ENew(_, _)                 => throw new NotSupportedException("Expression New not supported at %O", expr.loc)
        case EThis                      => throw new NotSupportedException("Expression This not supported at %O", expr.loc)
        case EMethodCall(_, _)          => throw new NotSupportedException("Expression Method Call not supported at %O", expr.loc)
        case EGetField(_)               => throw new NotSupportedException("Get Field Expression not supported at %O", expr.loc)
      }

    // Binary operation evaluation. Return the value + the label
    def evaluateBinOp(op: BOperator, lv: ValueWAbstr, rv: ValueWAbstr, implFlow: CADInfo): ValueWAbstr = {
      val res =
        (lv._1, rv._1) match {
          case (IntValue(l), IntValue(r)) =>
            op match {
              case BOPlus(ann)  => IntValue(l + r)
              case BOMinus(ann) => IntValue(l - r)
              case BOMul(ann)   => IntValue(l * r)
              case BODiv(ann)   => IntValue(l / r)
              case BOMod(ann)   => IntValue(l % r)
              case BOEq(ann)    => BoolValue(l == r)
              case BONeq(ann)   => BoolValue(l != r)
              case BOLt(ann)    => BoolValue(l < r)
              case BOLeq(ann)   => BoolValue(l <= r)
              case BOGt(ann)    => BoolValue(l > r)
              case BOGeq(ann)   => BoolValue(l >= r)
              case _                 => throw new EvaluationException("Type mismatch on binary operation")
            }
          case (StringValue(l), StringValue(r)) =>
            op match {
              case BOPlusPlus(ann) => StringValue(l + r)
              case BOEq(ann)       => BoolValue(l == r)
              case BONeq(ann)      => BoolValue(l != r)
              case BOLt(ann)       => BoolValue(l < r)
              case BOLeq(ann)      => BoolValue(l <= r)
              case BOGt(ann)       => BoolValue(l > r)
              case BOGeq(ann)      => BoolValue(l >= r)
              case _                    => throw new EvaluationException("Type mismatch on binary operation")
            }
          case (BoolValue(l), BoolValue(r)) =>
            op match {
              case BOAnd(ann) => BoolValue(l && r)
              case BOOr(ann)  => BoolValue(l || r)
              case BOEq(ann)  => BoolValue(l == r)
              case BONeq(ann) => BoolValue(l != r)
              case _               => throw new EvaluationException("Type mismatch on binary operation")
            }
          case _ => throw new EvaluationException("Type mismatch on binary operation")
        }
      // @FIXME: fix theUid; List(lv._1, rv._1) contains ConcreteValue, but we need abstract!
      (res, lv._2.update(op.annot, op.uid, (null, null)/*(lv._1, rv._1)*/, rv._2).join(implFlow)) //@TODO: check correctness of implicit
    }

    // Unary operation evaluation. Return the value + the label
    def evaluateUnOp(op: UOperator, v: ValueWAbstr, implFlow: CADInfo): ValueWAbstr = {
      v match {
        case (IntValue(i), lab) =>
          op match {
            // @FIXME: fix theUid; v._1 is a ConcreteValue, but we need abstract!
            case UNeg(ann) => (IntValue(-i), lab.update(ann, op.uid, null /*v._1*/).join(implFlow)) //@TODO: check correctness of implicit
            case _ => throw new EvaluationException("Type mismatch on unary operation")
          }
        case (BoolValue(b), lab) =>
          op match {
            // @FIXME: fix theUid; v._1 is a ConcreteValue, but we need abstract!
            case UNot(ann) => (BoolValue(!b), lab.update(ann, op.uid, null /*v._1*/).join(implFlow)) //@TODO: check correctness of implicit
            case _ => throw new EvaluationException("Type mismatch on unary operation")
          }
        case _ => throw new EvaluationException("Type mismatch on unary operation")
      }
    }
  }
}
*/

  /*
  file: functConvert

  package it.unive.dais.yaasa

/**
 * @author gbarbon
 */

import java.security.MessageDigest
import it.unive.dais.yaasa.analyzer._
import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.abstract_types._
import it.unive.dais.yaasa.datatype.FortyTwo.BitQuantity

/**
 * It contains functions conversion from the tiny java to scala
 */
object functConvert {

  def applyNative(name: String, actuals: List[ValueWAbstr]): ConcreteValue = {
    val res = name match {
      //stdlib functions
      case "encrypt" => actuals match {
        case List((StringValue(lab), _), (StringValue(key), _)) => stdlib.encrypt(lab, key)
        case _ => throw new EvaluationException("encrypt function arguments not matched")
      }
      case "substring" => actuals match {
        case List((StringValue(str), _), (IntValue(beg), _), (IntValue(end), _)) => stdlib.substring(str, beg, end)
        case _ => throw new EvaluationException("substring function arguments not matched")
      }
      case "hash" => actuals match {
        case List((StringValue(str), _)) => stdlib.hash(str)
        case _                           => throw new EvaluationException("hash function arguments not matched")
      }
      case "checkpwd" => actuals match {
        case List((StringValue(first), _), (StringValue(second), _)) => stdlib.checkpwd(first, second)
        case _ => throw new EvaluationException("checkpwd function arguments not matched")
      }
      case "intToString" => actuals match {
        case List((IntValue(v), _)) => stdlib.intToString(v)
        case _                      => throw new EvaluationException("intToString function arguments not matched")
      }
      case "boolToString" => actuals match {
        case List((BoolValue(v), _)) => stdlib.boolToString(v)
        case _                       => throw new EvaluationException("boolToString function arguments not matched")
      }
      case "strToInt" => actuals match {
        case List((StringValue(v), _)) => stdlib.strToInt(v)
        case _                         => throw new EvaluationException("strToInt function arguments not matched")
      }
      case "strToBool" => actuals match {
        case List((StringValue(v), _)) => stdlib.strToBool(v)
        case _                         => throw new EvaluationException("strToBool function arguments not matched")
      }
      case "length" => actuals match {
        case List((StringValue(v), _)) => stdlib.length(v)
        case _                         => throw new EvaluationException("length function arguments not matched")
      }
      case "log" => actuals match {
        case List((StringValue(v), _)) => stdlib.log(v)
        case _                         => throw new EvaluationException("log function arguments not matched")
      }

      //readlib functions
      case "readString" => actuals match {
        case List((StringValue(str), _)) => readlib.readString(str)
        case _                           => throw new EvaluationException("readString function arguments not matched")
      }
      case "readInt" => actuals match {
        case List((StringValue(str), _)) => readlib.readInt(str)
        case _                           => throw new EvaluationException("readInt function arguments not matched")
      }
      case "readBool" => actuals match {
        case List((StringValue(str), _)) => readlib.readBool(str)
        case _                           => throw new EvaluationException("readBool function arguments not matched")
      }
      case "readIMEI" => readlib.readIMEI
      case "readUsrPwd" => actuals match {
        case List((StringValue(str), _)) => readlib.readUsrPwd(str)
        case _                           => throw new EvaluationException("readUsrPwd function arguments not matched")
      }
      case "readGeoLoc" => readlib.readGeoLoc
      case "readPhoneNum" => actuals match {
        case List((StringValue(str), _)) => readlib.readPhoneNum(str)
        case _                           => throw new EvaluationException("readPhoneNum function arguments not matched")
      }
      case "strInput"  => readlib.strInput
      case "boolInput" => readlib.boolInput
      case "intInput"  => readlib.intInput
      case _           => throw new EvaluationException("unrecognized native function")
    }
    res match {
      case v: Int     => IntValue(v)
      case v: String  => StringValue(v)
      case v: Boolean => BoolValue(v)
      case _          => throw new EvaluationException("Unrecognized type")
      //@FIXME: problems may arise with functions without return value!
    }
  }

  /**
   * It replicates the tiny java stdlib (in resources)
   */
  object stdlib {

    /**
     * It encrypts the label with a give key
     * Notice: DUMMY ENCRYPTION!!!
     * @param label
     * @param key the encryption key
     * @return the encrypted label
     */
    def encrypt(label: String, key: String): String = label.concat(key)

    /**
     * Substring
     * @param str
     * @param beginChar
     * @param endChar
     * @return the result string
     */
    def substring(str: String, beginChar: Int, endChar: Int): String =
      str.drop(beginChar).take(endChar)

    /**
     * Hash function.
     * @param str input string
     * @return the hash value in Array[Byte]
     */
    def hash(str: String) =
      MessageDigest.getInstance("MD5").digest(str.getBytes)

    /**
     * Check if a password (string) is correct or not (string compare)
     * DUMMY FUNCTION
     * @param first password inserted by the user
     * @param second actual correct password
     * @return a boolean value, true if the two values are the same, false otherwise
     */
    def checkpwd(first: String, second: String): Boolean =
      (first == second)

    /**
     * It retrieves the device IMEI.
     * DUMMY IMEI
     * Actually, generates a random number of 15 digits.
     * @return the IMEI from the datastore
     */
    def getDeviceID = {
      val range = 100000000000000L to 999999999999999L
      val rnd = new scala.util.Random
      range(rnd.nextInt(range length))
    }

    /**
     * It converts an int to a string
     * @param intArg integer input argument
     * @return string
     */
    def intToString(intArg: Int): String = intArg.toString()

    /**
     * It converts a boolean to a string
     * @param boolArg boolean input argument
     * @return string
     */
    def boolToString(boolArg: Boolean): String =
      if (boolArg) "true"
      else "false"

    /**
     * It converts a string to an int
     * @param str integer input argument
     * @return int
     */
    def strToInt(str: String): Option[Int] = {
      try {
        Some(str.toInt)
      }
      catch {
        case e: Exception => None
      }
    }

    /**
     * It converts a string to a boolean
     * @param str integer input argument
     * @return int
     */
    def strToBool(str: String): Option[Boolean] = {
      try {
        Some(str.toBoolean)
      }
      catch {
        case e: Exception => None
      }
    }

    /**
     * @param str input string
     * @return the dimension in integer of a string
     */
    def length(str: String): Int = str.length()

    /**
     * It writes the argument to a log file.
     * Dummy function.
     * @param str
     */
    def log(str: String) = true
  }

  /**
   *  It replicates the tiny java readlib (in resources)
   *  @FIXME: all dummy methods, please fix with working ones!!
   */
  object readlib {

    /**
     * Read a generic string confidential label from the datastore of the device.
     * @param name the name of the label
     * @return the confidential label from the datastore (string)
     */
    def readString(name: String): String = {
      val label_content = "blabla"
      label_content
    }

    /**
     * Read a generic int confidential label from the datastore of the device.
     * @param name the name of the concrete value
     * @return the confidential label from the datastore (int)
     */
    def readInt(name: String): Int = {
      val label_content = 0
      label_content
    }

    /**
     * Read a generic boolean confidential label from the datastore of the device.
     * @param name the name of the concrete value
     * @return the confidential label from the datastore (bool)
     */
    def readBool(name: String): Boolean = {
      val label_content = true
      label_content
    }

    /**
     * Read the IMEI
     * @return the device IMEI
     */
    def readIMEI(): String = {
      var IMEI = "12345678912345"
      IMEI
    }

    /**
     * Read the password
     * @param name the name of the user
     * @return the password
     */
    def readUsrPwd(usr: String): String = {
      val pwd = usr + "pwd"
      pwd
    }

    /**
     * Read the geographic position of the device
     * @return the geographic coordinates of the devices
     */
    def readGeoLoc(): String = {
      val coords = ""
      coords
    }

    /**
     * Read the given contact from the address book
     * @return the geographic coordinates of the devices
     */
    def readPhoneNum(contact: String): String = {
      val phoneNum = ""
      phoneNum
    }

    /**
     * It reads the input from the keyboard
     * @return string
     */
    def strInput = readLine()

    /**
     * It reads the input from the keyboard
     * @return bool
     */
    def boolInput = stdlib.strToBool(strInput)

    /**
     * It reads the input from the keyboard
     * @return int
     */
    def intInput = stdlib.strToInt(strInput)

  }
}

  * */

}

// Old evaluator.scala file moved to graveyard (last mod 10/nov/2015)

/*package it.unive.dais.yaasa

/**
 * @author esteffin
 */

import utils.prelude._
import utils.pretty_print._
import utils.env._
import absyn._
import scala.collection.breakOut

object evaluator {
  case class EvaluationException(_message: string) extends MessageException("Evaluation exception: %s" format _message) {
    /*def this(fmt: string, args: Any) =
      this(sprintf(fmt)(args))*/
  }
  trait ConcreteValue {
    val value: Any
    val ty: Type
  }

  type EvEnv = Env[String, ConcreteValue]

  type MethInfo = Env[String, (MethodDecl, EvEnv)]

  case class IntValue(value: Int) extends ConcreteValue {
    def this() = this(0)
    val ty = TyInt
    override def toString() = "%d" format value
  }
  case class BoolValue(value: Boolean) extends ConcreteValue {
    def this() = this(false)
    val ty = TyBool
    override def toString() = "%b" format value
  }
  case class StringValue(value: String) extends ConcreteValue {
    def this() = this(null) //Only for compatibility with the horrendous java
    val ty = TyString
    override def toString() = "%s" format value
  }
  case class UnitValue() extends ConcreteValue {
    val ty = TyType("Unit")
    val value = throw new EvaluationException("Cannot access unit value")
    override def toString() = "()"
  }

  def evaluateProgram(program: Program) =
    {
      program match {
        case Program(List()) => throw new EvaluationException("Empty class definition.")
        case Program(classes) =>
          val venv: EvEnv =
            Env(
              ((for (Class(name, _, fields, _) <- classes)
                yield createVars(fields map { case FieldDecl(ty, ns) => (ty, ns map { "%s.%s" format (name, _) }) })) flatten)toMap)
          val fenv: MethInfo =
            Env(
              (for (Class(cname, _, _, methods) <- classes; m <- methods)
                yield ("%s.%s" format (cname, m.name), (m, venv))) toMap)
          fenv search_by_key { _ endsWith ".main" } match {
            case Some(main) => evaluateCall(fenv, main, List())
            case None       => throw new EvaluationException("No main found...")
          }
      }
    }

  def evaluateClass(c: Class) =
    {
      val fieldEnv =
        new Env(createVars(c.fields map { fd => (fd.ty, fd.names) }) toMap)
      val funEnv =
        new Env(for (m <- c.methods if m.name != "main") yield (m.name, (m, fieldEnv)))

      c.methods.find { _.name == "main" } match {
        case None => throw new Unexpected("No method main in first class :%s.", c.name)
        case Some(m) =>
          {
            //val env = new Env[String, ConcreteValue]()
            evaluateCall(funEnv, (m, fieldEnv), List[ConcreteValue]()) //(env)
          }
      }
    }

  def evaluateCall(ctx: MethInfo, call: (MethodDecl, EvEnv), actuals: List[ConcreteValue]) =
    {
      val (md, env) = call
      if (md.formals.length != actuals.length)
        throw new EvaluationException("Function %s is called with wrong argument number")

      val form_bind =
        for ((form, act) <- md.formals.zip(actuals))
          yield (
          if (form.ty != act.ty)
            throw new EvaluationException("Type error in method %s: formal %s has type %s, but is given type %s at %s".format(md.name, form.ty, act.ty, md.loc))
          else
            (form.name, act))
      val (ret, fenv) = evaluateBlock(ctx, env binds_new form_bind, md.body)
      (ret, env update_values fenv)
    }

  def createVars(vars: List[(Type, List[string])]): List[(string, ConcreteValue)] =
    for ((ty, names) <- vars; name <- names)
      yield (name,
      ty match {
        case TyInt    => new IntValue()
        case TyBool   => new BoolValue()
        case TyString => new StringValue()
        case _        => throw new Unexpected("Variable %s has not supported type %s", (name, ty))
      })

  def evaluateBlock(ctx: MethInfo, env: EvEnv, block: Block): (Option[ConcreteValue], EvEnv) = //(env: Env[String, ConcreteValue]) =
    {
      val nenv: List[(id, ConcreteValue)] = createVars(block.varDecls map { vd => (vd.ty, vd.ids) })

      val (ret, fenv) = block.stmts.foldLeft(None: Option[ConcreteValue], env binds_new nenv) {
        case (ret @ (Some(_), _), stmt) => ret
        case ((None, env), stmt)        => evaluateStmt(ctx, env, stmt)
      }
      (ret, env update_values fenv)
      //evaluateStmts(env, block.stmts)
    }
  /*def evaluateStmts(env: EvEnv, stmts: List[Stmt]): (Option[ConcreteValue], EvEnv) =
    stmts match {
      case List() => (None, env)
      case (retStmt @ SReturn(r)) :: List() =>
        evaluateReturn(env, retStmt)
      case (retStmt @ SReturn(r)) :: _ =>
        //warning: to be improved...
        //throw new EvaluationException("Return should be at end of the block at %O", retStmt.loc)
        //Just ignoring further statements
        evaluateReturn(env, retStmt)
      case stmt :: stmts =>
        evaluateStmt(env, stmt) match {
          case (None, nenv) =>
            evaluateStmts(nenv, stmts)
          case ret => ret //if the statement has returned something we just stop further evaluations
        }
    }
  def evaluateReturn(env: EvEnv, retStmt: SReturn): (Option[ConcreteValue], EvEnv) =
    retStmt match {
      case SReturn(None) => (Some(UnitValue()), env)
      case SReturn(Some(e)) =>
        val (nenv, res) = evaluateExpr(env, e)
        (Some(res), nenv)
    }
    */
  def evaluateStmt(ctx: MethInfo, env: EvEnv, stmt: Stmt): (Option[ConcreteValue], EvEnv) =
    stmt match {
      case SSkip => (None, env)
      case SAssign(x, e) =>
        val (res, nenv) = evaluateExpr(ctx, env, e)
        if (res.ty != nenv.lookup(x).ty)
          throw new EvaluationException("Type error: variable %s has type %s, but is given type %s at %s".format(x, nenv.lookup(x).ty, res.ty, stmt.loc.toString()))
        else
          (None, nenv.update(x) { _ => res })
      case SIf(c, thn, els) =>
        val (cond, nenv) = evaluateExpr(ctx, env, c)
        cond match {
          case BoolValue(v) =>
            if (v)
              evaluateStmt(ctx, nenv, thn)
            else
              evaluateStmt(ctx, nenv, els)
          case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
        }
      case SWhile(c, body) =>
        val (cond, nenv) = evaluateExpr(ctx, env, c)
        cond match {
          case BoolValue(v) =>
            if (v) {
              evaluateStmt(ctx, nenv, body) match {
                case (None, wenv)       => evaluateStmt(ctx, wenv, stmt)
                case ret @ (Some(_), _) => ret
              }
            }
            else
              (None, nenv)
          case _ => throw new EvaluationException("The evaluation of the if guard is not a boolean value %s" format stmt.loc)
        }
      case SBlock(block) => evaluateBlock(ctx, env, block)
      case SReturn(None) => (Some(UnitValue()), env)
      case SReturn(Some(e)) =>
        val (res, nenv) = evaluateExpr(ctx, env, e)
        (Some(res), nenv)
      case SPrint(ln, actual) =>
        val (vactual, nenv) = evaluateExpr(ctx, env, actual)
        if (ln) println(vactual.value) else print(vactual.value)
        (None, nenv)
      case SCall(name, actuals) =>
        applyCall(ctx, env, name, actuals) match {
          case (Some(_), env) => (None, env)
          case none           => none
        }

      //case rets @ SReturn(_) => evaluateReturn(env, rets)
      case SMethodCall(_, _) => throw new NotSupportedException("Statement Method Call not supported at %s" format stmt.loc)
      case SSetField(_, _)   => throw new NotSupportedException("Set field not supported at %s" format stmt.loc)
    }

  def applyCall(ctx: MethInfo, env: EvEnv, name: String, actuals: List[Expr]) =
    ctx.search(name) match {
      case None => throw new EvaluationException("Could not find the function named %s." format (name))
      case Some((m, cenv)) =>
        val (vacts, nenv) = evaluateActuals(ctx, env, actuals)
        val (ret, fcenv) = evaluateCall(ctx, (m, cenv update_values nenv), vacts)
        (ret, nenv update_values fcenv)
    }

  def evaluateActuals(ctx: MethInfo, env: EvEnv, actuals: List[Expr]): (List[ConcreteValue], EvEnv) =
    actuals.foldLeft((List[ConcreteValue](), env)) {
      case ((others, env), expr) =>
        val (v, nenv) = evaluateExpr(ctx, env, expr)
        (others ++ List(v), nenv)
    }

  def evaluateExpr(ctx: MethInfo, env: EvEnv, expr: Expr): (ConcreteValue, EvEnv) =
    expr match {
      case EVariable(x) =>
        (env.lookup(x), env)
      case EBExpr(op, l, r) =>
        val (lv, nenv) = evaluateExpr(ctx, env, l)
        val (rv, fenv) = evaluateExpr(ctx, nenv, r)
        try
          ((evaluateBinOp(op, lv, rv), fenv))
        catch {
          case EvaluationException(_) =>
            throw new EvaluationException("The evaluation of the binary expression has wrong arguments type at %s" format expr.loc) //%d,%d" format (expr.loc.line, expr.loc.column))
        }
      case EUExpr(op, e) =>
        val (v, nenv) = evaluateExpr(ctx, env, e)
        try
          ((evaluateUnOp(op, v), nenv))
        catch {
          case EvaluationException(_) =>
            throw new EvaluationException("The evaluation of the unary expression has wrong arguments type at %s" format expr.loc)
        }
      case ECall(name, actuals) =>
        applyCall(ctx, env, name, actuals) match {
          case (None, _)        => throw new EvaluationException("The function %s is void so it cannot be used in an expression call at %s" format (name, expr.loc))
          case (Some(ret), env) => (ret, env)
        }
      case ELit(IntLit(v))    => (IntValue(v), env)
      case ELit(BoolLit(v))   => (BoolValue(v), env)
      case ELit(StringLit(v)) => (StringValue(v), env)
      case ELit(NullLit)      => throw new NotSupportedException("Expression \"null\" not supported at %O", expr.loc)
      case ENew(_, _)         => throw new NotSupportedException("Expression New not supported at %O", expr.loc)
      case EThis              => throw new NotSupportedException("Expression This not supported at %O", expr.loc)
      case EMethodCall(_, _)  => throw new NotSupportedException("Expression Method Call not supported at %O", expr.loc)
      case EGetField(_)       => throw new NotSupportedException("Get Field Expression not supported at %O", expr.loc)
    }

  def evaluateBinOp(op: BOperator, lv: ConcreteValue, rv: ConcreteValue): ConcreteValue =
    (lv, rv) match {
      case (IntValue(l), IntValue(r)) =>
        op match {
          case BOPlus(_)  => IntValue(l + r)
          case BOMinus(_) => IntValue(l - r)
          case BOMul(_)   => IntValue(l * r)
          case BODiv(_)   => IntValue(l / r)
          case BOMod(_)   => IntValue(l % r)
          case BOEq(_)    => BoolValue(l == r)
          case BONeq(_)   => BoolValue(l != r)
          case BOLt(_)    => BoolValue(l < r)
          case BOLeq(_)   => BoolValue(l <= r)
          case BOGt(_)    => BoolValue(l > r)
          case BOGeq(_)   => BoolValue(l >= r)
          case _          => throw new EvaluationException("Type mismatch on binary operation")
        }
      case (StringValue(l), StringValue(r)) =>
        op match {
          case BOPlusPlus(_) => StringValue(l + r)
          case BOEq(_)       => BoolValue(l == r)
          case BONeq(_)      => BoolValue(l != r)
          case BOLt(_)       => BoolValue(l < r)
          case BOLeq(_)      => BoolValue(l <= r)
          case BOGt(_)       => BoolValue(l > r)
          case BOGeq(_)      => BoolValue(l >= r)
          case _             => throw new EvaluationException("Type mismatch on binary operation")
        }
      case (BoolValue(l), BoolValue(r)) =>
        op match {
          case BOAnd(_) => BoolValue(l && r)
          case BOOr(_)  => BoolValue(l || r)
          case BOEq(_)  => BoolValue(l == r)
          case BONeq(_) => BoolValue(l != r)
          /*case BOLt  (_) => BoolValue(l < r)
          case BOLeq (_) => BoolValue(l <= r)
          case BOGt  (_) => BoolValue(l > r)
          case BOGeq (_) => BoolValue(l >= r)*/
          case _        => throw new EvaluationException("Type mismatch on binary operation")
        }
      case _ => throw new EvaluationException("Type mismatch on binary operation")
    }

  def evaluateUnOp(op: UOperator, v: ConcreteValue): ConcreteValue =
    v match {
      case IntValue(i) =>
        op match {
          case UNeg(_) => IntValue(-i)
          case _       => throw new EvaluationException("Type mismatch on unary operation")
        }
      case BoolValue(b) =>
        op match {
          case UNot(_) => BoolValue(!b)
          case _       => throw new EvaluationException("Type mismatch on unary operation")
        }
      case _ => throw new EvaluationException("Type mismatch on unary operation")
    }


    // *** OLD UPDATE METHODS FROM CADINFO ***
        def update(updateType: UpdateType, ann: FunAnnot, pos: Uid, aVal: AbstractValue): ADInfo[FunAnnot, Uid, AbstractValue] = {
        //println("DEBUG: *** updt single ADINFO ***")
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) =>
              acc updated (key, entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), aVal))
              // @FIXME: cast abstracValue to abstractDegradationValue still missing

              // @TODO: remove following part when sure
              /**
                * updateType match {
                * case UpdateType.All =>
                * acc updated (key, entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), aVal))
                * case UpdateType.OverApp =>
                * acc updated (key, entry.addOExpStm(FlowElement(ann, key)).addOExplDegr(DegrElement(ann, pos), aVal))
                * case UpdateType.UnderApp =>
                * acc updated (key, entry.addUExpStm(FlowElement(ann, key)).addUExplDegr(DegrElement(ann, pos), aVal))
                * case _ => throw new WrongUpdateClass("Update type is not recognized")
                * }**/
          }
        new SetADInfo(newMap)
      }

      /**
       * check if label in B exist in A
       * if true
       *    update with statement (op, label) all label of set A
       *    update with statement (op, label) all label of set B
       * else
       *    retrieve all label names in A
       *    retrieve all label names in B
       *    create new adexp A+B: join
       *    update all A with stm (op, Li) for every i that belongs to B
       *    update all B with stm (op, Lj) for every J that belongs to A
       */
      def update(updateType: UpdateType, ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] = {
        var newMap = Map[Label, Entry]()
        val otherADInfo = anADExp match {
          case x: SetADInfo => x
          case _            => throw new ClassCastException
        }
        //println("premap: %s" format newMap)
        //println("DEBUG: *** upd two ADINFO: 1st ADINFO ***")
        theMap.foreach {
          case (key, entry) =>
            //println("DEBUG: updating label " + key)
            otherADInfo.getLabels.foreach(lab => {
              // @FIXME: cast abstracValue to abstractDegradationValue still missing
              //println("DEBUG: inserting tuple (" + ann + ", " + lab  + " ")
              newMap = newMap updated (key, entry.addExpStm(FlowElement(ann, lab)).addExplDegr(DegrElement(ann, pos), Vals._1))
              //println("DEBUG: the updated entry (1st ADINFO) is " + newMap(key))

              //
              /**updateType match {
                * case UpdateType.All =>
                * println("DEBUG: inserting tuple (" + ann + ", " + lab  + " ")
                * newMap = newMap updated (key, entry.addExpStm(FlowElement(ann, lab)).addExplDegr(DegrElement(ann, pos), Vals._1))
                * case UpdateType.OverApp =>
                * newMap = newMap updated (key, entry.addOExpStm(FlowElement(ann, lab)).addOExplDegr(DegrElement(ann, pos), Vals._1))
                * case UpdateType.UnderApp =>
                * newMap = newMap updated (key, entry.addUExpStm(FlowElement(ann, lab)).addUExplDegr(DegrElement(ann, pos), Vals._1))
                * case _ => throw new WrongUpdateClass("Update type is not recognized")
                * }**/
            })
        }
        //println("DEBUG: confirmation, printing newMap: "+newMap)
        //println("midmap: %s" format newMap)
        //println("DEBUG: *** upd two ADINFO: 2nd ADINFO ***")
        otherADInfo.getLabels.foreach {
          lab =>
            {
              //println("DEBUG: updating label " + lab)
              val entry = otherADInfo.getEntry(lab)
              //val entry = Entry(otherADInfo.getExplFlow(lab)._1, otherADInfo.getExplFlow(lab)._2, otherADInfo.getImplFlow(lab)._1, otherADInfo.getImplFlow(lab)._2, otherADInfo.getExplDegr(lab)._1, otherADInfo.getExplDegr(lab)._2, otherADInfo.getImplDegr(lab)._1, otherADInfo.getImplDegr(lab)._2)
              theMap.foreach {
                case (key, _) =>
                  // @FIXME: cast abstracValue to abstractDegradationValue still missing
                  //println("DEBUG: inserting tuple: (" + ann + ", " + key  + ")")
                  val newentry: Entry = entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), Vals._2)
                  //println("DEBUG: the updated entry (2nd ADINFO) is " + newentry)

                  //val myentry: (Label, Entry) = (lab, entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), Vals._2))
                    /**updateType match {
                      * case UpdateType.All =>
                      * println("DEBUG: inserting tuple (" + ann + ", " + key  + " ")
                      * (lab, entry.addExpStm(FlowElement(ann, key)).addExplDegr(DegrElement(ann, pos), Vals._2))
                      * case UpdateType.OverApp =>
                      * (lab, entry.addOExpStm(FlowElement(ann, key)).addOExplDegr(DegrElement(ann, pos), Vals._2))
                      * case UpdateType.UnderApp =>
                      * (lab, entry.addUExpStm(FlowElement(ann, key)).addUExplDegr(DegrElement(ann, pos), Vals._2))
                      * case _ => throw new WrongUpdateClass("Update type is not recognized")
                      * }**/
                  if (newMap.keys.exists {_ == lab}) {
                    //println("DEBUG: SONO QUI!!! ****")
                    //println("DEBUG: newMap era: " + newMap)
                    newMap = newMap.updated(lab, newentry join newMap(lab))
                    //println("DEBUG: newMap ORA E': " +  newMap)
                  }
                  else
                    newMap = newMap.updated(lab, newentry)
              }
            }
        }
        //println("newmap: %s" format newMap)
        new SetADInfo(newMap)
      }

      def newSize(ann: LabelAnnot) = {
        val newMap =
          theMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) => acc updated (key, entry.createSize(ann))
          }
        new SetADInfo(newMap)
      }

      // *** STUFF FROM CADINFO ***

      // this was inside entry
      def addOExpStm(stm: FlowElement) = this.copy(oExplStm = oExplStm + stm)
      def addUExpStm(stm: FlowElement) = this.copy(uExplStm = uExplStm + stm)
      def addOExplDegr(stm: DegrElement, theVal: AbstractValue) = {
        if (oExplDegr contains stm) {
          val prev_el: DegrAttrib = oExplDegr(stm)
          //println("DEBUG: Over iters for stm "+ stm +" was: " + prev_el.iters + "now is: " + prev_el.iters.incr + ". Value is: " + (theVal join prev_el.abstrVal))
          val res = this.copy(oExplDegr = oExplDegr updated(stm, DegrAttrib(theVal join prev_el.abstrVal, prev_el.iters.incr)))
          //println("DEBUG: now is " +  res.oExplDegr(stm))
          res
        }
        else {
          //println("DEBUG: Over iters for stm " + stm + "not found, creating new")
          this.copy(oExplDegr = oExplDegr + (stm -> DegrAttrib(theVal, Iterations.oneIter)))
        }
      }
      def addUExplDegr(stm: DegrElement, theVal: AbstractValue) = {
        if (uExplDegr contains stm) {
          val prev_el: DegrAttrib = uExplDegr(stm)
          //println("DEBUG: Under iters for stm "+ stm +" is " + prev_el.iters.incr)
          this.copy(uExplDegr = uExplDegr updated(stm, DegrAttrib(theVal join prev_el.abstrVal, prev_el.iters.incr)))
        }
        else {
          //println("DEBUG: Under iters for stm " + stm + "not found, creating new")
          this.copy(uExplDegr = uExplDegr + (stm -> DegrAttrib(theVal, Iterations.oneIter)))
        }
      }
      def addExpStm(stm: FlowElement) = this.copy(oExplStm = oExplStm + stm, uExplStm = uExplStm + stm)
      def addExplDegr(stm: DegrElement, theVal: AbstractValue) = this.addOExplDegr(stm, theVal).addUExplDegr(stm, theVal)

      // this was inside SetADInfo
            /**
        * private def getExplFlow(lab: Label): (Set[FlowElement], Set[FlowElement]) =
        * if (theMap contains lab)
        * (theMap(lab).oExplStm, theMap(lab).uExplStm)
        * else
        * (Set[FlowElement](), Set[FlowElement]())

        * private def getImplFlow(lab: Label): (Set[FlowElement], Set[FlowElement]) =
        * if (theMap contains lab)
        * (theMap(lab).oImplStm, theMap(lab).uImplStm)
        * else
        * (Set[FlowElement](), Set[FlowElement]())

        * private def getExplDegr(lab: Label): (Map[DegrElement, DegrAttrib], Map[DegrElement, DegrAttrib]) =
        * if (theMap contains lab)
        * (theMap(lab).oExplDegr, theMap(lab).uExplDegr)
        * else
        * (Map[DegrElement, DegrAttrib](), Map[DegrElement, DegrAttrib]())

        * private def getImplDegr(lab: Label): (Map[DegrElement, DegrAttrib], Map[DegrElement, DegrAttrib]) =
        * if (theMap contains lab)
        * (theMap(lab).oImplDegr, theMap(lab).uImplDegr)
        * else
        * (Map[DegrElement, DegrAttrib](), Map[DegrElement, DegrAttrib]())

        * private def getSize(lab: Label): BitQuantity =
        * if (theMap contains lab)
        * theMap(lab).size
        * else
        * BitQuantity.empty

        * private def getRowSafe(lab: Label) =
        * if (theMap contains lab)
        * theMap(lab)
        * else
        * Entry.empty
        **/

  // *** OLD ADTYPE ***
   //the Atomic Data Interface
object ADType {

  trait UpdateType
  object UpdateType {
    case object All extends UpdateType
    case object UnderApp extends UpdateType
    case object OverApp extends UpdateType
  }

  // The Atomic Data Interface
  trait ADInfo[FunAnnot, Uid, AbstractValue] extends  pretty{
    def update(updType: UpdateType, ann: FunAnnot, pos: Uid, aVal: AbstractValue): ADInfo[FunAnnot, Uid, AbstractValue] // label from this, flow element as parameter, unary operators
    def update(updType: UpdateType, ann: FunAnnot, pos: Uid, Vals: (AbstractValue, AbstractValue), anADExp: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] // label from this, flow element as parameter, binary operators
    //def update(ann: FunAnnot, pos: Uid, Vals: List[AbstractValue], ADExps: List[ADInfo[FunAnnot, Uid, AbstractValue]]): ADInfo[FunAnnot, Uid, AbstractValue]
    // The update method with more than two values has been removed. Signature maintained as comment for possible future re-implementation.

    def asImplicit: ADInfo[FunAnnot, Uid, AbstractValue] // convert the current ADInfo to implicit only
    def join(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] //join two ADInfo, this with the argument
    def widening(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue] // widening
    def meet(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue]
    def union(anADInfo: ADInfo[FunAnnot, Uid, AbstractValue]): ADInfo[FunAnnot, Uid, AbstractValue]
  }

  trait ADInfoFactory[FunAnnot, Uid, AbstractValue,Label, LabelAnnot] {
    def fromLabelAnnot(ann: LabelAnnot): ADInfo[FunAnnot, Uid, AbstractValue]
    def newInfo(aLabel: Label): ADInfo[FunAnnot, Uid, AbstractValue] = newInfo(List(aLabel))
    def newInfo(labels: List[Label]): ADInfo[FunAnnot, Uid, AbstractValue]
    val star: ADInfo[FunAnnot, Uid, AbstractValue] //  = newInfo(List(Label.star)) //empty adexp, it contains only a star label
    val empty: ADInfo[FunAnnot, Uid, AbstractValue] // = newInfo(List()) //empty adexp, it contains only a star label
  }
}

// old asImplicit  method in class CADInfo
      def asImplicit: ADInfo[FunAnnot, Uid, AbstractValue] = {
        /**val newMap =
          explMap.foldLeft(Map.empty[Label, Entry]) {
            case (acc, (key, entry)) =>
              val newEntry = Entry(
                oImplStm = entry.oStm ++ entry.oImplStm,
                uImplStm = entry.uStm ++ entry.uImplStm,
                oImplDegr = join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, entry.oDegr , entry.oImplDegr),
                uImplDegr = join_map[DegrElement, DegrAttrib]({ case (l, r) => l join r }, entry.uDegr , entry.uImplDegr),
                size = entry.size)
              acc updated (key, newEntry)
          }*/

}*/

