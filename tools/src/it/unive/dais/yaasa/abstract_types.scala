package it.unive.dais.yaasa

import javax.print.Doc

import it.unive.dais.yaasa.datatype.ABSValue._
import it.unive.dais.yaasa.datatype.ADType.ADInfo
import it.unive.dais.yaasa.datatype.lattice.Lattice
import it.unive.dais.yaasa.datatype.widening_lattice.WideningLattice
import it.unive.dais.yaasa.exception.{AbsValuesMismatch, EvaluationException}
import it.unive.dais.yaasa.utils.pretty_doc.{pretty_doc, prettySet, prettyBaseSet, prettyPair, prettyHGenSeq}
import org.kiama.output.PrettyPrinter._
import utils.pretty_print
import utils.prelude._

/**
 * @author esteffin
 */
object abstract_types {

  private[abstract_types] class BoolAt private[abstract_types] (private val value: Set[Boolean]) extends pretty_doc {

    override def equals(o: Any) = o match {
      case that: BoolAt => that.value == this.value
      case _ => false
    }
    override def hashCode = BoolAt.hashCode + value.hashCode

    private def copy(): BoolAt = new BoolAt(value)

    override def pretty_doc = prettyBaseSet(value)

    override def pretty = pretty_print.prettySet(value)

    def &&^(other: BoolAt) = new BoolAt(for (x <- this.value; y <- other.value) yield x && y)

    def ||^(other: BoolAt) = new BoolAt(for (x <- this.value; y <- other.value) yield x || y)

    def ==^(other: BoolAt) = new BoolAt(for (x <- this.value; y <- other.value) yield x == y)

    def !=^(other: BoolAt) = new BoolAt(for (x <- this.value; y <- other.value) yield x != y)

    def notAt = new BoolAt(for (x <- this.value) yield !x)

    def boolToString: StringAt = {
      if (this.value.isEmpty) StringAt.bottom
      else if (this.value.size == 1) StringAt.fromString(this.value.head.toString)
      else StringAt.top

    }

    def containsFalse: Boolean = value contains false
    def containsTrue: Boolean = value contains true

    def <==(r: BoolAt): Boolean = this.value subsetOf r.value

    def join(r: BoolAt): BoolAt = new BoolAt(this.value ++ r.value)

    def meet(r: BoolAt): BoolAt = new BoolAt(this.value intersect r.value)
    def widening(r: BoolAt): BoolAt =
      if (this == r) this
      else BoolAt.top
  }
  private[abstract_types] object BoolAt {

    def fromBool(value: Boolean): BoolAt = new BoolAt(Set(value))
    def sFalseAt: BoolAt = fromBool(false)
    def sTrueAt: BoolAt = fromBool(true)

    def bottom: BoolAt = new BoolAt(Set.empty[Boolean])
    def top: BoolAt = new BoolAt(Set(true, false))
  }

  class AbstractBoolWrapper private[abstract_types](content: BoolAt) extends AbsBoolean {

    override def &&^(r: AbsBoolean): AbstractBoolWrapper =
      r match {
        case b: AbstractBoolWrapper =>
          new AbstractBoolWrapper(content &&^ b.content)
        case _ => throw new AbsValuesMismatch("Argument should have type BoolAt, but it has not.")
      }
    override def ==^(r: AbsBoolean): AbstractBoolWrapper =
      r match {
        case b: AbstractBoolWrapper =>
          new AbstractBoolWrapper(content ==^ b.content)
        case _ => throw new AbsValuesMismatch("Argument should have type BoolAt, but it has not.")
      }
    override def ||^(r: AbsBoolean): AbstractBoolWrapper =
      r match {
        case b: AbstractBoolWrapper =>
          new AbstractBoolWrapper(content ||^ b.content)
        case _ => throw new AbsValuesMismatch("Argument should have type BoolAt, but it has not.")
      }
    override def !=^(r: AbsBoolean): AbstractBoolWrapper =
      r match {
        case b: AbstractBoolWrapper =>
          new AbstractBoolWrapper(content !=^ b.content)
        case _ => throw new AbsValuesMismatch("Argument should have type BoolAt, but it has not.")
      }
    override def notAt: AbsBoolean = new AbstractBoolWrapper(content.notAt)

    override def containsFalse: Boolean = content.containsFalse
    override def containsTrue: Boolean = content.containsTrue

    override def pretty_doc = content.pretty_doc

    override def pretty: String = content.pretty

    override def toStringAt: AbsString = new AbstractStringWrapper(content.boolToString)

    //Note: <==, join, meet, widening are inherited by WideningLattice

    override def <==(r: Lattice): Boolean = {
      r match {
        case b: AbstractBoolWrapper => content <== b.content
        case _ => throw new AbsValuesMismatch("Argument should have type BoolAt, but does not.")
      }
    }
    override def join(r: Lattice): AbsBoolean = {
      r match {
        case b: AbstractBoolWrapper => new AbstractBoolWrapper(content join b.content)
        case _ => throw new AbsValuesMismatch("Argument should have type BoolAt, but does not.")
      }
    }
    override def meet(r: Lattice): AbsBoolean = {
      r match {
        case b: AbstractBoolWrapper => new AbstractBoolWrapper(content meet b.content)
        case _ => throw new AbsValuesMismatch("Argument should have type BoolAt, but does not.")
      }
    }
    override def widening(r: WideningLattice): AbsBoolean = {
      r match {
        case b: AbstractBoolWrapper => new AbstractBoolWrapper(content widening b.content)
        case _ => throw new AbsValuesMismatch("Argument should have type BoolAt, but does not.")
      }
    }
  }
  type AbstractBool = AbsBoolean
  object AbstractBoolFactory extends AbsBooleanFactory {
    override def fromBool(value: Boolean): AbsBoolean = new AbstractBoolWrapper(BoolAt.fromBool(value))
    override def sFalseAt: AbsBoolean = new AbstractBoolWrapper(BoolAt.sFalseAt)
    override def sTrueAt: AbsBoolean = new AbstractBoolWrapper(BoolAt.sTrueAt)

    override def default: AbsBoolean = AbstractBoolFactory.sFalseAt

    override def bottom: AbsBoolean = new AbstractBoolWrapper(BoolAt.bottom)
    override def top: AbsBoolean = new AbstractBoolWrapper(BoolAt.top)
  }



  private[abstract_types] class NumAt private[abstract_types] (private val value: lib_intervals.itv.itv_t) extends pretty_doc {

    import lib_intervals.itv._

    override def pretty_doc = itv_sprint(value)
    override def pretty = itv_sprint(value)
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

    def toStringAt: StringAt = {
      if (itv_is_bottom(this.value)) StringAt.bottom
      else if (itv_is_point(this.value)) StringAt.fromString(itv_get_left(this.value).toString)
      else StringAt.top
    }

    def <==(y: NumAt): Boolean =
      //FIXME: Fatto dopo mezzanotte... UNSTABLE!!!
      lib_intervals.itv.itv_contains(y.value, this.value)
    def meet(y: NumAt): NumAt = new NumAt(lib_intervals.itv.itv_meet(this.value, y.value)._2)
    def join(y: NumAt): NumAt = new NumAt(lib_intervals.itv.itv_join(this.value, y.value))
    def widening(y: NumAt): NumAt = new NumAt(itv_widening(this.value, y.value))

    def isBottom: Boolean = itv_is_bottom(this.value)
    def isTop: Boolean = itv_is_top(this.value)
    def isPoint: Boolean = itv_is_point(this.value)
    def isOpenLeft: Boolean = itv_is_open_left(this.value)
    def isOpenRight: Boolean = itv_is_open_right(this.value)
    def getLeft: Int = itv_get_left(this.value)
    def getRight: Int = itv_get_right(this.value)

    def contains(x: Int): Boolean = itv_contains(this.value, x)

    override def equals(o: Any) = o match {
      case that: NumAt => itv_is_eq(value, that.value)
      case _ => false
    }

    override def hashCode = NumAt.hashCode + value.hashCode

    //TODO: Maybe add other comparison here
  }
  private[abstract_types] object NumAt {
    import lib_intervals.itv._

    def fromNum(b: Int): NumAt = new NumAt(itv_t.point(b))
    def interval(a: Int, b: Int): NumAt = {
      if (a > b)
        println/*throw new EvaluationException*/("Interval bounds are worng. %d should be greather than %d" format (a, b))
      new NumAt(itv_t.interval(a, b))
    }
    def open_left(a: Int): NumAt = new NumAt(itv_t.open_left(a))
    def open_right(b: Int): NumAt = new NumAt(itv_t.open_right(b))
    def top: NumAt = new NumAt(itv_t.top)
    def bottom: NumAt = new NumAt(itv_t.bottom)
  }

  class AbstractNumWrapper private[abstract_types](content: NumAt) extends AbsNum {
    private[abstract_types] val ctx: NumAt = content

    override def +^(r: AbsNum): AbsNum =
      r match {
        case n: AbstractNumWrapper => new AbstractNumWrapper(content +^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def -^(r: AbsNum): AbsNum =
      r match {
        case n: AbstractNumWrapper => new AbstractNumWrapper(content -^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def *^(r: AbsNum): AbsNum =
      r match {
        case n: AbstractNumWrapper => new AbstractNumWrapper(content *^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def /^(r: AbsNum): AbsNum =
      r match {
        case n: AbstractNumWrapper => new AbstractNumWrapper(content /^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def %^(r: AbsNum): AbsNum =
      r match {
        case n: AbstractNumWrapper => new AbstractNumWrapper(content %^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def negAt: AbsNum = new AbstractNumWrapper(content.negAt)

    override def ==^(r: AbsNum): AbsBoolean =
      r match {
        case n: AbstractNumWrapper => new AbstractBoolWrapper(content ==^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def !=^(r: AbsNum): AbsBoolean =
      r match {
        case n: AbstractNumWrapper => new AbstractBoolWrapper(content !=^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def <^(r: AbsNum): AbsBoolean =
      r match {
        case n: AbstractNumWrapper => new AbstractBoolWrapper(content <^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def <=^(r: AbsNum): AbsBoolean =
      r match {
        case n: AbstractNumWrapper => new AbstractBoolWrapper(content <=^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def >^(r: AbsNum): AbsBoolean =
      r match {
        case n: AbstractNumWrapper => new AbstractBoolWrapper(content >^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    override def >=^(r: AbsNum): AbsBoolean =
      r match {
        case n: AbstractNumWrapper => new AbstractBoolWrapper(content >=^ n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }

    override def toStringAt: AbsString = new AbstractStringWrapper(content.toStringAt)

    override def pretty_doc = content.pretty_doc
    override def pretty: String = content.pretty

    //Note: <==, join, meet, widening are inherited by WideningLattice
    override def <==(sndVal: Lattice): Boolean = {
      sndVal match {
        case n: AbstractNumWrapper => content <== n.content
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    }
    override def join(sndVal: Lattice): AbsNum = {
      sndVal match {
        case n: AbstractNumWrapper => new AbstractNumWrapper(content join n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    }
    override def meet(sndVal: Lattice): AbsNum = {
      sndVal match {
        case n: AbstractNumWrapper => new AbstractNumWrapper(content meet n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    }
    override def widening(sndVal: WideningLattice): AbsNum = {
      sndVal match {
        case n: AbstractNumWrapper => new AbstractNumWrapper(content widening n.content)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but has not it.")
      }
    }
  }
  type AbstractNum = AbsNum
  object AbstractNumFactory extends AbsNumFactory {
    override def fromNum(value: Int): AbsNum = new AbstractNumWrapper(NumAt.fromNum(value))
    override def open_right(left: Int): AbsNum = new AbstractNumWrapper(NumAt.open_right(left))
    override def open_left(right: Int): AbsNum = new AbstractNumWrapper(NumAt.open_left(right))
    override def interval(left: Int, right: Int): AbsNum = new AbstractNumWrapper(NumAt.interval(left, right))

    override def default: AbsNum = AbstractNumFactory.fromNum(0)

    override def bottom: AbsNum = new AbstractNumWrapper(NumAt.bottom)
    override def top: AbsNum = new AbstractNumWrapper(NumAt.top)
  }



  object StringAtImpl {

    private[abstract_types] class StringAt private[abstract_types] (private val values: Set[StrVal]) extends pretty_doc {
      override def equals(o: Any) = o match {
        case that: StringAt => that.values == this.values
        case _ => false
      }

      override def hashCode = StringAt.hashCode + values.hashCode

      private def normalize_set(vals: Set[StrVal]): Set[StrVal] = {
        /*for (strat <- vals){
          val vs = vals.filter( _ != strat)
          val ex = !vs.exists(strat <== _)
          println("%s %s %s" format (prettySet(vs), ex, strat))
        }*/
        val cnt =
          for (strat <- vals; if !vals.filter( _!= strat).exists(strat <== _))
            yield strat
        cnt
      }
      private def normalize = {
        new StringAt(normalize_set(values))
      }
      def ++^(other: StringAt): StringAt = {
        val new_cnt = for (s1 <- this.values; s2 <- other.values) yield s1 ++^ s2
        new StringAt(normalize_set(new_cnt))
      }
      def ==^(other: StringAt): BoolAt = {
        val res = for (s1 <- this.values; s2 <- other.values) yield s1 ==^ s2
        if (res contains BoolAt.top) BoolAt.top
        else res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }
      def !=^(other: StringAt): BoolAt = (this ==^ other).notAt
      def <^(other: StringAt): BoolAt = {
        val res = for (s1 <- this.values; s2 <- other.values) yield s1 <^ s2
        if (res contains BoolAt.top) BoolAt.top
        else res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }
      def <=^(other: StringAt): BoolAt = {
        val res = for (s1 <- this.values; s2 <- other.values) yield s1 <=^ s2
        if (res contains BoolAt.top) BoolAt.top
        else res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }
      def >^(other: StringAt): BoolAt = {
        val res = for (s1 <- this.values; s2 <- other.values) yield s1 >^ s2
        if (res contains BoolAt.top) BoolAt.top
        else res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }
      def >=^(other: StringAt): BoolAt = {
        val res = for (s1 <- this.values; s2 <- other.values) yield s1 >=^ s2
        if (res contains BoolAt.top) BoolAt.top
        else res.foldLeft(BoolAt.bottom) { (acc, v) => acc join v }
      }

      // @FIXME: move back to stdlib XD
      def encrypt(key: StringAt): StringAt = StringAt.top
      def checkpwd(pwd: StringAt): BoolAt = this ==^ pwd
      def hash: StringAt = StringAt.top
      def strToBool: BoolAt = this.values.foldLeft(BoolAt.bottom) { (acc, v) => acc join v.strToBool }
      def strToInt: NumAt = this.values.foldLeft(NumAt.bottom) { (acc, v) => acc join v.strToInt }
      def length: NumAt = this.values.foldLeft(NumAt.bottom) { (acc, v) => acc join v.length }


      def trimBefore(numVal: NumAt): StringAt = {
        //TODO: consider using different approaches than fold
        (for (s <- this.values) yield s.trimBefore(numVal)).foldLeft(StringAt.bottom) { (acc, v) => acc join v }
      }
      def trimAfter(numVal: NumAt): StringAt = {
        //TODO: consider using different approaches than fold
        (for (s <- this.values) yield s.trimAfter(numVal)).foldLeft(StringAt.bottom) { (acc, v) => acc join v }
      }

      def <==(y: StringAt): Boolean = this.values.forall( s => y.values.exists(s1 => s <== s1))
      def join(y: StringAt): StringAt = new StringAt(normalize_set(this.values ++ y.values))
      def meet(y: StringAt): StringAt = ???  //@FIXME: not implemented code
      def widening(y: StringAt): StringAt = ???  //@FIXME: not implemented code

      override def pretty_doc = prettySet(values)
      override def pretty: String = pretty_print.prettySet(values)
    }
    private[abstract_types] object StringAt {
      def fromString(value: String): StringAt = new StringAt(Set(Exact(value)))
      def top: StringAt = new StringAt(Set(StrVal.top))
      def bottom: StringAt = new StringAt(Set.empty[StrVal])
    }

    private[StringAtImpl] trait StrVal extends pretty_doc {
      def ++^(other: StrVal): StrVal = {
        this match {
          case Exact(x) => Prefix(x)
          case Prefix(x) => Prefix(x)
        }
      }
      def ==^(other: StrVal): BoolAt = {
        (this, other) match {
          case (Exact(x), Exact(y)) =>
            if (x == y) BoolAt.sTrueAt
            else BoolAt.sFalseAt
          case (Exact(x), Prefix(y)) =>
            if (x startsWith y) BoolAt.top
            else BoolAt.sFalseAt
          case (Prefix(y), Exact(x)) =>
            if (x startsWith y) BoolAt.top
            else BoolAt.sFalseAt
          case (Prefix(x), Prefix(y)) =>
            if (x.startsWith(y) || y.startsWith(x)) BoolAt.top
            else BoolAt.sFalseAt
        }
      }
      def !=^(other: StrVal): BoolAt = (this ==^ other).notAt
      def <^(other: StrVal): BoolAt = {
        (this, other) match {
          case (Exact(x), Exact(y)) => BoolAt.fromBool(x < y)
          case _ =>
            //TODO: metto sempre top perche' devo controllare le disuguaglianze su stringhe
            BoolAt.top
        }
      }
      def <=^(other: StrVal): BoolAt =  {
        (this, other) match {
          case (Exact(x), Exact(y)) => BoolAt.fromBool(x <= y)
          case _ =>
            //TODO: metto sempre top perche' devo controllare le disuguaglianze su stringhe
            BoolAt.top
        }
      }
      def >^(other: StrVal): BoolAt =  {
        (this, other) match {
          case (Exact(x), Exact(y)) => BoolAt.fromBool(x > y)
          case _ =>
            //TODO: metto sempre top perche' devo controllare le disuguaglianze su stringhe
            BoolAt.top
        }
      }
      def >=^(other: StrVal): BoolAt =  {
        (this, other) match {
          case (Exact(x), Exact(y)) => BoolAt.fromBool(x >= y)
          case _ =>
            //TODO: metto sempre top perche' devo controllare le disuguaglianze su stringhe
            BoolAt.top
        }
      }

      def hash: StrVal = StrVal.top
      def strToInt: NumAt = {
        this match {
          case Exact(x) =>
            toInt(x) match {
              case Some(v) => NumAt.fromNum(v)
              //FIXME: Bottom instead of top?
              case None => NumAt.top
            }
          //FIXME: Bottom instead of top?
          case _ => NumAt.top
        }
      }
      def strToBool: BoolAt = {
        this match {
          case Exact(x) =>
            toBool(x) match {
              case Some(v) => BoolAt.fromBool(v)
                //FIXME: Bottom instead of top?
              case None => BoolAt.top
            }
          //FIXME: Bottom instead of top?
          case _ => BoolAt.top
        }
      }
      def length: NumAt = {
        this match {
          case Exact(s) => NumAt.fromNum(s.length)
          case Prefix(p) => NumAt.open_right(p.length)
        }
      }

      private def trimBefore(n: Int): StringAt = {
        if (n < 0) StringAt.bottom
        else
          this match {
            case Exact(x) =>
              if (n > x.length) StringAt.bottom
              else new StringAt(Set(Exact(x.substring(n))))
            case Prefix(x) =>
              if (n > x.length) StringAt.top
              else new StringAt(Set(Prefix(x.substring(n))))
          }
      }
      def trimBefore(numVal: NumAt): StringAt = {
        if (numVal.isBottom) StringAt.bottom
        else if (numVal.isTop) StringAt.top
        else if (numVal.isOpenLeft) {
          if (numVal.getRight < 0) StringAt.bottom
          else if (numVal.getRight == 0) new StringAt(Set(this))
          else StringAt.top
        }
        else if (numVal.isOpenRight) {
          this match {
            case Exact(x) =>
              if (x.length < numVal.getLeft) StringAt.bottom
              else StringAt.top
            case Prefix(x) =>
              StringAt.top
          }
        }
        else if (numVal.isPoint) trimBefore(numVal.getLeft)
        else StringAt.top
      }

      private def trimAfter(n: Int): StringAt = {
        if (n < 0) StringAt.bottom
        else
          this match {
            case Exact(x) =>
              if (n > x.length) StringAt.bottom
              else new StringAt(Set(Exact(x.substring(0, n))))
            case Prefix(x) =>
              if (n > x.length) new StringAt(Set(Prefix(x)))
              else
                new StringAt(Set(Exact(x.substring(0, n))))
          }
      }
      def trimAfter(numVal: NumAt): StringAt = {
        if (numVal.isBottom) StringAt.bottom
        else if (numVal.isTop) StringAt.top
        else if (numVal.isOpenLeft) {
          if (numVal.getRight < 0) StringAt.bottom
          else if (numVal.getRight == 0) new StringAt(Set(Exact("")))
          else StringAt.top
        }
        else if (numVal.isOpenRight) {
          this match {
            case Exact(x) =>
              if (x.length < numVal.getLeft) StringAt.bottom
              else if (numVal.getLeft > 0) new StringAt(Set(Prefix(x.substring(0, numVal.getLeft))))
              else StringAt.top
            case Prefix(x) =>
              if (x.length < numVal.getLeft) new StringAt(Set(this))
              else if (numVal.getLeft > 0) new StringAt(Set(Prefix(x.substring(0, numVal.getLeft))))
              else StringAt.top
          }
        }
        else if (numVal.isPoint) trimAfter(numVal.getLeft)
        else {
          this match {
            case Exact(x) =>
              new StringAt(Set(Prefix(x.substring(0, numVal.getLeft))))
            case Prefix(x) =>
              new StringAt(Set(Prefix(x.substring(0, numVal.getLeft))))
          }
        }
      }

      def <==(other: StrVal): Boolean = {
        (this, other) match {
          case (Exact(x), Exact(y)) => x == y
          case (Prefix(x), Exact(y)) => false
          case (Exact(x), Prefix(y)) => x startsWith y
          case (Prefix(x), Prefix(y)) => x startsWith y
        }
      }
      def join(y: StrVal): StringAt = ???  //@FIXME: not implemented code
      def meet(y: StrVal): StringAt = ???  //@FIXME: not implemented code



      override def pretty: String
    }
    private[StringAtImpl] object StrVal {
      def top = Prefix("")
    }

    private[StringAtImpl] case class Exact(str: String) extends StrVal {
      override def pretty_doc = pretty
      override def pretty: String = "\"%s\"" format  str
    }
    private[StringAtImpl] case  class Prefix(prefix: String) extends StrVal {
      override def pretty_doc = pretty
      override def pretty: String = "\"%s*\"" format prefix
    }

    /*
    case class StringAt(value: String) extends pretty {

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

  private[abstract_types] type StringAt = StringAtImpl.StringAt
  private[abstract_types] val StringAt = StringAtImpl.StringAt

  class AbstractStringWrapper private[abstract_types](content : StringAt) extends AbsString {

    override def pretty_doc = content.pretty
    override def pretty: String = content.pretty

    override def ++^(r: AbsString): AbsString =
      r match {
        case s: AbstractStringWrapper => new AbstractStringWrapper(content ++^ s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    override def ==^(r: AbsString): AbsBoolean =
      r match {
        case s: AbstractStringWrapper => new AbstractBoolWrapper(content ==^ s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    override def !=^(r: AbsString): AbsBoolean =
      r match {
        case s: AbstractStringWrapper => new AbstractBoolWrapper(content !=^ s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    override def <^(r: AbsString): AbsBoolean =
      r match {
        case s: AbstractStringWrapper => new AbstractBoolWrapper(content <^ s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    override def <=^(r: AbsString): AbsBoolean =
      r match {
        case s: AbstractStringWrapper => new AbstractBoolWrapper(content <=^ s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    override def >^(r: AbsString): AbsBoolean =
      r match {
        case s: AbstractStringWrapper => new AbstractBoolWrapper(content >^ s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    override def >=^(r: AbsString): AbsBoolean =
      r match {
        case s: AbstractStringWrapper => new AbstractBoolWrapper(content >=^ s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }

    override def length: AbsNum = new AbstractNumWrapper(content.length)
    override def strToInt: AbsNum = new AbstractNumWrapper(content.strToInt)
    override def strToBool: AbsBoolean = new AbstractBoolWrapper(content.strToBool)
    override def dropUntil(r: AbstractNum): AbsString =
      r match {
        case n: AbstractNumWrapper => new AbstractStringWrapper(content trimBefore n.ctx)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    override def takeUntil(r: AbstractNum): AbsString =
      r match {
        case n: AbstractNumWrapper => new AbstractStringWrapper(content trimAfter n.ctx)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }


    override def <==(r: Lattice): Boolean = {
      r match {
        case s: AbstractStringWrapper => content <== s.content
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    }
    override def join(r: Lattice): AbsString = {
      r match {
        case s: AbstractStringWrapper => new AbstractStringWrapper(content join s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    }
    override def meet(r: Lattice): AbsString = {
      r match {
        case s: AbstractStringWrapper => new AbstractStringWrapper(content meet s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    }
    override def widening(r: WideningLattice): AbsString = {
      r match {
        case s: AbstractStringWrapper => new AbstractStringWrapper(content widening s.content)
        case _ => throw new AbsValuesMismatch("Argument should have type StringAt, but has not it.")
      }
    }

  }
  type AbstractString = AbsString
  object AbstractStringFactory extends AbsStringFactory {
    override def fromString(value: String): AbsString = new AbstractStringWrapper(StringAt.fromString(value))

    //FIXME: find a better value...
    override def default: AbsString = AbstractStringFactory.bottom

    override def bottom: AbsString = new AbstractStringWrapper(StringAt.bottom)
    override def top: AbsString = new AbstractStringWrapper(StringAt.top)
  }

  case object AbstractUnit extends TypedAbstractValue with pretty_doc {
    val ty = TyType("Unit")
    val value = throw new EvaluationException("Cannot access unit value")
    override def pretty_doc = "()"
    override def pretty = "()"

    override def <==(r: Lattice): Boolean = ???  //@FIXME: not implemented code
    override def join(r: Lattice): AbstractValue = ???  //@FIXME: not implemented code
    override def widening(r: WideningLattice): AbstractValue = ???  //@FIXME: not implemented code
    override def meet(r: Lattice): AbstractValue = ???  //@FIXME: not implemented code
  }

  private[abstract_types] case class ArrayAt
            (private val inner_type: Type,
             private val _length: Int,
             private val elements: Vector[(ValueWithAbstraction, Boolean)])
    extends pretty_doc {

    val ty: Type = TyArray(inner_type)
    def set(i: NumAt, x: ValueWithAbstraction): ArrayAt = {
      if (i.isPoint){
        //The index is exact
        val idx = i.getLeft
        if (idx < _length) {
          this.copy(elements = elements.updated(idx, (x, true)))
        }
        else {
          //ArrayIndexOutOfBoundException...
          //ArrayAtFact.bottom(inner_type)[InnerType]
          throw new ArrayIndexOutOfBoundsException()
        }
      }
      else {
        //index is not punctual... Change many and set to false their precision flag
        val elems =
          for (((v, prec), idx) <- elements.zipWithIndex)
            yield {
              if (i.contains(idx)){
                (v join x, false)
              }
              else (v, prec)
            }
        this.copy(elements = elems)
      }
    }

    def get(i: NumAt): Option[ValueWithAbstraction] = {
      if (i.isPoint) {
        //The index is exact
        val idx = i.getLeft
        if (idx < _length) {
          Some(elements(idx)._1)
        }
        else {
          //ArrayIndexOutOfBoundException...
          //ArrayAtFact.bottom(inner_type)[InnerType]
          throw new ArrayIndexOutOfBoundsException()
          None
        }
      }
      else{
        val elems =
          for (((v, prec), idx) <- elements.zipWithIndex if i.contains(idx))
          yield v

        if (elems.nonEmpty)
          Some(elems.tail.foldLeft(elems.head){ (acc, v) => acc join v })
        else
          None
      }
    }
    def length: NumAt = NumAt.fromNum(_length)

    override def pretty_doc = {
      val body = elements.map{ case (e, p) => prettyPair(e.pretty_doc, p.toString) }
      ty.pretty <> ":" <+> prettyHGenSeq({d => "[|" <> d <> "|]"}, body)
    }
  }
  object ArrayAtFact {
    def create(ty: Type, length: Int, default: ValueWithAbstraction, implicit_ctx: ADInfo): ArrayAt = {
      //FIXME: Continue here...
      throw new Unexpected("BOOM!")
      /*val elements = for (_ <- Range(0, length - 1)) yield (default.copy(implicit_ctx = default.adInfo.join(implicit_ctx)))
        ArrayAt(ty, length, Vector.empty)
     */
    }
    def empty(ty: Type): ArrayAt =
      ArrayAt(ty, 0, Vector.empty)
    def bottom(ty: Type): ArrayAt = empty(ty)
  }
}
