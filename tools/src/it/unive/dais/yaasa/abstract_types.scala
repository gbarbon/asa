package it.unive.dais.yaasa

import datatype.lattice._
import it.unive.dais.yaasa.abstract_types.StringAt.StringAt
import it.unive.dais.yaasa.datatype.ABSValue._
import it.unive.dais.yaasa.datatype.widening_lattice.WideningLattice
import utils.pretty_print._
import utils.prelude._

/**
 * @author esteffin
 */
object abstract_types {

  private[abstract_types] class BoolAt private[abstract_types] (private val value: Set[Boolean]) extends pretty {

    private def copy(): BoolAt = new BoolAt(value)

    def pretty = prettySet(value)

    def &&^(other: BoolAt) = new BoolAt(for (x <- this.value; y <- other.value) yield x && y)

    def ||^(other: BoolAt) = new BoolAt(for (x <- this.value; y <- other.value) yield x || y)

    def ==^(other: BoolAt) = new BoolAt(for (x <- this.value; y <- other.value) yield x == y)

    def !=^(other: BoolAt) = new BoolAt(for (x <- this.value; y <- other.value) yield x != y)

    def notAt = new BoolAt(for (x <- this.value) yield !x)

    def boolToString: StringAt.StringAt = ???

    def containsFalse: Boolean = value contains false
    def containsTrue: Boolean = value contains true

    def <==(r: BoolAt): Boolean = this.value subsetOf r.value

    def join(r: BoolAt): BoolAt = if (this <== r) r.copy() else this.copy()

    def meet(r: BoolAt): BoolAt = if (this <== r) this.copy() else r.copy()
    def widening(r: BoolAt): BoolAt = ???
  }
  private[abstract_types] object BoolAt {

    def fromBool(value: Boolean): BoolAt = new BoolAt(Set(value))

    def sFalseAt: BoolAt = fromBool(false)

    def sTrueAt: BoolAt = fromBool(true)

    def bottom: BoolAt = new BoolAt(Set(true, false))

    def top: BoolAt = new BoolAt(Set.empty[Boolean])
  }

  implicit def absBoolAt(l: BoolAt): AbsBoolean[BoolAt, NumAt, StringAt.StringAt] = {
    new AbsBoolean[BoolAt, NumAt, StringAt.StringAt] {

      override def &&^(r: BoolAt): BoolAt = l &&^ r
      override def ==^(r: BoolAt): BoolAt = l ==^ r
      override def ||^(r: BoolAt): BoolAt = l ||^ r
      override def !=^(r: BoolAt): BoolAt = l !=^ r
      override def notAt: BoolAt = l.notAt

      override def containsFalse: Boolean = l.containsFalse
      override def containsTrue: Boolean = l.containsTrue

      override def <==(r: BoolAt): Boolean = l <== r
      override def join(r: BoolAt): BoolAt = l join r
      override def meet(r: BoolAt): BoolAt = l join r
      override def widening(r: BoolAt): BoolAt = l widening r

      override def pretty: String = l.pretty

      override def boolToString: StringAt = l.boolToString
    }
  }
  type AbstractBool = AbsBoolean[BoolAt, NumAt, StringAt.StringAt]
  object AbstractBoolFactory extends AbsBooleanFactory[BoolAt, NumAt, StringAt.StringAt] {
     override def fromBool(value: Boolean): AbstractBool = BoolAt.fromBool(value)
     override def sFalseAt: AbstractBool = BoolAt.sFalseAt
     override def sTrueAt: AbstractBool = BoolAt.sTrueAt
     override def bottom: WideningLattice[BoolAt] = BoolAt.bottom
     override def top: WideningLattice[BoolAt] = BoolAt.top
  }



  private[abstract_types] class NumAt private[abstract_types] (private val value: lib_intervals.itv.itv_t) extends pretty {

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
  }
  private[abstract_types] object NumAt {

    import lib_intervals.itv._

    def fromNum(b: Int): NumAt = new NumAt(itv_t.point(b))

    def interval(a: Int, b: Int): NumAt = new NumAt(itv_t.interval(a, b))

    def open_left(a: Int): NumAt = new NumAt(itv_t.open_left(a))

    def open_right(b: Int): NumAt = new NumAt(itv_t.open_left(b))

    def top: NumAt = new NumAt(itv_t.top)
    def bottom: NumAt = new NumAt(itv_t.bottom)
  }

  implicit def absNumAt(l: NumAt): AbsNum[BoolAt, NumAt, StringAt.StringAt] = {
    new AbsNum[BoolAt, NumAt, StringAt.StringAt] {
      override def +^(r: NumAt): NumAt = l +^ r

      override def -^(r: NumAt): NumAt = l -^ r

      override def *^(r: NumAt): NumAt = l *^ r

      override def /^(r: NumAt): NumAt = l /^ r

      override def %^(r: NumAt): NumAt = l %^ r

      override def ==^(r: NumAt): BoolAt = l ==^ r

      override def !=^(r: NumAt): BoolAt = l !=^ r

      override def <^(r: NumAt): BoolAt = l <^ r

      override def <=^(r: NumAt): BoolAt = l <=^ r

      override def >^(r: NumAt): BoolAt = l >^ r

      override def >=^(r: NumAt): BoolAt = l >=^ r

      override def negAt: NumAt = l.negAt

      override def intToString: StringAt.StringAt = ???

      override def <==(r: NumAt): Boolean = l <== r

      override def join(r: NumAt): NumAt = l join r

      override def meet(r: NumAt): NumAt = l meet r

      override def widening(r: NumAt): NumAt = l widening r

      override def pretty: String = l.pretty
    }
  }
  type AbstractNum = AbsNum[BoolAt, NumAt, StringAt.StringAt]
  object AbstractNumFactory extends AbsNumFactory[BoolAt, NumAt, StringAt.StringAt] {
    override def fromNum(value: Int): AbsNum[BoolAt, NumAt, StringAt] = NumAt.fromNum(value)
    override def open_right(left: Int): AbsNum[BoolAt, NumAt, StringAt] = NumAt.open_right(left)
    override def open_left(right: Int): AbsNum[BoolAt, NumAt, StringAt] = NumAt.open_left(right)
    override def interval(left: Int, right: Int): AbsNum[BoolAt, NumAt, StringAt] = NumAt.interval(left, right)
    override def bottom: WideningLattice[NumAt] = NumAt.bottom
    override def top: WideningLattice[NumAt] = NumAt.top
  }



  object StringAt {

    class StringAt private[abstract_types] (private val values: Set[StrVal]) extends pretty {
      private def normalize() = {
        val cnt =
          for (strat <- values; if (!values.filter( _ == strat).exists(strat <== _)))
            yield strat
        new StringAt(cnt)
      }
      def ++^(other: StringAt): StringAt = ???
      def ==^(other: StringAt): BoolAt = ???
      def !=^(other: StringAt): BoolAt = ???
      def <^(other: StringAt): BoolAt = ???
      def <=^(other: StringAt): BoolAt = ???
      def >^(other: StringAt): BoolAt = ???
      def >=^(other: StringAt): BoolAt = ???

      def encrypt(key: StringAt): StringAt = ???
      def checkpwd(pwd: StringAt): BoolAt = ???
      def hash: StringAt = ???
      def strToInt: NumAt = ???
      def strToBool: BoolAt = ???
      def length: NumAt = ???

      def trimBegin(numVal: NumAt): StringAt = ???
      def trimEnd(numVal: NumAt): StringAt = ???

      def <==(y: StringAt): Boolean = ???
      def meet(y: StringAt): StringAt = ???
      def join(y: StringAt): StringAt = ???

      override def pretty: String = ???
    }

    private[StringAt] trait StrVal extends pretty {
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
      def <=^(other: StrVal): BoolAt = ???
      def >^(other: StrVal): BoolAt = ???
      def >=^(other: StrVal): BoolAt = ???

      //def encrypt(key: StrVal): StrAt = Prefix(*)
      def checkpwd(pwd: StrVal): BoolAt = this ==^ pwd
      def hash: StrVal = StrVal.top
      def strToInt: NumAt =
        this match {
          case Exact(x) =>
            toInt(x) match {
              case Some(v) => NumAt.fromNum(v)
              case None => NumAt.top
            }
          case _ => NumAt.top
        }
      def strToBool: BoolAt =
        this match {
          case Exact(x) =>
            toBool(x) match {
              case Some(v) => BoolAt.fromBool(v)
              case None => BoolAt.top
            }
          case _ => BoolAt.top
        }
      def length: NumAt =
        this match {
          case Exact(s) => NumAt.fromNum(s.length)
          case Prefix(p) => NumAt.open_right(p.length)
        }

      def trimBegin(numVal: NumAt): StrVal = ???
      def trimEnd(numVal: NumAt): StrVal = ???

      def <==(y: StrVal): Boolean =
        (this, y) match {
          case (Exact(x), Exact(y)) => x == y
          case (Prefix(x), Exact(y)) => false
          case (Exact(x), Prefix(y)) => x startsWith y
          case (Prefix(x), Prefix(y)) => x startsWith y
        }
      def join(y: StrVal): StringAt = ???
      def meet(y: StrVal): StringAt = ???


      override def pretty: String
    }
    object StrVal {
      def top = Prefix("")
    }

    private[StringAt] case class Exact(str: String) extends StrVal {
      override def pretty: String = "\"%s\"" format  str
    }
    private[StringAt] case  class Prefix(prefix: String) extends StrVal {
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

  implicit def absStrAt(l: NumAt): AbsString[BoolAt, NumAt, StringAt.StringAt] = {
    new AbsString[BoolAt, NumAt, StringAt.StringAt] {

      override def pretty: String = l.pretty

      override def ++^(sndVal: StringAt): StringAt = ???

      override def >=^(sndVal: StringAt): BoolAt = ???

      override def strToInt: NumAt = ???

      override def length: NumAt = ???

      override def hash: StringAt = ???

      override def ==^(sndVal: StringAt): BoolAt = ???

      override def encrypt(key: StringAt): StringAt = ???

      override def <^(sndVal: StringAt): BoolAt = ???

      override def trimEnd(numVal: NumAt): StringAt = ???

      override def >^(sndVal: StringAt): BoolAt = ???

      override def checkpwd(pwd: StringAt): StringAt = ???

      override def <=^(sndVal: StringAt): BoolAt = ???

      override def trimBegin(numVal: NumAt): StringAt = ???

      override def !=^(sndVal: StringAt): BoolAt = ???

      override def strToBool: BoolAt = ???

      override def widening(r: StringAt): StringAt = ???

      override def join(r: StringAt): StringAt = ???

      override def meet(r: StringAt): StringAt = ???

      override def <==(r: StringAt): Boolean = ???
    }
  }
  type AbstractString = AbsNum[BoolAt, NumAt, StringAt.StringAt]
  object AbstractStringFactory extends AbsStringFactory[BoolAt, NumAt, StringAt.StringAt] {
    override def fromString(value: Int): AbsString[BoolAt, NumAt, StringAt] = ???

    override def bottom: WideningLattice[StringAt] = ???

    override def top: WideningLattice[StringAt] = ???
  }
}
