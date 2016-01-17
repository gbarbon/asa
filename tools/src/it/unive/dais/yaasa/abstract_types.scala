package it.unive.dais.yaasa

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

    def boolToString: StringAt = {
      if (this.value.isEmpty) StringAt.bottom
      else if (this.value.size == 1) StringAt.fromString(this.value.head.toString)
      else StringAt.top

    }

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

  implicit def absBoolAt(l: BoolAt): AbsBoolean[BoolAt, NumAt, StringAt] = {
    new AbsBoolean[BoolAt, NumAt, StringAt] {

      override val cnt: BoolAt = l

      override def &&^(r: Wrapper[BoolAt]): BoolAt = l &&^ r.cnt
      override def ==^(r: Wrapper[BoolAt]): BoolAt = l ==^ r.cnt
      override def ||^(r: Wrapper[BoolAt]): BoolAt = l ||^ r.cnt
      override def !=^(r: Wrapper[BoolAt]): BoolAt = l !=^ r.cnt
      override def notAt: BoolAt = l.notAt

      override def containsFalse: Boolean = l.containsFalse
      override def containsTrue: Boolean = l.containsTrue

      override def <==(r: Wrapper[BoolAt]): Boolean = l <== r.cnt
      override def join(r: Wrapper[BoolAt]): BoolAt = l join r.cnt
      override def meet(r: Wrapper[BoolAt]): BoolAt = l join r.cnt
      override def widening(r: Wrapper[BoolAt]): BoolAt = l widening r.cnt

      override def pretty: String = l.pretty

      override def boolToString: StringAt = l.boolToString
    }
  }
  type AbstractBool = AbsBoolean[BoolAt, NumAt, StringAt]
  object AbstractBoolFactory extends AbsBooleanFactory[BoolAt, NumAt, StringAt] {
     override def fromBool(value: Boolean): AbstractBool = BoolAt.fromBool(value)
     override def sFalseAt: AbstractBool = BoolAt.sFalseAt
     override def sTrueAt: AbstractBool = BoolAt.sTrueAt
     override def bottom: AbstractBool = BoolAt.bottom
     override def top: AbstractBool = BoolAt.top
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

    def intToString: StringAt = {
      if (itv_is_bottom(this.value)) StringAt.bottom
      else if (itv_is_point(this.value)) StringAt.fromString(itv_get_left(this.value).toString)
      else StringAt.top
    }

    def widening(y: NumAt): NumAt = new NumAt(itv_widening(this.value, y.value))

    def <==(y: NumAt): Boolean = lib_intervals.itv.itv_contains(this.value, y.value)

    def meet(y: NumAt): NumAt = new NumAt(lib_intervals.itv.itv_meet(this.value, y.value)._2)

    def join(y: NumAt): NumAt = new NumAt(lib_intervals.itv.itv_join(this.value, y.value))

    def isBottom: Boolean = itv_is_bottom(this.value)

    def isTop: Boolean = itv_is_top(this.value)

    def isPoint: Boolean = itv_is_point(this.value)

    def isOpenLeft: Boolean = itv_is_open_left(this.value)

    def isOpenRight: Boolean = itv_is_open_right(this.value)

    def getLeft: Int = itv_get_left(this.value)

    def getRight: Int = itv_get_right(this.value)

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

  implicit def absNumAt(l: NumAt): AbsNum[BoolAt, NumAt, StringAt] = {
    new AbsNum[BoolAt, NumAt, StringAt] {

      override val cnt: NumAt = l

      override def +^(r: Wrapper[NumAt]): NumAt = l +^ r.cnt

      override def -^(r: Wrapper[NumAt]): NumAt = l -^ r.cnt

      override def *^(r: Wrapper[NumAt]): NumAt = l *^ r.cnt

      override def /^(r: Wrapper[NumAt]): NumAt = l /^ r.cnt

      override def %^(r: Wrapper[NumAt]): NumAt = l %^ r.cnt

      override def ==^(r: Wrapper[NumAt]): BoolAt = l ==^ r.cnt

      override def !=^(r: Wrapper[NumAt]): BoolAt = l !=^ r.cnt

      override def <^(r: Wrapper[NumAt]): BoolAt = l <^ r.cnt

      override def <=^(r: Wrapper[NumAt]): BoolAt = l <=^ r.cnt

      override def >^(r: Wrapper[NumAt]): BoolAt = l >^ r.cnt

      override def >=^(r: Wrapper[NumAt]): BoolAt = l >=^ r.cnt

      override def negAt: NumAt = l.negAt

      override def intToString: StringAt = l.intToString

      override def <==(r: Wrapper[NumAt]): Boolean = l <== r.cnt

      override def join(r: Wrapper[NumAt]): NumAt = l join r.cnt

      override def meet(r: Wrapper[NumAt]): NumAt = l meet r.cnt

      override def widening(r: Wrapper[NumAt]): NumAt = l widening r.cnt

      override def pretty: String = l.pretty
    }
  }
  type AbstractNum = AbsNum[BoolAt, NumAt, StringAt]
  object AbstractNumFactory extends AbsNumFactory[BoolAt, NumAt, StringAt] {
    override def fromNum(value: Int): AbstractNum = NumAt.fromNum(value)
    override def open_right(left: Int): AbstractNum = NumAt.open_right(left)
    override def open_left(right: Int): AbstractNum = NumAt.open_left(right)
    override def interval(left: Int, right: Int): AbstractNum = NumAt.interval(left, right)
    override def bottom: AbstractNum = NumAt.bottom
    override def top: AbstractNum = NumAt.top
  }



  object StringAtImpl {

    private[abstract_types] class StringAt private[abstract_types] (private val values: Set[StrVal]) extends pretty {
      override def equals(o: Any) = o match {
        case that: StringAt => that.values == this.values
        case _ => false
      }

      private def normalize_set(vals: Set[StrVal]): Set[StrVal] = {
        val cnt =
          for (strat <- vals; if !vals.filter( _ == strat).exists(strat <== _))
            yield strat
        cnt
      }
      private def normalize() = {
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

      //FIXME: move back to stdlib XD
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
      def meet(y: StringAt): StringAt = ???
      def widening(y: StringAt): StringAt = ???

      override def pretty: String = prettySet(values)
    }
    private[abstract_types] object StringAt {
      def fromString(value: String): StringAt = new StringAt(Set(Exact(value)))
      def top: StringAt = new StringAt(Set(StrVal.top))
      def bottom: StringAt = new StringAt(Set.empty[StrVal])
    }

    private[StringAtImpl] trait StrVal extends pretty {
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

      //def encrypt(key: StrVal): StrAt = Prefix(*)
      def checkpwd(pwd: StrVal): BoolAt = this ==^ pwd
      def hash: StrVal = StrVal.top
      def strToInt: NumAt = {
        this match {
          case Exact(x) =>
            toInt(x) match {
              case Some(v) => NumAt.fromNum(v)
              case None => NumAt.top
            }
          case _ => NumAt.top
        }
      }
      def strToBool: BoolAt = {
        this match {
          case Exact(x) =>
            toBool(x) match {
              case Some(v) => BoolAt.fromBool(v)
              case None => BoolAt.top
            }
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
      def join(y: StrVal): StringAt = ???
      def meet(y: StrVal): StringAt = ???


      override def pretty: String
    }
    private[StringAtImpl] object StrVal {
      def top = Prefix("")
    }

    private[StringAtImpl] case class Exact(str: String) extends StrVal {
      override def pretty: String = "\"%s\"" format  str
    }
    private[StringAtImpl] case  class Prefix(prefix: String) extends StrVal {
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

  implicit def absStrAt(l: StringAt): AbsString[BoolAt, NumAt, StringAt] = {
    new AbsString[BoolAt, NumAt, StringAt] {

      override val cnt: StringAt = l

      override def pretty: String = l.pretty

      override def ++^(r: Wrapper[StringAt]): StringAt = l ++^ r.cnt
      override def ==^(r: Wrapper[StringAt]): BoolAt = l ==^ r.cnt
      override def !=^(r: Wrapper[StringAt]): BoolAt = l !=^ r.cnt
      override def <^(r: Wrapper[StringAt]): BoolAt = l <^ r.cnt
      override def <=^(r: Wrapper[StringAt]): BoolAt = l <=^ r.cnt
      override def >^(r: Wrapper[StringAt]): BoolAt = l >^ r.cnt
      override def >=^(r: Wrapper[StringAt]): BoolAt = l <=^ r.cnt

      override def length: NumAt = l.length
      override def strToInt: NumAt = l.strToInt
      override def strToBool: BoolAt = l.strToBool
      override def trimBefore(r: Wrapper[NumAt]): StringAt = l trimBefore r.cnt
      override def trimAfter(r: Wrapper[NumAt]): StringAt = l trimAfter r.cnt

      //FIXME: move back to stdlib XD
      override def hash: StringAt = l.hash
      override def encrypt(key: StringAt): StringAt = l encrypt key
      override def checkpwd(pwd: StringAt): BoolAt = l checkpwd pwd

      override def <==(r: Wrapper[StringAt]): Boolean = l <== r.cnt
      override def join(r: Wrapper[StringAt]): StringAt = l join r.cnt
      override def meet(r: Wrapper[StringAt]): StringAt = l meet r.cnt
      override def widening(r: Wrapper[StringAt]): StringAt = l widening r.cnt
    }
  }
  type AbstractString = AbsString[BoolAt, NumAt, StringAt]
  object AbstractStringFactory extends AbsStringFactory[BoolAt, NumAt, StringAt] {
    override def fromString(value: String): AbstractString = fromString(value)
    override def bottom: AbstractString = StringAt.bottom
    override def top: AbstractString = StringAt.top
  }
}
