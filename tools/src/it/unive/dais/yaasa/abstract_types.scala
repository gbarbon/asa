package it.unive.dais.yaasa

import datatype.type_definitions._
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
          case _ if n > 0  => Plus
          case _ if n < 0  => Minus
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
        NumAt((this.value map { _.inv.value }) flatten)
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
          case _ if n > 0  => Set(Plus)
          case _ if n < 0  => Set(Minus)
          case _           => bottom //set [Zero; Plus; Minus]
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
          case Plus  => "(+)"
          case Zero  => "(0)"
          case Minus => "(-)"
        }

      def +(y: NumAtVal): NumAt =
        (this, y) match {
          case (Zero, n)      => NumAt(Set(n))
          case (n, Zero)      => NumAt(Set(n))
          case (Plus, Plus)   => NumAt(Set(Plus))
          case (Minus, Minus) => NumAt(Set(Minus))
          case (Plus, Minus)  => NumAt(Set(Plus, Minus, Zero))
          case (Minus, Plus)  => NumAt(Set(Plus, Minus, Zero))
        }
      def *(y: NumAtVal): NumAt =
        (this, y) match {
          case (Zero, _)      => NumAt(Set(Zero))
          case (_, Zero)      => NumAt(Set(Zero))
          case (Plus, Plus)   => NumAt(Set(Plus))
          case (Minus, Minus) => NumAt(Set(Plus))
          case (Plus, Minus)  => NumAt(Set(Minus))
          case (Minus, Plus)  => NumAt(Set(Minus))
        }
      def /(y: NumAtVal): NumAt =
        (this, y) match {
          case (_, Zero)      => NumAt.bottom
          case (Plus, Plus)   => NumAt(Set(Plus))
          case (Minus, Minus) => NumAt(Set(Plus))
          case (Plus, Minus)  => NumAt(Set(Minus))
          case (Minus, Plus)  => NumAt(Set(Minus))
          case (Zero, _)      => NumAt(Set(Zero))
        }
      def inv: NumAt =
        this match {
          case Zero  => NumAt(Set(Zero))
          case Plus  => NumAt(Set(Minus))
          case Minus => NumAt(Set(Plus))
        }
      def -(y: NumAtVal) = {
        NumAt(Set(this)) + (y.inv)
      }
      def %(y: NumAtVal): NumAt =
        (this, y) match {
          case (_, Zero)  => NumAt.bottom
          case (Zero, _)  => NumAt(Set(Zero))
          case (Plus, _)  => NumAt(Set(Plus, Zero))
          case (Minus, _) => NumAt(Set(Minus, Zero))
        }
      def ==^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Zero, Zero)   => BoolAt.sTrueAt
          case (Minus, Minus) => BoolAt.top
          case (Plus, Plus)   => BoolAt.top
          case (_, _)         => BoolAt.sFalseAt
        }
      def !=^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Zero, Zero)   => BoolAt.sFalseAt
          case (Minus, Minus) => BoolAt.top
          case (Plus, Plus)   => BoolAt.top
          case (_, _)         => BoolAt.sTrueAt
        }
      def >^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Plus, Plus)   => BoolAt.top
          case (Minus, Minus) => BoolAt.top
          case (Zero, Zero)   => BoolAt.sFalseAt
          case (Minus, _)     => BoolAt.sFalseAt
          case (Zero, Plus)   => BoolAt.sFalseAt
          case (_, _)         => BoolAt.sTrueAt
        }
      def <^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Plus, Plus)   => BoolAt.top
          case (Minus, Minus) => BoolAt.top
          case (Zero, Zero)   => BoolAt.sFalseAt
          case (Minus, _)     => BoolAt.sTrueAt
          case (Zero, Plus)   => BoolAt.sTrueAt
          case (_, _)         => BoolAt.sFalseAt
        }
      def <=^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Plus, Plus)   => BoolAt.top
          case (Minus, Minus) => BoolAt.top
          case (Zero, Zero)   => BoolAt.sTrueAt
          case (Minus, _)     => BoolAt.sTrueAt
          case (Zero, Plus)   => BoolAt.sTrueAt
          case (_, _)         => BoolAt.sFalseAt
        }
      def >=^(y: NumAtVal): BoolAt =
        (this, y) match {
          case (Plus, Plus)   => BoolAt.top
          case (Minus, Minus) => BoolAt.top
          case (Zero, Zero)   => BoolAt.sTrueAt
          case (Minus, _)     => BoolAt.sFalseAt
          case (Zero, Plus)   => BoolAt.sFalseAt
          case (_, _)         => BoolAt.sTrueAt
        }
    }
  }

  type NumAt = NumAtVal.NumAt

  object StringAt {

    object StringAt { def top = StringAt("") }

    case class StringAt(value: String) {

      override def toString() = pretty
      def pretty =
        "\"%s*\"" format value
      def length =
        NumAtVal.NumAt(Set(NumAtVal.Plus, NumAtVal.Zero))
      def Substring(startIndex: Int, length: Int) =
        {
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
