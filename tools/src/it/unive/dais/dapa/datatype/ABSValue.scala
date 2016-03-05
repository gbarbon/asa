package it.unive.dais.dapa.datatype

import it.unive.dais.dapa.absyn._
import it.unive.dais.dapa.datatype.ADType.ADInfo
import it.unive.dais.dapa.datatype.FortyTwo.FunAnnot
import it.unive.dais.dapa.datatype.lattice.Lattice
import it.unive.dais.dapa.datatype.widening_lattice._
import it.unive.dais.dapa.exception.AbsValuesMismatch
import it.unive.dais.dapa.utils.prelude.{Wrapper, pretty}
import it.unive.dais.dapa.utils.pretty_doc.pretty_doc

/**
 * @author gbarbon
 * @author esteffin
 */
object ABSValue {

  trait Visitable {
    //def accept[A, B](f: (A => B)): B
  }

  abstract class Type(name: String) extends pretty {
    override def pretty = name
  }
  case object TyNum
    extends Type("int")
  case object TyBool
    extends Type("boolean")
  case object TyString
    extends Type("String")
  case class TyArray(inner: Type)
    extends Type("%s[]" format inner.pretty)
  case class TyType(name: String)
    extends Type(name)


  trait TypedAbstractValue extends WideningLattice with pretty_doc {
    def ty: Type

    override def <==(r: Lattice): Boolean
    override def join(r: Lattice): TypedAbstractValue
    override def meet(r: Lattice): TypedAbstractValue
    def widening(r: WideningLattice): TypedAbstractValue
  }

  type AbstractValue = TypedAbstractValue

  type AbstractDegrValue = TypedAbstractValue

  type InCADInfo = ADInfo[FunAnnot, Uid, AbstractValue]

  trait ValueWithAbstraction extends TypedAbstractValue with pretty_doc {
    def joinADInfo(r: InCADInfo): ValueWithAbstraction
    def joinValue(r: AbstractValue): ValueWithAbstraction
    def setADInfo(r: InCADInfo): ValueWithAbstraction
    def merge(r: ValueWithAbstraction): ValueWithAbstraction
    override def <==(r: Lattice): Boolean
    override def join(r: Lattice): ValueWithAbstraction
    override def meet(r: Lattice): ValueWithAbstraction
    override def widening(r: WideningLattice): ValueWithAbstraction
  }

  case class SingleValueWithAbstraction(value: AbstractValue, adInfo: InCADInfo) extends ValueWithAbstraction {

    override def pretty_doc = value.pretty_doc <+> adInfo.pretty_doc
    //override def pretty = value.pretty + " -- " + adInfo.pretty

    /*def join_adinfo(other: ADInfo[FunAnnot, Uid, AbstractValue]) =
      SingleValueWithAbstraction(value, adInfo.join(other))*/

    override def <==(r: Lattice): Boolean = {
      r match {
        case r: SingleValueWithAbstraction => value <== r.value // && adInfo <== r.adInfo
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but does not.")
      }
    }

    override def join(r: Lattice): SingleValueWithAbstraction = {
      r match {
        case r: SingleValueWithAbstraction => SingleValueWithAbstraction(value join r.value, adInfo join r.adInfo)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but does not.")
      }
    }

    override def widening(r: WideningLattice): SingleValueWithAbstraction = {
      r match {
        case r: SingleValueWithAbstraction => SingleValueWithAbstraction(value widening r.value, adInfo widening r.adInfo)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but does not.")
      }
    }

    override def meet(r: Lattice): SingleValueWithAbstraction = {
      r match {
        case r: SingleValueWithAbstraction => SingleValueWithAbstraction(value meet r.value, adInfo meet r.adInfo)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but does not.")
      }
    }

    override def joinADInfo(r: InCADInfo): SingleValueWithAbstraction = SingleValueWithAbstraction(value, adInfo join r)

    override def joinValue(r: AbstractValue): SingleValueWithAbstraction = SingleValueWithAbstraction(value join r, adInfo)

    override def ty: Type = value.ty

    override def setADInfo(r: InCADInfo): SingleValueWithAbstraction = this.copy(adInfo = r)

    override def merge(r: ValueWithAbstraction): SingleValueWithAbstraction = {
      r match {
        case r: SingleValueWithAbstraction => SingleValueWithAbstraction(value join r.value, adInfo union r.adInfo)
        case _ => throw new AbsValuesMismatch("Argument should have type NumAt, but does not.")
      }
    }
  }

  trait AbsArray extends ValueWithAbstraction {
    val creation_implicit: InCADInfo
    def inner_type: Type
    override def ty: Type = TyArray(inner_type)
    def set(i: ValueWithAbstraction, x: ValueWithAbstraction): AbsArray
    def get(i: ValueWithAbstraction): Option[ValueWithAbstraction]
    def length: ValueWithAbstraction
    override def <==(r: Lattice): Boolean
    override def join(r: Lattice): AbsArray
    override def meet(r: Lattice): AbsArray
    override def widening(r: WideningLattice): AbsArray
    override def joinADInfo(r: InCADInfo): AbsArray
    override def joinValue(r: AbstractValue): AbsArray
    override def setADInfo(r: InCADInfo): AbsArray
    override def merge(r: ValueWithAbstraction): AbsArray
    def getValues: Vector[TypedAbstractValue]
  }
  trait AbsArrayFactory extends WideningLatticeFactory {
    def create(ty: Type, length: Int, creation_implicit: InCADInfo, default: ValueWithAbstraction): AbsArray
    def empty(ty: Type, creation_implicit: InCADInfo): AbsArray
    def bottom(ty: Type, creation_implicit: InCADInfo): AbsArray
  }

  trait AbsBoolean extends TypedAbstractValue with Visitable {
    def &&^(sndVal: AbsBoolean): AbsBoolean
    def ||^(sndVal: AbsBoolean): AbsBoolean
    def ==^(sndVal: AbsBoolean): AbsBoolean
    def !=^(sndVal: AbsBoolean): AbsBoolean
    def notAt: AbsBoolean

    def containsTrue: Boolean
    def containsFalse: Boolean

    def toStringAt: AbsString

    override def ty: Type = TyBool

    //Note: <==, join, meet, widening are inherited by WideningLattice
    override def <==(sndVal: Lattice): Boolean
    override def join(sndVal: Lattice): AbsBoolean
    override def meet(sndVal: Lattice): AbsBoolean
    override def widening(sndVal: WideningLattice): AbsBoolean
  }
  trait AbsBooleanFactory extends WideningLatticeFactory {
    def fromBool(value: Boolean): AbsBoolean
    def sTrueAt: AbsBoolean
    def sFalseAt: AbsBoolean

    def default: AbsBoolean

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsBoolean
    override def bottom: AbsBoolean
  }

  trait AbsNum extends TypedAbstractValue with Visitable {
    def +^(sndVal: AbsNum): AbsNum
    def -^(sndVal: AbsNum): AbsNum
    def *^(sndVal: AbsNum): AbsNum
    def /^(sndVal: AbsNum): AbsNum
    def %^(sndVal: AbsNum): AbsNum
    def ==^(sndVal: AbsNum): AbsBoolean
    def !=^(sndVal: AbsNum): AbsBoolean
    def <^(sndVal: AbsNum): AbsBoolean
    def <=^(sndVal: AbsNum): AbsBoolean
    def >^(sndVal: AbsNum): AbsBoolean
    def >=^(sndVal: AbsNum): AbsBoolean
    def negAt: AbsNum

    def toStringAt: AbsString

    override def ty: Type = TyNum

    //Note: <==, join, meet, widening are inherited by WideningLattice
    override def <==(sndVal: Lattice): Boolean
    override def join(sndVal: Lattice): AbsNum
    override def meet(sndVal: Lattice): AbsNum
    override def widening(sndVal: WideningLattice): AbsNum
  }
  trait AbsNumFactory extends WideningLatticeFactory {
    def fromNum(value: Int): AbsNum
    def interval(left: Int, right: Int): AbsNum
    def open_left(right: Int): AbsNum
    def open_right(left: Int): AbsNum

    def default: AbsNum

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsNum
    override def bottom: AbsNum
  }

  trait AbsString extends TypedAbstractValue with Visitable {
    def ++^(sndVal: AbsString): AbsString
    def ==^(sndVal: AbsString): AbsBoolean
    def !=^(sndVal: AbsString): AbsBoolean
    def <^(sndVal: AbsString): AbsBoolean
    def <=^(sndVal: AbsString): AbsBoolean
    def >^(sndVal: AbsString): AbsBoolean
    def >=^(sndVal: AbsString): AbsBoolean

    def strToInt: AbsNum
    def strToBool: AbsBoolean
    def length: AbsNum
    def dropUntil(numVal: AbsNum): AbsString
    def takeUntil(numVal: AbsNum): AbsString
    //def toCharArray: AbsArray

    def charAt(numVal: AbsNum): AbsString

    override def ty: Type = TyString

    //Note: <==, join, meet, widening are inherited by WideningLattice
    override def <==(sndVal: Lattice): Boolean
    override def join(sndVal: Lattice): AbsString
    override def meet(sndVal: Lattice): AbsString
    override def widening(sndVal: WideningLattice): AbsString
  }
  trait AbsStringFactory extends WideningLatticeFactory {
    def fromString(value: String): AbsString

    def default: AbsString

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsString
    override def bottom: AbsString
  }


  //TODO: DRAFT... Should not be used
  trait BlobValue extends TypedAbstractValue with pretty_doc {
    val num: AbsNum
    val bool: AbsBoolean
    val string: AbsString

    override def <==(sndVal: Lattice): Boolean
    override def join(sndVal: Lattice): AbsString
    override def meet(sndVal: Lattice): AbsString
    override def widening(sndVal: WideningLattice): AbsString
  }
}
