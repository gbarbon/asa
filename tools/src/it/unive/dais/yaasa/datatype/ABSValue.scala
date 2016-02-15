package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.datatype.lattice.Lattice
import it.unive.dais.yaasa.datatype.widening_lattice._
import it.unive.dais.yaasa.utils.prelude.{Wrapper, pretty}

/**
 * @author gbarbon
 * @author esteffin
 */
object ABSValue {

  abstract class Type(name: String) extends pretty {
    override def pretty = name
  }
  case object TyNum
    extends Type("int")
  case object TyBool
    extends Type("boolean")
  case object TyString
    extends Type("String")
  case class TyArray[+A <: Type](inner: A)
    //FIXME: WHhy A??
    extends Type("%s[]" format inner.pretty)
  case class TyType(name: String)
    extends Type(name)


  trait TypedAbstractValue extends WideningLattice[Any] with pretty {
    val ty: Type

    override def <==[B >: Any](r: Lattice[B]): Boolean
    override def join[B >: Any](r: Lattice[B]): TypedAbstractValue
    override def meet[B >: Any](r: Lattice[B]): TypedAbstractValue
    def widening[B >: Any](r: WideningLattice[B]): TypedAbstractValue
  }

  trait AbsBoolean[BoolVal, NumVal, StringVal] extends WideningLattice[BoolVal] with TypedAbstractValue with Wrapper[BoolVal] with pretty {
    def &&^(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def ||^(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def ==^(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def !=^(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def notAt: AbsBoolean[BoolVal, NumVal, StringVal]

    def containsTrue: Boolean
    def containsFalse: Boolean

    def toStringAt: AbsString[BoolVal, NumVal, StringVal]

    override val ty: Type = TyBool

    //Note: <==, join, meet, widening are inherited by WideningLattice
    override def <==[B >: BoolVal](sndVal: Lattice[B]): Boolean
    override def join[B >: BoolVal](sndVal: Lattice[B]): AbsBoolean[BoolVal, NumVal, StringVal]
    override def meet[B >: BoolVal](sndVal: Lattice[B]): AbsBoolean[BoolVal, NumVal, StringVal]
    override def widening[B >: BoolVal](sndVal: WideningLattice[B]): AbsBoolean[BoolVal, NumVal, StringVal]
  }
  trait AbsBooleanFactory[BoolVal, NumVal, StringVal] extends WideningLatticeFactory[BoolVal] {
    def fromBool(value: Boolean): AbsBoolean[BoolVal, NumVal, StringVal]
    def sTrueAt: AbsBoolean[BoolVal, NumVal, StringVal]
    def sFalseAt: AbsBoolean[BoolVal, NumVal, StringVal]

    def default: AbsBoolean[BoolVal, NumVal, StringVal]

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsBoolean[BoolVal, NumVal, StringVal]
    override def bottom: AbsBoolean[BoolVal, NumVal, StringVal]
  }

  trait AbsNum[BoolVal, NumVal, StringVal] extends WideningLattice[NumVal] with TypedAbstractValue with Wrapper[NumVal] with pretty {
    def +^(sndVal: Wrapper[NumVal]): AbsNum[BoolVal, NumVal, StringVal]
    def -^(sndVal: Wrapper[NumVal]): AbsNum[BoolVal, NumVal, StringVal]
    def *^(sndVal: Wrapper[NumVal]): AbsNum[BoolVal, NumVal, StringVal]
    def /^(sndVal: Wrapper[NumVal]): AbsNum[BoolVal, NumVal, StringVal]
    def %^(sndVal: Wrapper[NumVal]): AbsNum[BoolVal, NumVal, StringVal]
    def ==^(sndVal: Wrapper[NumVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def !=^(sndVal: Wrapper[NumVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def <^(sndVal: Wrapper[NumVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def <=^(sndVal: Wrapper[NumVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def >^(sndVal: Wrapper[NumVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def >=^(sndVal: Wrapper[NumVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def negAt: AbsNum[BoolVal, NumVal, StringVal]

    def toStringAt: AbsString[BoolVal, NumVal, StringVal]

    override val ty: Type = TyNum

    //Note: <==, join, meet, widening are inherited by WideningLattice
    override def <==[B >: NumVal](sndVal: Lattice[B]): Boolean
    override def join[B >: NumVal](sndVal: Lattice[B]): AbsNum[BoolVal, NumVal, StringVal]
    override def meet[B >: NumVal](sndVal: Lattice[B]): AbsNum[BoolVal, NumVal, StringVal]
    override def widening[B >: NumVal](sndVal: WideningLattice[B]): AbsNum[BoolVal, NumVal, StringVal]
  }
  trait AbsNumFactory[BoolVal, NumVal, StringVal] extends WideningLatticeFactory[NumVal] {
    def fromNum(value: Int): AbsNum[BoolVal, NumVal, StringVal]
    def interval(left: Int, right: Int): AbsNum[BoolVal, NumVal, StringVal]
    def open_left(right: Int): AbsNum[BoolVal, NumVal, StringVal]
    def open_right(left: Int): AbsNum[BoolVal, NumVal, StringVal]

    def default: AbsNum[BoolVal, NumVal, StringVal]

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsNum[BoolVal, NumVal, StringVal]
    override def bottom: AbsNum[BoolVal, NumVal, StringVal]
  }

  trait AbsString[BoolVal, NumVal, StringVal] extends WideningLattice[StringVal] with TypedAbstractValue with Wrapper[StringVal] with pretty {
    def ++^(sndVal: Wrapper[StringVal]): AbsString[BoolVal, NumVal, StringVal]
    def ==^(sndVal: Wrapper[StringVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def !=^(sndVal: Wrapper[StringVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def <^(sndVal: Wrapper[StringVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def <=^(sndVal: Wrapper[StringVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def >^(sndVal: Wrapper[StringVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def >=^(sndVal: Wrapper[StringVal]): AbsBoolean[BoolVal, NumVal, StringVal]

    def strToInt: AbsNum[BoolVal, NumVal, StringVal]
    def strToBool: AbsBoolean[BoolVal, NumVal, StringVal]
    def length: AbsNum[BoolVal, NumVal, StringVal]
    def dropUntil(numVal: Wrapper[NumVal]): AbsString[BoolVal, NumVal, StringVal]
    def takeUntil(numVal: Wrapper[NumVal]): AbsString[BoolVal, NumVal, StringVal]

    override val ty: Type = TyString

    //Note: <==, join, meet, widening are inherited by WideningLattice
    override def <==[B >: StringVal](sndVal: Lattice[B]): Boolean
    override def join[B >: StringVal](sndVal: Lattice[B]): AbsString[BoolVal, NumVal, StringVal]
    override def meet[B >: StringVal](sndVal: Lattice[B]): AbsString[BoolVal, NumVal, StringVal]
    override def widening[B >: StringVal](sndVal: WideningLattice[B]): AbsString[BoolVal, NumVal, StringVal]
  }
  trait AbsStringFactory[BoolVal, NumVal, StringVal] extends WideningLatticeFactory[StringVal] {
    def fromString(value: String): AbsString[BoolVal, NumVal, StringVal]

    def default: AbsString[BoolVal, NumVal, StringVal]

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsString[BoolVal, NumVal, StringVal]
    override def bottom: AbsString[BoolVal, NumVal, StringVal]
  }
/*
  trait AbsArray[ActualTy <: TypedAbstractValue, BoolVal, NumVal, StringVal]
    extends WideningLattice[AbsArray[ActualTy, BoolVal, NumVal, StringVal]] with
            TypedAbstractValue with
            Wrapper[AbsArray[ActualTy, BoolVal, NumVal, StringVal]] with
            pretty {
    val inner_type: Type
    override val ty: Type = TyArray(inner_type)
    protected val size: NumVal
    def set(i: NumVal, x: ActualTy): AbsArray[ActualTy, BoolVal, NumVal, StringVal]
    def get(i: NumVal): ActualTy
    def length: NumVal = size
  }
  trait AbsArrayFactory[ActualTy, BoolVal, NumVal, StringVal] {
    def create(ty: Type, i: NumVal): AbsArray[ActualTy, BoolVal, NumVal, StringVal]
  }*/


  type AbstractValue = TypedAbstractValue

  type AbstractDegrValue = TypedAbstractValue



  //TODO: DRAFT... Should not be used
  trait BlobValue[BoolVal, NumVal, StringVal] extends WideningLattice[StringVal] with TypedAbstractValue with Wrapper[StringVal] with pretty {
    val num: NumVal
    val bool: BoolVal
    val string: StringVal

    override def <==[B >: StringVal](sndVal: Lattice[B]): Boolean
    override def join[B >: StringVal](sndVal: Lattice[B]): AbsString[BoolVal, NumVal, StringVal]
    override def meet[B >: StringVal](sndVal: Lattice[B]): AbsString[BoolVal, NumVal, StringVal]
    override def widening[B >: StringVal](sndVal: WideningLattice[B]): AbsString[BoolVal, NumVal, StringVal]
  }

/*
  case class AbstractBooleanGenValue[BoolVal, NumVal, StringVal](value: AbsBoolean[BoolVal, NumVal, StringVal])
    extends AbstractValue {
    override val ty = TyBool
    override def pretty: String = value.pretty
  }
  case class AbstractNumGenValue[BoolVal, NumVal, StringVal](value: AbsNum[BoolVal, NumVal, StringVal])
    extends AbstractValue {
    override val ty = TyNum
    override def pretty: String = value.pretty
  }
  case class AbstractStringGenValue[BoolVal, NumVal, StringVal](value: AbsString[BoolVal, NumVal, StringVal])
    extends AbstractValue {
    override val ty = TyString
    override def pretty: String = pretty
  }


  case class AbstractBooleanGenDegrValue[BoolVal, NumVal, StringVal](value: AbsBoolean[BoolVal, NumVal, StringVal])
    extends AbstractDegrValue {
    override val ty = TyBool
    override def pretty: String = value.pretty
  }
  case class AbstractNumGenDegrValue[BoolVal, NumVal, StringVal](value: AbsNum[BoolVal, NumVal, StringVal])
    extends AbstractDegrValue {
    override val ty = TyNum
    override def pretty: String = value.pretty
  }
  case class AbstractStringGenDegrValue[BoolVal, NumVal, StringVal](value: AbsString[BoolVal, NumVal, StringVal])
    extends AbstractDegrValue {
    override val ty = TyString
    override def pretty: String = pretty
  }*/

}
