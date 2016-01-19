package it.unive.dais.yaasa.datatype

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
    extends Type("bool")
  case object TyString
    extends Type("string")
  case class TyType(name: String)
    extends Type(name)


  trait AbstractValue extends pretty { val ty: Type }

  trait AbstractDegrValue extends pretty { val ty: Type }


  trait AbsBoolean[BoolVal, NumVal, StringVal] extends WideningLattice[BoolVal] with AbstractValue with AbstractDegrValue with Wrapper[BoolVal] with pretty {
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
    override def <==(sndVal: Wrapper[BoolVal]): Boolean
    override def join(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    override def meet(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    override def widening(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
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

  trait AbsNum[BoolVal, NumVal, StringVal] extends WideningLattice[NumVal] with AbstractValue with AbstractDegrValue with Wrapper[NumVal] with pretty {
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
    override def <==(sndVal: Wrapper[NumVal]): Boolean
    override def join(sndVal: Wrapper[NumVal]): AbsNum[BoolVal, NumVal, StringVal]
    override def meet(sndVal: Wrapper[NumVal]): AbsNum[BoolVal, NumVal, StringVal]
    override def widening(sndVal: Wrapper[NumVal]): AbsNum[BoolVal, NumVal, StringVal]
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

  trait AbsString[BoolVal, NumVal, StringVal] extends WideningLattice[StringVal] with AbstractValue with AbstractDegrValue with Wrapper[StringVal] with pretty  {
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
    override def <==(sndVal: Wrapper[StringVal]): Boolean
    override def join(sndVal: Wrapper[StringVal]): AbsString[BoolVal, NumVal, StringVal]
    override def meet(sndVal: Wrapper[StringVal]): AbsString[BoolVal, NumVal, StringVal]
    override def widening(sndVal: Wrapper[StringVal]): AbsString[BoolVal, NumVal, StringVal]
  }
  trait AbsStringFactory[BoolVal, NumVal, StringVal] extends WideningLatticeFactory[StringVal] {
    def fromString(value: String): AbsString[BoolVal, NumVal, StringVal]

    def default: AbsString[BoolVal, NumVal, StringVal]

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsString[BoolVal, NumVal, StringVal]
    override def bottom: AbsString[BoolVal, NumVal, StringVal]
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
