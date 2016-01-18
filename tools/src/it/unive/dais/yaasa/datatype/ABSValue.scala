package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.analyzer.IntValue
import it.unive.dais.yaasa.datatype.widening_lattice._
import it.unive.dais.yaasa.utils.prelude.{Wrapper, pretty}

/**
 * @author gbarbon
 * @author esteffin
 */
object ABSValue {

  trait AbstractValue extends pretty {  }

  trait AbstractDegrValue extends pretty {  }

/*
  trait AbsBool extends AbsBoolean[AbstractValue]
  trait AbsBoolFactory extends AbsBooleanFactory[AbstractValue]
  trait AbsInt extends AbsNum[AbstractValue]
  trait AbsIntFactory extends AbsNumFactory[AbstractValue]
  trait AbsStr extends AbsString[AbstractValue]
  trait AbsStrFactory extends AbsStringFactory[AbstractValue]

  trait AbsDegBool extends AbsBoolean[AbstractDegrValue]
  trait AbsDegBoolFactory extends AbsBooleanFactory[AbstractDegrValue]
  trait AbsDegInt extends AbsNum[AbstractDegrValue]
  trait AbsDegIntFactory extends AbsNumFactory[AbstractDegrValue]
  trait AbsDegStr extends AbsString[AbstractDegrValue]
  trait AbsDegStrFactory extends AbsStringFactory[AbstractDegrValue]
  */

  trait AbsBoolean[BoolVal, NumVal, StringVal] extends WideningLattice[BoolVal] with Wrapper[BoolVal] with pretty {
    def &&^(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def ||^(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def ==^(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def !=^(sndVal: Wrapper[BoolVal]): AbsBoolean[BoolVal, NumVal, StringVal]
    def notAt: AbsBoolean[BoolVal, NumVal, StringVal]

    def containsTrue: Boolean
    def containsFalse: Boolean

    def toStringAt: AbsString[BoolVal, NumVal, StringVal]

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

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsBoolean[BoolVal, NumVal, StringVal]
    override def bottom: AbsBoolean[BoolVal, NumVal, StringVal]
  }

  trait AbsNum[BoolVal, NumVal, StringVal] extends WideningLattice[NumVal] with Wrapper[NumVal] with pretty {
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

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsNum[BoolVal, NumVal, StringVal]
    override def bottom: AbsNum[BoolVal, NumVal, StringVal]
  }

  trait AbsString[BoolVal, NumVal, StringVal] extends WideningLattice[StringVal] with Wrapper[StringVal] with pretty  {
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
    def trimBefore(numVal: Wrapper[NumVal]): AbsString[BoolVal, NumVal, StringVal]
    def trimAfter(numVal: Wrapper[NumVal]): AbsString[BoolVal, NumVal, StringVal]

    //Note: <==, join, meet, widening are inherited by WideningLattice
    override def <==(sndVal: Wrapper[StringVal]): Boolean
    override def join(sndVal: Wrapper[StringVal]): AbsString[BoolVal, NumVal, StringVal]
    override def meet(sndVal: Wrapper[StringVal]): AbsString[BoolVal, NumVal, StringVal]
    override def widening(sndVal: Wrapper[StringVal]): AbsString[BoolVal, NumVal, StringVal]
  }
  trait AbsStringFactory[BoolVal, NumVal, StringVal] extends WideningLatticeFactory[StringVal] {
    def fromString(value: String): AbsString[BoolVal, NumVal, StringVal]

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsString[BoolVal, NumVal, StringVal]
    override def bottom: AbsString[BoolVal, NumVal, StringVal]
  }
}
