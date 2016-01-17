package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.analyzer.IntValue
import it.unive.dais.yaasa.datatype.widening_lattice._
import it.unive.dais.yaasa.utils.prelude.{Wrapper, pretty}

/**
 * @author gbarbon
 * @author esteffin
 */
object ABSValue {

  trait AbstractValue extends pretty {
  }

  trait AbstractDegrValue extends  pretty {
  }



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
    def &&^(sndVal: Wrapper[BoolVal]): BoolVal
    def ||^(sndVal: Wrapper[BoolVal]): BoolVal
    def ==^(sndVal: Wrapper[BoolVal]): BoolVal
    def !=^(sndVal: Wrapper[BoolVal]): BoolVal
    def notAt: BoolVal

    def containsTrue: Boolean
    def containsFalse: Boolean

    def boolToString: StringVal

    //Note: <==, join, meet, widening are inherited by WideningLattice
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
    def +^(sndVal: Wrapper[NumVal]): NumVal
    def -^(sndVal: Wrapper[NumVal]): NumVal
    def *^(sndVal: Wrapper[NumVal]): NumVal
    def /^(sndVal: Wrapper[NumVal]): NumVal
    def %^(sndVal: Wrapper[NumVal]): NumVal
    def ==^(sndVal: Wrapper[NumVal]): BoolVal
    def !=^(sndVal: Wrapper[NumVal]): BoolVal
    def <^(sndVal: Wrapper[NumVal]): BoolVal
    def <=^(sndVal: Wrapper[NumVal]): BoolVal
    def >^(sndVal: Wrapper[NumVal]): BoolVal
    def >=^(sndVal: Wrapper[NumVal]): BoolVal
    def negAt: NumVal

    def intToString: StringVal

    //Note: <==, join, meet, widening are inherited by WideningLattice
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
    def ++^(sndVal: Wrapper[StringVal]): StringVal
    def ==^(sndVal: Wrapper[StringVal]): BoolVal
    def !=^(sndVal: Wrapper[StringVal]): BoolVal
    def <^(sndVal: Wrapper[StringVal]): BoolVal
    def <=^(sndVal: Wrapper[StringVal]): BoolVal
    def >^(sndVal: Wrapper[StringVal]): BoolVal
    def >=^(sndVal: Wrapper[StringVal]): BoolVal

    def strToInt: NumVal
    def strToBool: BoolVal
    def length: NumVal
    def trimBefore(numVal: Wrapper[NumVal]): StringVal
    def trimAfter(numVal: Wrapper[NumVal]): StringVal

    //FIXME: move back to stdlib XD
    def encrypt(key: StringVal): StringVal
    def hash: StringVal
    def checkpwd(pwd: StringVal): BoolVal
    // def substring

    //Note: <==, join, meet, widening are inherited by WideningLattice
  }
  trait AbsStringFactory[BoolVal, NumVal, StringVal] extends WideningLatticeFactory[StringVal] {
    def fromString(value: String): AbsString[BoolVal, NumVal, StringVal]

    //Note: top, bottom are inherited by WideningLatticeFactory
    override def top: AbsString[BoolVal, NumVal, StringVal]
    override def bottom: AbsString[BoolVal, NumVal, StringVal]
  }
}
