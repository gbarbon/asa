package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.datatype.widening_lattice._

/**
 * @author gbarbon
 * @author esteffin
 */
object ABSValue {

  trait AbstractValue {
    def pretty: String
    override def toString() = pretty
  }

  trait AbstractDegrValue {
    def pretty: String
    override def toString() = pretty
  }

   // trait AbsBoolean extends AbstractValue {
  trait AbsBoolean[AbsValue] extends AbstractValue with AbstractDegrValue with WideningLattice[AbsValue] {
    def &&^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def ||^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def ==^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def !=^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def notAt: AbsBoolean[AbsValue]

    def containsTrue: Boolean
    def containsFalse: Boolean

    //def boolToInt: AbsInteger[AbsValue] // @FIXME: to define in functConvert
    //def boolToString: AbsString[AbsValue] // @FIXME: to define in functConvert

    //Note: <==, join, meet, widening are inherited by WideningLattice

    def pretty: String
    override def toString() = pretty
  }
  trait AbsBooleanFactory[AbsValue] extends WideningLatticeFactory[AbsValue] {
    def fromBool(value: Boolean): AbsBoolean[AbsValue]
    def sTrueAt: AbsBoolean[AbsValue]
    def sFalseAt: AbsBoolean[AbsValue]
    //Note: top, bottom are inherited by WideningLatticeFactory
  }

  // trait AbsInteger extends AbstractValue {
  trait AbsInteger[AbsValue] extends AbstractValue with AbstractDegrValue with WideningLattice[AbsValue] {
    def +^(sndVal: AbsInteger[AbsValue]): AbsInteger[AbsValue]
    def -^(sndVal: AbsInteger[AbsValue]): AbsInteger[AbsValue]
    def *^(sndVal: AbsInteger[AbsValue]): AbsInteger[AbsValue]
    def /^(sndVal: AbsInteger[AbsValue]): AbsInteger[AbsValue]
    def %^(sndVal: AbsInteger[AbsValue]): AbsInteger[AbsValue]
    def ==^(sndVal: AbsInteger[AbsValue]): AbsBoolean[AbsValue]
    def !=^(sndVal: AbsInteger[AbsValue]): AbsBoolean[AbsValue]
    def <^(sndVal: AbsInteger[AbsValue]): AbsBoolean[AbsValue]
    def <=^(sndVal: AbsInteger[AbsValue]): AbsBoolean[AbsValue]
    def >^(sndVal: AbsInteger[AbsValue]): AbsBoolean[AbsValue]
    def >=^(sndVal: AbsInteger[AbsValue]): AbsBoolean[AbsValue]
    def negAt: AbsInteger[AbsValue]
    //def intToBool: AbsBoolean[AbsValue] // @FIXME: to define in functConvert
    //def intToString: AbsString[AbsValue] // @FIXME: to define in functConvert

    //Note: <==, join, meet, widening are inherited by WideningLattice

    def pretty: String
    override def toString() = pretty
  }
  trait AbsIntegerFactory[AbsValue] extends WideningLatticeFactory[AbsValue] {
    def fromNum(value: Int): AbsInteger[AbsValue]
    def interval(left: Int, right: Int): AbsInteger[AbsValue]
    def open_left(right: Int): AbsInteger[AbsValue]
    def open_right(left: Int): AbsInteger[AbsValue]
    //Note: top, bottom are inherited by WideningLatticeFactory
  }

  // trait AbsString extends AbstractValue {
  trait AbsString[AbsValue] extends AbstractValue with AbstractDegrValue with WideningLattice[AbsValue] {
    def ++^(sndVal: AbsString[AbsValue]): AbsString[AbsValue]
    def ==^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def !=^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def <^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def <=^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def >^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def >=^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    //def strToBool: AbsBoolean[AbsValue] // @FIXME: to define in functConvert
    //def strToInt: AbsInteger[AbsValue] // @FIXME: to define in functConvert

    //Note: <==, join, meet, widening are inherited by WideningLattice

    def pretty: String
    override def toString() = pretty
  }
  trait AbsStringFactory[AbsValue] extends WideningLatticeFactory[AbsValue] {
    def fromString(value: Int): AbsString[AbsValue]
    //Note: top, bottom are inherited by WideningLatticeFactory
  }
}
