package it.unive.dais.yaasa.datatype

import it.unive.dais.yaasa.datatype.widening_lattice._

/**
 * @author gbarbon
 */
object ABSValue {

  // trait AbsBoolean extends AbstractValue {
  trait AbsBoolean[AbsValue] extends WideningLattice[AbsValue] {
    def &&^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def ||^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def ==^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def !=^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def notAt: AbsBoolean[AbsValue]

    //TODO: Strongly suggested
    def containsTrue: Boolean
    def containsFalse: Boolean

    //def boolToInt: AbsInteger[AbsValue] // @FIXME: to define in functConvert
    //def boolToString: AbsString[AbsValue] // @FIXME: to define in functConvert

    //Note: <==, join, meet, widening are inherited by WideningLattice

    def pretty: String
    override def toString() = pretty
  }
  trait AbsBooleanFactory[AbsValue] extends WideningLatticeFactory[AbsValue] {
    def fromBool(b: Boolean): AbsBoolean[AbsValue]
    def sTrueAt: AbsBoolean[AbsValue]
    def sFalseAt: AbsBoolean[AbsValue]
    //Note: top, bottom are inherited by WideningLatticeFactory
  }

  // trait AbsInteger extends AbstractValue {
  trait AbsInteger[AbsValue] extends WideningLattice[AbsValue] {
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
    def fromNum(b: Int): AbsInteger[AbsValue]
    def interval(a: Int, b: Int): AbsInteger[AbsValue]
    def open_left(a: Int): AbsInteger[AbsValue]
    def open_right(b: Int): AbsInteger[AbsValue]
    //Note: top, bottom are inherited by WideningLatticeFactory
  }

  // trait AbsString extends AbstractValue {
  trait AbsString[AbsValue] extends WideningLattice[AbsValue] {
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
    def fromString(b: Int): AbsString[AbsValue]
    //Note: top, bottom are inherited by WideningLatticeFactory
  }

  trait AbstractValue {
    val value: Any
    // val ty: Type // @FIXME: referece to absyn, to avoid!

    def join(secondEl: AbstractValue): AbstractValue
    // @TODO: raise exception if types are not compatible

    def pretty: String = "[%s]" format (value)
    override def toString() = pretty
  }

  trait AbsBool extends AbstractValue with AbsBoolean[AbstractValue]
  trait AbsInt extends AbstractValue with AbsInteger[AbstractValue]
  trait AbsStr extends AbstractValue with AbsString[AbstractValue]

  trait AbstractDegrValue {
    val value: Any
    // val ty: Type // @FIXME: referece to absyn, to avoid!

    def join(sndVal: AbstractDegrValue): AbstractDegrValue

    def pretty: String = "[%s]" format (value)
    override def toString() = pretty
  }

  trait AbsDegBool extends AbstractDegrValue with AbsBoolean[AbstractDegrValue]
  trait AbsDegInt extends AbstractDegrValue with AbsInteger[AbstractDegrValue]
  trait AbsDegStr extends AbstractDegrValue with AbsString[AbstractDegrValue]
}
