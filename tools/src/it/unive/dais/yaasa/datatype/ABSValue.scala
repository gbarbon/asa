package it.unive.dais.yaasa.datatype

/**
 * @author gbarbon
 */
object ABSValue {

  // trait AbsBoolean extends AbstractValue {
  trait AbsBoolean[AbsValue] {
    def &&^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def ||^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def ==^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def !=^(sndVal: AbsBoolean[AbsValue]): AbsBoolean[AbsValue]
    def notAt: AbsBoolean[AbsValue]
    //def boolToInt: AbsInteger[AbsValue] // @FIXME: to define in functConvert
    //def boolToString: AbsString[AbsValue] // @FIXME: to define in functConvert

    def pretty: String
    override def toString() = pretty
  }

  // trait AbsInteger extends AbstractValue {
  trait AbsInteger[AbsValue] {
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

    def pretty: String
    override def toString() = pretty
  }

  // trait AbsString extends AbstractValue {
  trait AbsString[AbsValue] {
    def ++^(sndVal: AbsString[AbsValue]): AbsString[AbsValue]
    def ==^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def !=^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def <^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def <=^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def >^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    def >=^(sndVal: AbsString[AbsValue]): AbsBoolean[AbsValue]
    //def strToBool: AbsBoolean[AbsValue] // @FIXME: to define in functConvert
    //def strToInt: AbsInteger[AbsValue] // @FIXME: to define in functConvert

    def pretty: String
    override def toString() = pretty
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