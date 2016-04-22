package it.unive.dais.dapa

/**
 * @author gbarbon
 */


import it.unive.dais.dapa.analyzer._
import it.unive.dais.dapa.utils.prelude.NotSupportedException
import it.unive.dais.dapa.abstract_types._
import it.unive.dais.dapa.datatype.ABSValue.{SingleValueWithAbstraction, ValueWithAbstraction, AbstractValue}

// Functions conversion from the tiny java to scala
object functConvert {
  def applyNative(name: String, oactuals: List[ValueWithAbstraction]): AbstractValue = {
    val actuals = oactuals map {
      case SingleValueWithAbstraction(v, _) => v
      case _ => throw new NotSupportedException("Native function with Array argument is not supported yet...")}
    val res: AbstractValue = name match {

      //stdlib functions

      case "encrypt" => actuals match {
        case List(lab: AbstractString, key: AbstractString) => stdlib.encrypt(lab, key)  // @FIXME: warning on compilation (non-variable type argument in type pattern is since it is eliminated by erasure
        case _ => throw new EvaluationException("encrypt function arguments not matched")
      }
      case "prefix" => actuals match {
        case List(str: AbstractString, end_char: AbstractNum) => stdlib.prefix(str, end_char)  // @FIXME: same as above...
        case _ => throw new EvaluationException("takeUntil function arguments not matched")
      }
      case "suffix" => actuals match {
        case List(str: AbstractString, begin_char: AbstractNum) => stdlib.suffix(str, begin_char)  // @FIXME: same as above...
        case _ => throw new EvaluationException("dropUntil function arguments not matched")
      }
      case "hash" => actuals match {
        case List(str: AbstractString) => stdlib.hash(str)  // @FIXME: same as above...
        case _                           => throw new EvaluationException("hash function arguments not matched")
      }
      case "checkpwd" => actuals match {
        case List(first: AbstractString, second: AbstractString) => stdlib.checkpwd(first, second) // @FIXME: same as above...
        case _ => throw new EvaluationException("checkpwd function arguments not matched")
      }
      case "intToString" => actuals match {
        case List(v: AbstractNum) => stdlib.intToString(v) // @FIXME: same as above...
        case _                      => throw new EvaluationException("intToString function arguments not matched")
      }
      case "boolToString" => actuals match {
        case List(v: AbstractBool) => stdlib.boolToString(v) // @FIXME: same as above...
        case _                       => throw new EvaluationException("boolToString function arguments not matched")
      }
      case "strCharAt" => actuals match {
        case List(v: AbstractString, i: AbstractNum) => stdlib.strCharAt(v, i)// @FIXME: same as above...
        case _                         => throw new EvaluationException("strCharAt function arguments not matched")
      }
      case "strToInt" => actuals match {
        case List(v: AbstractString) => stdlib.strToInt(v)// @FIXME: same as above...
        case _                         => throw new EvaluationException("strToInt function arguments not matched")
      }
      case "strToBool" => actuals match {
        case List(v: AbstractString) => stdlib.strToBool(v)  // @FIXME: same as above...
        case _                         => throw new EvaluationException("strToBool function arguments not matched")
      }
      case "length" => actuals match {
        case List(v: AbstractString) => stdlib.length(v)  // @FIXME: same as above...
        case _                         => throw new EvaluationException("length function arguments not matched")
      }

      //readlib functions

      case "readString" => actuals match {
        case List(str: AbstractString) => readlib.readString(str)  // @FIXME: same as above...
        case _                           => throw new EvaluationException("readString function arguments not matched")
      }
      case "readInt" => actuals match {
        case List(str: AbstractString) => readlib.readInt(str)  // @FIXME: same as above...
        case _                           => throw new EvaluationException("readInt function arguments not matched")
      }
      case "readBool" => actuals match {
        case List(str: AbstractString) => readlib.readBool(str)  // @FIXME: same as above...
        case _                           => throw new EvaluationException("readBool function arguments not matched")
      }
      case "readIMEI" => readlib.readIMEI()
      case "readUsrPwd" => actuals match {
        case List(str: AbstractString) => readlib.readUsrPwd(str)  // @FIXME: same as above...
        case _                           => throw new EvaluationException("readUsrPwd function arguments not matched")
      }
      case "readGeoLoc" => readlib.readGeoLoc()
      case "readPhoneNum" => actuals match {
        case List(str: AbstractString) => readlib.readPhoneNum(str)  // @FIXME: same as above...
        case _                           => throw new EvaluationException("readPhoneNum function arguments not matched")
      }
      case "strInput"  => readlib.strInput
      case "boolInput" => readlib.boolInput
      case "intInput"  => readlib.intInput
      case "readPreciseString" => readlib.readPreciseString()
      case "readPreciseInt" => readlib.readPreciseInt()
      case "readPreciseBool" => readlib.readPreciseBool()
      case "readPreciseIMEI" => readlib.readPreciseIMEI()
      case _           => throw new EvaluationException("unrecognized native function")
    }
    res
  }

  //It replicates the tiny java stdlib (in resources)
  object stdlib {

    /**
     * It encrypts the label with a give key
     * @return the encrypted label
     */
    def encrypt(label: AbstractString, key: AbstractString): AbstractString = AbstractStringFactory.top


    /**
      * str.substring(0, endChar)
      * @return the result string
      */
    def prefix(str: AbstractString, endChar: AbstractNum): AbstractString =
      str.takeUntil(endChar)

    /**
      * str.substring(beginChar, str.length - 1)
      * @return the result string
      */
    def suffix(str: AbstractString, beginChar: AbstractNum): AbstractString =
      str.dropUntil(beginChar)


    /**
     * Hash function.
     * @param str input string
     * @return the hash value in Array[Byte]
     */
    def hash(str: AbstractString) = AbstractStringFactory.top

    /**
     * Check if a password (string) is correct or not (string compare). Dummy function.
     * @param first password inserted by the user
     * @param second actual correct password
     * @return a boolean value, true if the two values are the same, false otherwise
     */
    def checkpwd(first: AbstractString, second: AbstractString): AbstractBool = first ==^ second

    /**
     * It retrieves the device IMEI.
     * @return the IMEI from the datastore
     */
    def getDeviceID: AbstractString = {
      AbstractStringFactory.top
    }

    /**
      * It returns the character at the specified index of the given string
      * @param str the string
      * @param intArg integer input argument
      * @return string
      */
    def strCharAt(str: AbstractString, intArg: AbstractNum): AbstractString = str.charAt(intArg)

    /**
     * It converts an int to a string
     * @param intArg integer input argument
     * @return string
     */
    def intToString(intArg: AbstractNum): AbstractString = intArg.toStringAt

    /**
     * It converts a boolean to a string
     * @param boolArg boolean input argument
     * @return string
     */
    def boolToString(boolArg: AbstractBool): AbstractString = boolArg.toStringAt

    /**
     * It converts a string to an int
     * @param str integer input argument
     * @return int
     */
    def strToInt(str: AbstractString): AbstractNum = { str.strToInt}

    /**
     * It converts a string to a boolean
     * @param str integer input argument
     * @return int
     */
    def strToBool(str: AbstractString): AbstractBool = { str.strToBool}

    /**
     * @param str input string
     * @return the dimension in integer of a string
     */
    def length(str: AbstractString): AbstractNum = str.length

  }

  /**
   * It replicates the tiny java readlib (in resources)
   */
  object readlib {

    /**
      * The following are testing methods
      */
    def readPreciseInt(): AbstractNum = {
      val value: Int = 5
      AbstractNumFactory.fromNum(value)
    }
    def readPreciseBool(): AbstractBool = {
      val value: Boolean = true
      AbstractBoolFactory.fromBool(value)
    }
    def readPreciseString(): AbstractString = {
      val value: String = "blabla"
      AbstractStringFactory.fromString(value)
    }
    def readPreciseIMEI(): AbstractString = {
      val value: Int = 5
      AbstractStringFactory.fromString("35-209900-176148-1")
    }

    /**
     * Read a generic string confidential label from the datastore of the device.
     * @param name the name of the label
     * @return the confidential label from the datastore (string)
     */
    def readString(name: AbstractString): AbstractString = {
      AbstractStringFactory.top
    }

    /**
     * Read a generic int confidential label from the datastore of the device.
     * @param name the name of the concrete value
     * @return the confidential label from the datastore (int)
     */
    def readInt(name: AbstractString): AbstractNum = {
      AbstractNumFactory.top
    }

    /**
     * Read a generic boolean confidential label from the datastore of the device.
     * @param name the name of the concrete value
     * @return the confidential label from the datastore (bool)
     */
    def readBool(name: AbstractString): AbstractBool = {
      AbstractBoolFactory.top
    }

    /**
     * Read the IMEI
     * @return the device IMEI
     */
    def readIMEI(): AbstractString = {
      AbstractStringFactory.top
    }

    /**
     * Read the password
     * @param usr the name of the user
     * @return the password
     */
    def readUsrPwd(usr: AbstractString): AbstractString = {
      AbstractStringFactory.top
    }

    /**
     * Read the geographic position of the device
     * @return the geographic coordinates of the devices
     */
    def readGeoLoc(): AbstractString = {
      AbstractStringFactory.top
    }

    /**
     * Read the given contact from the address book
     * @return the geographic coordinates of the devices
     */
    def readPhoneNum(contact: AbstractString): AbstractString = {
      AbstractStringFactory.top
    }

    /**
     * It reads the input from the keyboard
     * @return string
     */
    def strInput = AbstractStringFactory.top //readLine()

    /**
     * It reads the input from the keyboard
     * @return bool
     */
    def boolInput = AbstractBoolFactory.top

    /**
     * It reads the input from the keyboard
     * @return int
     */
    def intInput = AbstractStringFactory.top

  }
}
