package it.unive.dais.yaasa

/**
 * @author gbarbon
 */

import java.security.MessageDigest
import it.unive.dais.yaasa.analyzer._
import it.unive.dais.yaasa.absyn._
import it.unive.dais.yaasa.abstract_types._
import it.unive.dais.yaasa.datatype.ABSValue.AbstractValue
import it.unive.dais.yaasa.datatype.FortyTwo.{ValueWithAbstraction, BitQuantity}

/**
 * It contains functions conversion from the tiny java to scala
 */
object functConvert {
  //FIXME: tuttavia non sono convito si debba ritornare SOLO un AbstractValue...
  def applyNative(name: String, oactuals: List[ValueWithAbstraction]): AbstractValue = {
    val actuals = oactuals map { _.value }
    val res: AbstractValue = name match {
      //stdlib functions
      case "encrypt" => actuals match {
        case List(lab: AbstractString, key: AbstractString) => stdlib.encrypt(lab, key)
        case _ => throw new EvaluationException("encrypt function arguments not matched")
      }
      /*case "substring" => actuals match {
        case List((StringValue(str), _), (IntValue(beg), _), (IntValue(end), _)) => stdlib.substring(str, beg, end)
        case _ => throw new EvaluationException("substring function arguments not matched")
      }*/
      case "takeUntil" => actuals match {
        case List(str: AbstractString, end_char: AbstractNum) => stdlib.takeUntil(str, end_char)
        case _ => throw new EvaluationException("takeUntil function arguments not matched")
      }
      case "dropUntil" => actuals match {
        case List(str: AbstractString, begin_char: AbstractNum) => stdlib.takeUntil(str, begin_char)
        case _ => throw new EvaluationException("dropUntil function arguments not matched")
      }
      case "hash" => actuals match {
        case List(str: AbstractString) => stdlib.hash(str)
        case _                           => throw new EvaluationException("hash function arguments not matched")
      }
      case "checkpwd" => actuals match {
        case List(first: AbstractString, second: AbstractString) => stdlib.checkpwd(first, second)
        case _ => throw new EvaluationException("checkpwd function arguments not matched")
      }
      case "intToString" => actuals match {
        case List(v: AbstractNum) => stdlib.intToString(v)
        case _                      => throw new EvaluationException("intToString function arguments not matched")
      }
      case "boolToString" => actuals match {
        case List(v: AbstractBool) => stdlib.boolToString(v)
        case _                       => throw new EvaluationException("boolToString function arguments not matched")
      }
      case "strToInt" => actuals match {
        case List(v: AbstractString) => stdlib.strToInt(v)
        case _                         => throw new EvaluationException("strToInt function arguments not matched")
      }
      case "strToBool" => actuals match {
        case List(v: AbstractString) => stdlib.strToBool(v)
        case _                         => throw new EvaluationException("strToBool function arguments not matched")
      }
      case "length" => actuals match {
        case List(v: AbstractString) => stdlib.length(v)
        case _                         => throw new EvaluationException("length function arguments not matched")
      }
      case "log" => actuals match {
        case List(v: AbstractString) => stdlib.log(v)
        case _                         => throw new EvaluationException("log function arguments not matched")
      }

      //readlib functions
      case "readString" => actuals match {
        case List(str: AbstractString) => readlib.readString(str)
        case _                           => throw new EvaluationException("readString function arguments not matched")
      }
      case "readInt" => actuals match {
        case List(str: AbstractString) => readlib.readInt(str)
        case _                           => throw new EvaluationException("readInt function arguments not matched")
      }
      case "readBool" => actuals match {
        case List(str: AbstractString) => readlib.readBool(str)
        case _                           => throw new EvaluationException("readBool function arguments not matched")
      }
      case "readIMEI" => readlib.readIMEI
      case "readUsrPwd" => actuals match {
        case List(str: AbstractString) => readlib.readUsrPwd(str)
        case _                           => throw new EvaluationException("readUsrPwd function arguments not matched")
      }
      case "readGeoLoc" => readlib.readGeoLoc
      case "readPhoneNum" => actuals match {
        case List(str: AbstractString) => readlib.readPhoneNum(str)
        case _                           => throw new EvaluationException("readPhoneNum function arguments not matched")
      }
      case "strInput"  => readlib.strInput
      case "boolInput" => readlib.boolInput
      case "intInput"  => readlib.intInput
      case _           => throw new EvaluationException("unrecognized native function")
    }
    res
  }

  /**
   * It replicates the tiny java stdlib (in resources)
   */
  object stdlib {

    /**
     * It encrypts the label with a give key
     * Notice: DUMMY ENCRYPTION!!!
     * @param label
     * @param key the encryption key
     * @return the encrypted label
     */
    def encrypt(label: AbstractString, key: AbstractString): AbstractString = AbstractStringFactory.top

    /*
    /**
     * Substring
     * @param str
     * @param beginChar
     * @param endChar
     * @return the result string
     */
    def substring(str: AbstractString, beginChar: AbstractNum, endChar: AbstractNum): AbstractString =
      str.takeUntil(endChar).dropUntil(beginChar)
    */

    /**
      * str.substring(0, endChar)
      * @param str
      * @param endChar
      * @return the result string
      */
    def takeUntil(str: AbstractString, endChar: AbstractNum): AbstractString =
      str.takeUntil(endChar)

    /**
      * str.substring(beginChar, str.length - 1)
      * @param str
      * @param beginChar
      * @return the result string
      */
    def dropUntil(str: AbstractString, beginChar: AbstractNum): AbstractString =
      str.dropUntil(beginChar)


    /**
     * Hash function.
     * @param str input string
     * @return the hash value in Array[Byte]
     */
    def hash(str: AbstractString) = AbstractStringFactory.top

    /**
     * Check if a password (string) is correct or not (string compare)
     * DUMMY FUNCTION
     * @param first password inserted by the user
     * @param second actual correct password
     * @return a boolean value, true if the two values are the same, false otherwise
     */
    def checkpwd(first: AbstractString, second: AbstractString): AbstractBool = first ==^ second

    /**
     * It retrieves the device IMEI.
     * DUMMY IMEI
     * Actually, generates a random number of 15 digits.
     * @return the IMEI from the datastore
     */
    def getDeviceID: AbstractString = {
      //FIXME: migliorabile
      AbstractStringFactory.top
    }

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
    //FIXME: Bottom instead of option?
    def strToInt(str: AbstractString): AbstractNum = { str.strToInt
      /*try {
        Some(str.toInt)
      }
      catch {
        case e: Exception => None
      }*/
    }

    /**
     * It converts a string to a boolean
     * @param str integer input argument
     * @return int
     */
    def strToBool(str: AbstractString): AbstractBool = { str.strToBool
      /*try {
        Some(str.toBoolean)
      }
      catch {
        case e: Exception => None
      }*/
    }

    /**
     * @param str input string
     * @return the dimension in integer of a string
     */
    def length(str: AbstractString): AbstractNum = str.length

    /**
     * It writes the argument to a log file.
     * Dummy function.
     * @param str
      FIXME: Unit instead of AbstractBool
     */
    def log(str: AbstractString) = AbstractBoolFactory.sTrueAt
  }

  /**
   *  It replicates the tiny java readlib (in resources)
   */
  object readlib {

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
      *         FIXME forse migliorabile...
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
