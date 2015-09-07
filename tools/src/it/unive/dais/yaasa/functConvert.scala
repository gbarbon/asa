package it.unive.dais.yaasa

/**
 * @author gbarbon
 */

import java.security.MessageDigest

/**
 * It contains functions conversion from the tiny java to scala
 */
object functConvert {

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
    def encrypt(label: String, key: String): String = label.concat(key)

    /**
     * Substring
     * @param str
     * @param beginChar
     * @param endChar
     * @return the result string
     */
    def substring(str: String, beginChar: Int, endChar: Int): String =
      str.drop(beginChar).take(endChar)

    /**
     * Hash function.
     * @param str input string
     * @return the hash value in Array[Byte]
     */
    def hash(str: String) =
      MessageDigest.getInstance("MD5").digest(str.getBytes)

    /**
     * Check if a password (string) is correct or not (string compare)
     * DUMMY FUNCTION
     * @param first password inserted by the user
     * @param second actual correct password
     * @return a boolean value, true if the two values are the same, false otherwise
     */
    def checkpwd(first: String, second: String): Boolean =
      (first == second)

    /**
     * It reads the input from the keyboard
     * @return string
     */
    def strInput = readLine()

    /**
     * It reads the input from the keyboard
     * @return bool
     */
    def boolInput = strToBool(strInput)

    /**
     * It reads the input from the keyboard
     * @return int
     */
    def intInput = strToInt(strInput)

    /**
     * It retrieves the device IMEI.
     * DUMMY IMEI
     * Actually, generates a random number of 15 digits.
     * @return the IMEI from the datastore
     */
    def getDeviceID {
      val range = 100000000000000L to 999999999999999L
      val rnd = new scala.util.Random
      range(rnd.nextInt(range length))
    }

    /**
     * It converts an int to a string
     * @param intArg integer input argument
     * @return string
     */
    def intToString(intArg: Int): String = intArg.toString()

    /**
     * It converts a boolean to a string
     * @param boolArg boolean input argument
     * @return string
     */
    def boolToString(boolArg: Boolean): String =
      if (boolArg) "true"
      else "false"

    /**
     * It converts a string to an int
     * @param str integer input argument
     * @return int
     */
    def strToInt(str: String): Option[Int] = {
      try {
        Some(str.toInt)
      }
      catch {
        case e: Exception => None
      }
    }

    /**
     * It converts a string to a boolean
     * @param str integer input argument
     * @return int
     */
    def strToBool(str: String): Option[Boolean] = {
      try {
        Some(str.toBoolean)
      }
      catch {
        case e: Exception => None
      }
    }

    /**
     * @param str input string
     * @return the dimension in integer of a string
     */
    def length(str: String): Int = str.length()

    /**
     * It writes the argument to a log file.
     * Dummy function.
     * @param str
     */
    def log(str: String) = ()
  }

  /**
   *  It replicates the tiny java readlib (in resources)
   *  @FIXME: all dummy methods, please fix with working ones!!
   */
  object readlib {

    /**
     * Read a generic string confidential label from the datastore of the device.
     * @param name the name of the label
     * @return the confidential label from the datastore (string)
     */
    def readString(name: String): String = {
      val label_content = "blabla"
      label_content
    }

    /**
     * Read a generic int confidential label from the datastore of the device.
     * @param name the name of the concrete value
     * @return the confidential label from the datastore (int)
     */
    def readInt(name: String): Int = {
      val label_content = 0
      label_content
    }

    /**
     * Read a generic boolean confidential label from the datastore of the device.
     * @param name the name of the concrete value
     * @return the confidential label from the datastore (bool)
     */
    def readBool(name: String): Boolean = {
      val label_content = true
      label_content
    }

    /**
     * Read the IMEI
     * @return the device IMEI
     */
    def readIMEI(): Long = {
      var IMEI = 12345678912345L
      IMEI
    }

    /**
     * Read the password
     * @param name the name of the user
     * @return the password
     */
    def readUsrPwd(usr: String): String = {
      val pwd = ""
      pwd
    }

    /**
     * Read the geographic position of the device
     * @return the geographic coordinates of the devices
     */
    def readGeoLoc(): String = {
      val coords = ""
      coords
    }

    /**
     * Read the given contact from the address book
     * @return the geographic coordinates of the devices
     */
    def readPhoneNum(contact: String): String = {
      val phoneNum = ""
      phoneNum
    }
  }

  /**
   * It replicates the operators behaviour in Scala
   * @FIXME: DISCARDED, as already present in the parser
   */
  /**
   * class operators {
   *
   * /**
   * Sum operator
   * @param first
   * @param second
   * @return first + second, integer
   * */
   * def BOPlus(first: Int, second: Int) = first + second
   *
   * /**
   * Minus operator
   * @param first
   * @param second
   * @return first + second, integer
   * */
   * def BOMinus(first: Int, second: Int) = first + second
   *
   * /**
   * Multiplier operator
   * @param first
   * @param second
   * @return first * second, integer
   * */
   * def BOMul(first: Int, second: Int) = first * second
   *
   * /**
   * Divisor operator
   * @param first
   * @param second
   * @return first / second, integer
   * @FIXME: do we need cast??
   * */
   * def BODiv(first: Int, second: Int) = first / second
   *
   * /**
   * Boolean AND operator
   * @param first
   * @param second
   * @return first && second, boolean
   * */
   * def BOAnd(first: Boolean, second: Boolean) = first && second
   *
   * /**
   * Boolean OR operator
   * @param first
   * @param second
   * @return first || second, boolean
   * */
   * def BOOr(first: Boolean, second: Boolean) = first || second
   *
   * /**
   * Modulo operator
   * @param first
   * @param second
   * @return first % second, boolean
   * */
   * def BOMod(first: Int, second: Int) = first % second
   *
   * /**
   * Less than operator
   * @param first
   * @param second
   * @return first < second, boolean
   * */
   * def BOLt(first: Int, second: Int) = first < second
   *
   * /**
   * Less than or equal operator
   * @param first
   * @param second
   * @return first <= second, boolean
   * */
   * def BOLeq(first: Int, second: Int) = first <= second
   *
   * /**
   * Equality operator
   * @param first
   * @param second
   * @return first == second, boolean
   * */
   * def BOEq(first: Int, second: Int) = first == second
   *
   * /**
   * Greather than operator
   * @param first
   * @param second
   * @return first > second, boolean
   * */
   * def BOGt(first: Int, second: Int) = first > second
   *
   * /**
   * Greather than or equal operator
   * @param first
   * @param second
   * @return first >= second, boolean
   * */
   * def BOGeq(first: Int, second: Int) = first >= second
   *
   * /**
   * Not equal operator
   * @param first
   * @param second
   * @return first != second, boolean
   * */
   * def BONeq(first: Int, second: Int) = first != second
   *
   * /**
   * String concatenation operator
   * @param first
   * @param second
   * @return concatenated strings
   * */
   * def BOPlusPlus(first: String, second: String) = first.concat(second)
   *
   * /**
   * Not operator
   * @param arg
   * @return not argument, boolean
   * */
   * def UNot(arg: Boolean) = !arg
   *
   * /**
   * Negation operator
   * @param arg
   * @return negated argument, integer
   * */
   * def UNeg(arg: Int) = -arg
   * }
   */
}
