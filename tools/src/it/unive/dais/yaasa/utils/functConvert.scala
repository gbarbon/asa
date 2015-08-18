package it.unive.dais.yaasa.utils

/**
 * @author gbarbon
 */

import java.security.MessageDigest

object functConvert {

  class stdlib {

    /**
     * Read function, reads confidential labels from the datastore of the device.
     * @param name label name
     * @return the confidential label from the datastore (string)
     */
    //def readString(name: String)
    //string label_content;
    //return label_content;

    /**
     * Read function, reads confidential labels from the datastore of the device.
     * @param name label name
     * @return the confidential label from the datastore (int)
     */
    //def readInt(name: String)

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
    def userinput = readLine()

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
    def intToStrin(intArg: Int): String = intArg.toString()

    /**
     * It converts a boolean to a string
     * @param boolArg boolean input argument
     * @return string
     */
    def boolToString(boolArg: Boolean): String =
      if (boolArg) "true"
      else "false"

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
    def log(str: String) { val res = str }
  }
}
