/**
 * Simple-java basic library.
 * All methods are static.
 * @author gbarbon
 *
 */
class stdlib {
	
	/**
	 * It encrypts the label with a give key
	 * @param label 
	 * @param key the encryption key
	 * @return the encrypted label
	 */
	@@[name:"encrypt";obf:"H"]
	static	String encrypt(String label, String key) {
		return #encrypt(label, key);
	}

	/**
	 * Prefix
	 * @param str
	 * @param endChar
	 * @return the prefix of str
	 */
	@@[name:"prefix";obf:"M"]
	static	String prefix(String str, int endChar) {
		return #prefix(str, endChar);
	}

	/**
	 * Suffix
	 * @param str
	 * @param beginChar
	 * @return the suffix of str
	 */
	@@[name:"suffix";obf:"M"]
	static	String suffix(String str, int beginChar) {
		return #suffic(str, beginChar);
	}

	/**
	 * Substring
	 * @param str
	 * @param beginChar
	 * @param endChar
	 * @return the result String
	 */
	static	String substring(String str, int beginChar, int endChar) {
		String first;
		first = prefix(str, endChar);
		return suffix(first, beginChar);
	}

	/**
	 * Hash function.
	 * @param inStr input String
	 * @return the hash value
	 */
	@@[name:"hash";obf:"M"]
	static	String hash(String inStr){
		return #hash(inStr);
	}

	/**
	 * Check if a password is correct or not (String compare)
	 * @param pwd password inserted by the user
	 * @param orig actual correct password
	 * @return a boolean value, true if the two values are the same, false otherwise
	 */
	@@[name:"checkpwd";obf:"H"]
	static	bool checkpwd(String pwd, String orig){
		return #checkpwd(pwd, orig);
	}

	/**
	 * It converts an int to a String
	 * @param intArg integer input argument
	 * @return String
	 */
	@@[name:"intToString";obf:"L"]
	static	String intToString(int intArg) {
		return #intToString(intArg);
	}

	/**
	 * It converts a bool to a String
	 * @param boolArg boolean input argument
	 * @return String
	 */
	@@[name:"boolToString";obf:"L"]
	static	String boolToString(bool boolArg) {
		return #boolToString(boolArg);
	}

	/**
	 * It converts a String to an int
	 * @param str integer input argument
	 * @return int
	 */
	@@[name:"strToInt";obf:"L"]
	static	int strToInt(String str) {
		return #strToInt(str);
	}

	/**
	 * It converts a String to a boolean
	 * @param str integer input argument
	 * @return int
	 */
	@@[name:"strToBool";obf:"L"]
	static	bool strToBool(String str) {
		return #strToBool(str);
	}

	/**
	 * @param str input String
	 * @return the dimension in integer of a String
	 */
	@@[name:"length";obf:"M"]
	static	int length(String str){
		return #length(str);
	}

}
