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
	@@[name:"encrypt";obf:"H";implq:"0"]
	//@FIXME: encrypt should be parametric on the obfuscation, it depends on the key
	static	string encrypt(string label, string key) {
		return #encrypt(label, key);
	}
	
	/**
	 * @FIXME: strin concatenation is now an operator
	 * string concatenation
	 * @param firstString 
	 * @param secondString
	 * @return the result string (the two original string concatenated)
	 */
	/*@@[name:"concat";obf:;implq:]
 static	string concat(string firstString, string secondString) {
		string result;
		return result;
	}*/
	
	/**
	 * Substring
	 * @param str
	 * @param beginChar
	 * @param endChar
	 * @return the result string
	 */
	//@FIXME: substring should be parametric on the obfuscation
	@@[name:"substring";obf:"M";implq:"0"]
	static	string substring(string str, int beginChar, int endChar) {
		return #substring(str, beginChar, endChar);
	}
	
	/**
	 * Hash function.
	 * @param inStr input string
	 * @return the hash value
	 */
	@@[name:"hash";obf:"M";implq:"0"]
	static	string hash(string inStr){
		return #hash(inStr);
	}
	
	/**
	 * Check if a password is correct or not (string compare)
	 * @param pwd password inserted by the user
	 * @param orig actual correct password
	 * @return a boolean value, true if the two values are the same, false otherwise
	 */
	//@FIXME: what is the obfuscation of the checkpwd??
	@@[name:"checkpwd";obf:"H";implq:"1"]
	static	bool checkpwd(string pwd, string orig){
		return #checkpwd(pwd, orig);
	}
	
	/**
	* It retrieves the device IMEI
	* @return the IMEI from the datastore
	*/
	//@FIXME: substituted by the readIMEI in the readlib
	/**
	@@[name:"";obf:"";implq:""]
 static	int getDeviceID() {
		int IMEI;
		return IMEI;
	}
	*/
	
	/**
	 * It converts an int to a string
	 * @param intArg integer input argument
	 * @return string 
	 */
	@@[name:"intToString";obf:"L";implq:"0"]
	static	string intToString(int intArg) {
		return #intToString(intArg);
	}
	
	/**
	 * It converts a bool to a string
	 * @param boolArg boolean input argument
	 * @return string 
	 */
	@@[name:"boolToString";obf:"L";implq:"0"]
	static	string boolToString(bool boolArg) {
		return #boolToString(boolArg);
	}
	
	/**
	 * It converts a string to an int
	 * @param str integer input argument
	 * @return int 
	 */
	@@[name:"strToInt";obf:"L";implq:"0"]
	static	int strToInt(string str) {
		return #strToInt(str);
	}
	
	/**
	 * It converts a string to a boolean
	 * @param str integer input argument
	 * @return int 
	 */
	@@[name:"strToBool";obf:"L";implq:"1"]
	static	bool strToBool(string str) {
		return #strToBool(str);
	}
	
	/**
	 * @param str input string
	 * @return the dimension in integer of a string 
	 */
	@@[name:"length";obf:"M";implq:"0"]
	static	int length(string str){
		return #length(str);
	}
	
	/**
	 * It writes the argument to a log file
	 * @param str
	 */
	@@[name:"log";obf:"L";implq:"0"]
	static	void log(string str){
		#log(str);
	}
}
