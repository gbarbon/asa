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
	@@[obf:"H";implq:"0"]
	//@FIXME: encrypt should be parametric on the obfuscation, it depends on the key
	string encrypt(string label, string key) {
		string result;
		return result;
	}
	
	/**
	 * @FIXME: strin concatenation is now an operator
	 * string concatenation
	 * @param firstString 
	 * @param secondString
	 * @return the result string (the two original string concatenated)
	 */
	/*@@[obf:;implq:]
	string concat(string firstString, string secondString) {
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
	@@[obf:"M";implq:"0"]
	string substring(string str, int beginChar, int endChar) {
		string result;
		return result;
	}
	
	/**
	 * Hash function.
	 * @param inStr input string
	 * @return the hash value
	 */
	@@[obf:"M";implq:"0"]
	string hash(string inStr){
		string hash;
		return hash;
	}
	
	/**
	 * Check if a password is correct or not (string compare)
	 * @param pwd password inserted by the user
	 * @param orig actual correct password
	 * @return a boolean value, true if the two values are the same, false otherwise
	 */
	//@FIXME: what is the obfuscation of the checkpwd??
	@@[obf:"H";implq:"1"]
	bool checkpwd(string pwd, string orig){
		bool result;
		return result;
	}
	
	/**
	 * It reads the input from the keyboard
	 * @return string
	 */
	//@FIXME: no obfuscation, no imlicit values, only read from user input!
	@@[obf:"";implq:""]
	string strInput() {
		string result;
		return result;
	}
	
	/**
	 * It reads the input from the keyboard
	 * @return bool
	 */
	//@FIXME: no obfuscation, no imlicit values, only read from user input!
	@@[obf:"";implq:""]
	bool boolInput() {
		bool result;
		return result;
	}
	
	/**
	 * It reads the input from the keyboard
	 * @return int
	 */
	//@FIXME: no obfuscation, no imlicit values, only read from user input!
	@@[obf:"";implq:""]
	int intInput() {
		int result;
		return result;
	}
	
	/**
	* It retrieves the device IMEI
	* @return the IMEI from the datastore
	*/
	//@FIXME: substituted by the readIMEI in the readlib
	/**
	@@[obf:"";implq:""]
	int getDeviceID() {
		int IMEI;
		return IMEI;
	}
	*/
	
	
	
	/**
	 * It converts an int to a string
	 * @param intArg integer input argument
	 * @return string 
	 */
	@@[obf:"L";implq:"0"]
	string intToString(int intArg) {
		string result;
		return result;
	}
	
	/**
	 * It converts a bool to a string
	 * @param boolArg boolean input argument
	 * @return string 
	 */
	@@[obf:"L";implq:"0"]
	string boolToString(bool boolArg) {
		string result;
		return result;
	}
	
	/**
	 * It converts a string to an int
	 * @param str integer input argument
	 * @return int 
	 */
	@@[obf:"L";implq:"0"]
	int strToInt(string str) {
		int result;
		return result;
	}
	
	/**
	 * It converts a string to a boolean
	 * @param str integer input argument
	 * @return int 
	 */
	@@[obf:"L";implq:"1"]
	bool strToBool(string str) {
		bool result;
		return result;
	}
	
	/**
	 * @param str input string
	 * @return the dimension in integer of a string 
	 */
	@@[obf:"M";implq:"0"]
	int length(string str){
		int dim;
		return dim;
	}
	
	/**
	 * It writes the argument to a log file
	 * @param str
	 */
	@@[obf:"L";implq:"0"]
	void log(string str){
		
	}
}
