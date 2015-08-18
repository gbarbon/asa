/**
 * Simple-java basic library.
 * All methods are static.
 * @author gbarbon
 *
 */



/**
 * Read function, reads confidential labels from the datastore of the device.
 * @param __name label name
 * @return the confidential label from the datastore (string)
 */
@@ obf 0.00
@@ implq
string readString(string __name) {
	string label_content;
	return label_content;
}

/**
 * Read function, reads confidential labels from the datastore of the device.
 * @param __name label name
 * @return the confidential label from the datastore (int)
 */
@@ obf 0.00
@@ implq
int readInt(string __name) {
	int label_content;
	return label_content;
}

/**
 * It encrypts the label with a give key
 * @param __label 
 * @param __key the encryption key
 * @return the encrypted label
 */
@@ obf 10
@@ implq
string encrypt(string __label, string __key) {
	string result;
	return result;
}

/**
 * @FIXME: strin concatenation is now an operator
 * string concatenation
 * @param __firstString 
 * @param __secondString
 * @return the result string (the two original string concatenated)
 */
/*@@ obf
string concat(string __firstString, string __secondString) {
	string result;
	return result;
}*/

/**
 * Substring
 * @param __str
 * @param __beginChar
 * @param __endChar
 * @return the result string
 */
@@ obf
@@ implq
string substring(string __str, int __beginChar, int __endChar) {
	string result;
	return result;
}

/**
 * Hash function.
 * @param __inStr input string
 * @return the hash value
 */
@@ obf
@@ implq
string hash(string __inStr){
	string hash;
	return hash;
}

/**
 * Check if a password is correct or not (string compare)
 * @param __pwd password inserted by the user
 * @param __orig actual correct password
 * @return a boolean value, true if the two values are the same, false otherwise
 */
@@ obf
@@ implq
bool checkpwd(string __pwd, string __orig){
	bool result;
	return result;
}

/**
 * It reads the input from the keyboard
 * @return string
 */
@@ obf
@@ implq
string userinput() {
	string result;
	return result;
}

/**
 * It retrieves the device IMEI
 * @return the IMEI from the datastore
 */
@@ obf
@@ implq
int getDeviceID() {
	int IMEI;
	return IMEI;
}

/**
 * It converts an int to a string
 * @param __intArg integer input argument
 * @return string 
 */
@@ obf
@@ implq
string intToString(int __intArg) {
	string result;
	return result;
}

/**
 * It converts a bool to a string
 * @param __boolArg boolean input argument
 * @return string 
 */
@@ obf
@@ implq
string boolToString(bool __boolArg) {
	string result;
	return result;
}

/**
 * @param __str input string
 * @return the dimension in integer of a string 
 */
@@ obf
@@ implq
int length(string __str){
	int dim;
	return dim;
}

/**
 * It writes the argument to a log file
 * @param __str
 */
@@ obf
@@ implq
void log(string __str){
	
}

