/**
 * Simple-java basic library.
 * All methods are static.
 * @author gbarbon
 *
 */



/**
 * Read function, reads confidential labels from the datastore of the device.
 * @param label name
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
 * @param label name
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
 * @param label 
 * @param key the encryption key
 * @return the encrypted label
 */
@@ obf 10
@@ implq
string encrypt(string label, string key) {
	string result;
	return result;
}

/**
 * @FIXME: strin concatenation is now an operator
 * string concatenation
 * @param first_string 
 * @param second_string
 * @return the result string (the two original string concatenated)
 */
/*@@ obf
string concat(string first_string, string second_string) {
	string result;
	return result;
}*/

/**
 * Substring
 * @param first_string 
 * @param second_string
 * @return the result string
 */
@@ obf
@@ implq
string substring(string first_string, string second_string) {
	string result;
	return result;
}

/**
 * Hash function.
 * @param input string
 * @return the hash value
 */
@@ obf
@@ implq
string hash(string input){
	string hash;
	return hash;
}

/**
 * Check if a password is correct or not (string compare)
 * @param pwd password inserted by the user
 * @param orig actual correct password
 * @return a boolean value, true if the two values are the same, false otherwise
 */
@@ obf
@@ implq
bool checkpwd(string pwd, string orig){
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
 * @param arg input argument
 * @return string 
 */
@@ obf
@@ implq
string toString(int arg) {
	string result;
	return result;
}

/**
 * @param s input string
 * @return the dimension in integer of a string 
 */
@@ obf
@@ implq
int length(string s){
	int dim;
	return dim;
}

/**
 * It writes the argument to a log file
 * @param str
 */
@@ obf
@@ implq
void log(string str){
	
}

