/**
 * Simple-java read library.
 * All methods are static.
 * #nameFunction represents a call to the Scala function
 * @author gbarbon
 *
 */
class readlib {


	/**
	 * the following are testing methods
	 *
	 */
	@@[labelName:"readPreciseString";conf:"L";dim:"0"]
	static String readPreciseString() {
		return #readPreciseString();
	}
	@@[labelName:"readPreciseInt";conf:"L";dim:"0"]
	static int readPreciseInt() {
		return #readPreciseInt();
	}
	@@[labelName:"readPreciseBool";conf:"L";dim:"0"]
	static boolean readPreciseBool() {
		return #readPreciseBool();
	}
	@@[labelName:"readPreciseIMEI";conf:"H";dim:"0"]
	static String readPreciseIMEI() {
		return #readPreciseIMEI();
	}
	
	/**
	 * Read a generic String confidential label from the datastore of the device.
	 * @param name the name of the label
	 * @return the confidential label from the datastore (String)
	 */
	@@[labelName:"genString";conf:"L";dim:"0"]
	static String readString(String name) {
		return #readString(name);
	}
	
	/**
	 * Read a generic int confidential label from the datastore of the device.
	 * @param name the name of the concrete value
	 * @return the confidential label from the datastore (int)
	 */
	@@[labelName:"genInt";conf:"L";dim:"0"]
	static int readInt(String name) {
		return #readInt(name);
	}
	
	/**
	 * Read a generic boolean confidential label from the datastore of the device.
	 * @param name the name of the concrete value
	 * @return the confidential label from the datastore (bool)
	 */
	@@[labelName:"genBool";conf:"L";dim:"0"]
	static boolean readBool(String name) {
		return #readBool(name);
	}
	
	/**
	 * Read the IMEI
	 * @return the device IMEI
	 */
	// testing dim:  6 char * 15 elements = 90 (for dim)
	@@[labelName:"IMEI";conf:"H";dim:"90"] 
	static String readIMEI() {
		return #readIMEI();
	}
	
	/**
	 * Read the password
	 * @param name the name of the user
	 * @return the password
	 */
	//@FIXME: may return more than one possible label if we use different usr!
	// testing dim:  6 char * 10 elements = 60 (for dim), but we want something dependent on the real String!!!
	@@[labelName:"pwd";conf:"H";dim:"60"] 
	static String readUsrPwd(String usr) {
		return #readUsrPwd(usr);
	}
	
	/**
	 * Read the geographic position of the device
	 * @return the geographic coordinates of the devices
	 */
	//@FIXME: this function returns more than one possible label!!
	@@[labelName:"geoCoord";conf:"M";dim:"0";molt:"10"] 
	static String readGeoLoc() {
		return #readGeoLoc();
	}
	
	/**
	 * Read the given contact from the address book
	 * @return the geographic coordinates of the devices
	 */
	//@FIXME: this function returns more than one possible label, depending on the parameter!!
	@@[labelName:"phoneNum";conf:"M";dim:"0"]
	static String readPhoneNum(String contact) {
		return #readPhoneNum(contact);
	}
	
	/**
	 * It reads the input from the keyboard
	 * @return String
	 */
	@@[labelName:"star";conf:"L";dim:"0"]
	static	String strInput() {
		return #strInput();
	}
	
	/**
	 * It reads the input from the keyboard
	 * @return bool
	 */
	@@[labelName:"star";conf:"L";dim:"0"]
	static	boolean boolInput() {
		return #boolInput(boolInput);
	}
	
	/**
	 * It reads the input from the keyboard
	 * @return int
	 */
	@@[labelName:"star";conf:"L";dim:"0"]
	static	int intInput() {
		return #intInput();
	}
}

/**
 * THe following classes mimicks Android classes
 */

class TelephonyManager {
	static String getDeviceId() {
		return readlib.readPreciseIMEI();
	}
}
