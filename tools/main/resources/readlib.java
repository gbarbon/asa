/**
 * Simple-java read library.
 * All methods are static.
 * #nameFunction represents a call to the Scala function
 * @author gbarbon
 *
 */
class readlib {
	
	/**
	 * Read a generic string confidential label from the datastore of the device.
	 * @param name the name of the label
	 * @return the confidential label from the datastore (string)
	 */
	@@[labelName:"genString";conf:"L";dim:"0"]
	static string readString(string name) {
		return #readString(name);
	}
	
	/**
	 * Read a generic int confidential label from the datastore of the device.
	 * @param name the name of the concrete value
	 * @return the confidential label from the datastore (int)
	 */
	@@[labelName:"genInt";conf:"L";dim:"0"]
	static int readInt(string name) {
		return #readInt(name);
	}
	
	/**
	 * Read a generic boolean confidential label from the datastore of the device.
	 * @param name the name of the concrete value
	 * @return the confidential label from the datastore (bool)
	 */
	@@[labelName:"genBool";conf:"L";dim:"0"]
	static bool readBool(string name) {
		return #readBool(name);
	}
	
	/**
	 * Read the IMEI
	 * @return the device IMEI
	 */
	@@[labelName:"IMEI";conf:"H";dim:"0"] 
	static int readIMEI() {
		return #readIMEI();
	}
	
	/**
	 * Read the password
	 * @param name the name of the user
	 * @return the password
	 */
	//@FIXME: may return more than one possible label if we use different usr!
	@@[labelName:"pwd";conf:"H";dim:"0"] 
	static string readUsrPwd(string usr) {
		return #readUsrPwd(usr);
	}
	
	/**
	 * Read the geographic position of the device
	 * @return the geographic coordinates of the devices
	 */
	//@FIXME: this function returns more than one possible label!!
	@@[labelName:"geoCoord";conf:"M";dim:"0";molt:"10"] 
	static string readGeoLoc() {
		return #readGeoLoc();
	}
	
	/**
	 * Read the given contact from the address book
	 * @return the geographic coordinates of the devices
	 */
	//@FIXME: this function returns more than one possible label, depending on the parameter!!
	@@[labelName:"phoneNum";conf:"M";dim:"0"]
	static string readPhoneNum(string contact) {
		return #readPhoneNum(contact);
	}
}
