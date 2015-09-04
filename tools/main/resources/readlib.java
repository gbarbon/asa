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
	@@[name:"genString";conf:"L";dim:""] 
	string readString(string name) {
		return #readString(name);
	}
	
	/**
	 * Read a generic int confidential label from the datastore of the device.
	 * @param name the name of the concrete value
	 * @return the confidential label from the datastore (int)
	 */
	@@[name:"genInt";conf:"L";dim:""]
	int readInt(string name) {
		return #readInt(name);
	}
	
	/**
	 * Read a generic boolean confidential label from the datastore of the device.
	 * @param name the name of the concrete value
	 * @return the confidential label from the datastore (bool)
	 */
	@@[name:"genBool";conf:"L";dim:""]
	bool readBool(string name) {
		return #readBool(name);
	}
	
	/**
	 * Read the IMEI
	 * @return the device IMEI
	 */
	@@[name:"IMEI";conf:"H";dim:""] 
	int readIMEI() {
		return #readIMEI();
	}
	
	/**
	 * Read the password
	 * @param name the name of the user
	 * @return the password
	 */
	//@FIXME: may return more than one possible label if we use different usr!
	@@[name:"pwd";conf:"H";dim:""] 
	string readUsrPwd(string usr) {
		return #readUsrPwd(usr);
	}
	
	/**
	 * Read the geographic position of the device
	 * @return the geographic coordinates of the devices
	 */
	//@FIXME: this function returns more than one possible label!!
	@@[name:"geoCoord";conf:"M";dim:""] 
	string readGeoLoc() {
		return #readGeoLoc();
	}
	
	/**
	 * Read the given contact from the address book
	 * @return the geographic coordinates of the devices
	 */
	//@FIXME: this function returns more than one possible label, depending on the parameter!!
	@@[name:"phoneNum";conf:"M";dim:""]
	string readPhoneNum(string contact) {
		return #readPhoneNum(contact);
	}
}
