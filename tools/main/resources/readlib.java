/**
 * Simple-java read library.
 * All methods are static.
 * @author gbarbon
 *
 */
class readlib {
	
	/**
	 * Read a generic string confidential label from the datastore of the device.
	 * @param name the name of the label
	 * @return the confidential label from the datastore (string)
	 */
	@@[labelName:"genString";conf:"L";dim:""] 
	string readString(string name) {
		string label_content;
		return label_content;
	}
	
	/**
	 * Read a generic int confidential label from the datastore of the device.
	 * @param name the name of the concrete value
	 * @return the confidential label from the datastore (int)
	 */
	@@[labelName:"genInt";conf:"L";dim:""]
	int readInt(string name) {
		int label_content;
		return label_content;
	}
	
	/**
	 * Read a generic boolean confidential label from the datastore of the device.
	 * @param name the name of the concrete value
	 * @return the confidential label from the datastore (bool)
	 */
	@@[labelName:"genBool";conf:"L";dim:""]
	bool readBool(string name) {
		bool label_content;
		return label_content;
	}
	
	/**
	 * Read the IMEI
	 * @return the device IMEI
	 */
	@@[labelName:"IMEI";conf:"H";dim:""] 
	int readIMEI() {
		int IMEI
		return IMEI
	}
	
	/**
	 * Read the password
	 * @param name the name of the user
	 * @return the password
	 */
	@@[labelName:"pwd";conf:"H";dim:""] 
	string readUsrPwd(string usr) {
		string pwd
		return pwd
	}
	
	/**
	 * Read the geographic position of the device
	 * @return the geographic coordinates of the devices
	 */
	//@FIXME: this function returns more than one possible label!!
	@@[labelName:"geoCoord";conf:"M";dim:""] 
	string readGeoLoc() {
		string coords
		return coords
	}
	
	/**
	 * Read the given contact from the address book
	 * @return the geographic coordinates of the devices
	 */
	//@FIXME: this function returns more than one possible label, depending on the parameter!!
	@@[labelName:"phoneNum";conf:"M";dim:""]
	string readPhoneNum(string contact) {
		string phoneNum
		return phoneNum
	}
}
