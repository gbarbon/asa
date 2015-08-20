/**
 * Simple-java read library.
 * All methods are static.
 * @author gbarbon
 *
 */



/**
 * Read a generic string confidential label from the datastore of the device.
 * @param name the name of the label
 * @return the confidential label from the datastore (string)
 */
@@[labelName:genString; obf=L; dim:] 
string readString(string name) {
	string label_content;
	return label_content;
}

/**
 * Read a generic int confidential label from the datastore of the device.
 * @param name the name of the concrete value
 * @return the confidential label from the datastore (int)
 */
@@[labelName:genInt; obf=L; dim:]
int readInt(string name) {
	int label_content;
	return label_content;
}

/**
 * Read a generic boolean confidential label from the datastore of the device.
 * @param name the name of the concrete value
 * @return the confidential label from the datastore (bool)
 */
@@[labelName:genBool; obf=L; dim:]
bool readBool(string name) {
	bool label_content;
	return label_content;
}

/**
 * Read the IMEI
 * @return the device IMEI
 */
@@[labelName:IMEI; obf=L; dim:] 
int readIMEI() {
	int IMEI
	return IMEI
}

/**
 * Read the password
 * @param name the name of the user
 * @return the password
 */
@@[labelName:pwd; obf=L; dim:] 
string readUsrPwd(string usr) {
	string pwd
	return pwd
}

/**
 * Read the geographic position of the device
 * @return the geographic coordinates of the devices
 */
@@[labelName geoCoord; obf:L; dim:] 
string readGeoLoc() {
	string coords
	return coords
}

/**
 * Read the given contact from the address book
 * @return the geographic coordinates of the devices
 */
@@[labelName:phoneNum; obf:L; dim:]
string readGeoLoc(string contact) {
	string phoneNum
	return phoneNum
}

