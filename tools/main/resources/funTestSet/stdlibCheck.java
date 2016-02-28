/**
 * Test for stdlib functions
 */
class StdlibCheck extends Activity {
	
	static void main() {

		String val, key, pwd, res, stringBool, stringInt;
		boolean bool, tmpbool;
		int intval, tmpint;
		val = readlib.readPreciseString();
		key = "theKey";
		pwd = readlib.readUsrPwd("theUsrName");
		tmpint = readlib.readPreciseInt();
		tmpbool = readlib.readPreciseBool();
		stringBool = "true";
		stringInt = "100";

		res = stdlib.encrypt(val, key);
		log(res);
		bool = stdlib.checkpwd(val, pwd);
		log(bool);
		res = stdlib.hash(val);
		log(res);
		res = stdlib.substring(val, 2, 4);
		log(res);
		bool = stdlib.strToBool("true");
		log(bool);
		bool = stdlib.strToBool(stringBool);
		log(bool);
		intval = stdlib.strToInt("100");
		log(intval);
		intval = stdlib.strToInt(stringInt);
		log(intval);
		res = stdlib.intToString(100);
		log(res);
		res = stdlib.intToString(tmpint);
		log(res);
		res = stdlib.boolToString(true);
		log(res);
		res = stdlib.boolToString(tmpbool);
		log(res);
		intval = stdlib.length(val);
		log(intval);
	}
}
