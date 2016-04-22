//package de.ecspride;
//
//import android.app.Activity;
//import android.content.Context;
//import android.os.Bundle;
//import android.telephony.SmsManager;
//import android.telephony.TelephonyManager;
/**
 * @testcase_name ArrayAccess2
 * @version 0.1
 * @author Secure Software Engineering Group (SSE), European Center for Security and Privacy by Design (EC SPRIDE) 
 * @author_mail siegfried.rasthofer@cased.de
 * 
 * @description an array is created which is filled with untainted and tainted (deviceId source) data.
 *   The untainted data of a calculated array position is retrieved and sent via sms.
 * @dataflow -
 * @number_of_leaks 0
 * @challenges the analysis must distinguish between different array positions and has to evaluate the function to recognize that the tainted
 *  data does not get leaked. 
 */

class ArrayAccess2 extends Activity {

	static void onCreate(Bundle savedInstanceState) {

        String[] array;
		array = new String[10];
		//TelephonyManager telephonyManager = (TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE); //source
		array[5] = TelephonyManager.getDeviceId();
		array[4] = "no taint";
		
		//SmsManager sm = SmsManager.getDefault();
		SmsManager.sendTextMessage("+49 1234", "", array[calculateIndex()], "", ""); //sink, no leak
	}
	
	static int calculateIndex(){
		int index;
		index = 1;
		index = index + 1;
		index = index * 5;
		index = index % 10;
		index = index + 4;
		
		return index;
	}
}
