//package de.ecspride;
//
//import android.app.Activity;
//import android.content.Context;
//import android.os.Bundle;
//import android.telephony.SmsManager;
//import android.telephony.TelephonyManager;
/**
 * @testcase_name Loop2
 * @version 0.1
 * @author Secure Software Engineering Group (SSE), European Center for Security and Privacy by Design (EC SPRIDE) 
 * @author_mail siegfried.rasthofer@cased.de
 * 
 * @description tainted data is created and sent to a sink after it was transformed in a loop.
 * @dataflow source -> imei -> obfuscated -> sink
 * @number_of_leaks 1
 * @challenges the analysis must handle standard java constructs
 */

class LoopExample2 extends Activity {

    protected void onCreate(Bundle savedInstanceState) {
        //super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_loop_example2);
        
        //TelephonyManager telephonyManager = (TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE);
		String imei, obfuscated;
		String[] imeiArray;
		int idx, idx2;
		imei = TelephonyManager.getDeviceId(); //source

		obfuscated = "";
		imeiArray = toCharArray(imei);
		idx = 0;

		while (idx < 10) {
			if(idx == 9) {
				idx2 = 0;
				while (idx2 < len(imeiArray)) {
					obfuscated = obfuscated ++ imeiArray[idx2];
					obfuscated = obfuscated ++ "_";
					idx2 = idx2 + 1;
				}
			}
			idx = idx + 1;
		}
		
		//SmsManager sm = SmsManager.getDefault();

		SmsManager.sendTextMessage("+49 1234", "", obfuscated, "", ""); //sink, leak
    }    
}
