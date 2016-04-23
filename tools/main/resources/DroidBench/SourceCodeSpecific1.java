//package de.ecspride;
//
//import java.util.HashSet;
//import java.util.Set;
//
//import android.app.Activity;
//import android.content.Context;
//import android.os.Bundle;
//import android.telephony.SmsManager;
//import android.telephony.TelephonyManager;
/**
 * @testcase_name SourceCodeSpecific1
 * @version 0.1
 * @author Secure Software Engineering Group (SSE), European Center for Security and Privacy by Design (EC SPRIDE) 
 * @author_mail siegfried.rasthofer@cased.de
 * 
 * @description tainted data is created in a condition branch and afterwards sent to a sink in a loop
 * @dataflow source -> message -> sink
 * @number_of_leaks 1
 * @challenges the analysis must handle standard java constructs
 */
class MainActivity extends Activity {

    //@Override
    static void onCreate(Bundle savedInstanceState) {
        //super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_main);
        //TelephonyManager telephonyManager = (TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE);
		int a, b;
		String message;
		String[] phoneNumbers;


		//Set<String> phoneNumbers = new HashSet<String>();
		phoneNumbers = new String[5];
		phoneNumbers[0] = "+49 123456";
		phoneNumbers[1] = "+49 654321";
		phoneNumbers[2] = "+49 111111";
		phoneNumbers[3] = "+49 222222";
		phoneNumbers[4] = "+49 333333";
		
		a = 22 + 11;
		b = 22 * 2 - 1 + a;
		
		//String message = (a == b) ? "no taint" : telephonyManager.getDeviceId(); //source
		if (a==b) {
			message = "no taint";
		}
		else {
			message = TelephonyManager.getDeviceId(); //source
		}
		
		sendSMS(phoneNumbers, message);		
	}
	
	static void sendSMS(String[] numbers, String message){
		//SmsManager sm = SmsManager.getDefault();

		int idx;
		idx = 0;
		while (idx < len(numbers)) {
			SmsManager.sendTextMessage(numbers[idx], "", message, "", ""); //sink
			idx = idx + 1;
		}
	}
}
