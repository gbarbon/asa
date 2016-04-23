//package edu.mit.string_to_char;
//
//import android.app.Activity;
//import android.os.Bundle;
//import android.telephony.TelephonyManager;
//import android.util.Log;

/**
 * @testcase_name String-to-Char
 * 
 * @description Test conversion of String to char[]
 * @dataflow source -> sink
 * @number_of_leaks 1
 * @challenges  The analysis tool has to be able to follow taint through character-string conversion
 */
class MainActivity extends Activity {

    //@Override
    static void onCreate(Bundle savedInstanceState) {
        //super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_main);
         
        //TelephonyManager mgr = (TelephonyManager) this.getSystemService(TELEPHONY_SERVICE);
	    String builtImei, imei;
        String[] chars;
        int idx;
        imei = TelephonyManager.getDeviceId();  //source
        //char[] chars = new char[imei.length()];

        //imei.getChars(0, imei.length(), chars, 0);
        chars = toCharArray(imei);

        builtImei = "";
        idx = 0;
        while (idx < len(chars)) {
            builtImei = builtImei ++ chars[idx];
            idx = idx + 1;
        }
                
        Log.i("DroidBench", builtImei);  //sink, leak
    }
}
