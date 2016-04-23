//package edu.mit.to_string;
//
//import java.util.Arrays;
//
//import android.app.Activity;
//import android.os.Bundle;
//import android.telephony.TelephonyManager;
//import android.util.Log;


/**
 * @testcase_name ToString
 * 
 * @description Test underlying api calls to an objects toString() method
 * @dataflow source -> sink
 * @number_of_leaks 1
 * @challenges - Have to model that Array.toString invokes toString() for each object of array
 */
class Arrays { // sems to be unable to read the array
    static String toString(String[] array) {
        String str;
        int idx;
        str = "";
        idx = 0;
        while (idx < len(array)) {
            str = str ++ array[idx];
            idx = idx + 1;
        }
        return str;
    }
}
class MainActivity extends Activity {

    //@Override
    static void onCreate(Bundle savedInstanceState) {
        //super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_main);

        //TelephonyManager mgr = (TelephonyManager) this.getSystemService(TELEPHONY_SERVICE);
        String imei;
        String[] array;
        String arrayToString;
        imei = TelephonyManager.getDeviceId();

        array = new String[10]; // was 1 on DroidBench, why? it was indexOutOfBounds exception...
        
        array[1] = imei;

        arrayToString = Arrays.toString(array);
        
        Log.i("DroidBench", arrayToString);
    }
}
