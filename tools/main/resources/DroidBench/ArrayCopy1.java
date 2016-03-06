//package edu.mit.array_copy;
//
//import android.app.Activity;
//import android.os.Bundle;
//import android.telephony.TelephonyManager;
//import android.util.Log;

/**
 * @testcase_name ArrayCopy
 * 
 * @description Testing System.arraycopy()
 * @dataflow source -> sink
 * @number_of_leaks 1
 * @challenges - The analysis tool must have a model for System.arraycopy()
 */
class MainActivity extends Activity {

    //@Override
    static void onCreate(Bundle savedInstanceState) {
        //super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_main);
         
        //TelephonyManager mgr = (TelephonyManager) this.getSystemService(TELEPHONY_SERVICE);
        String imei;
        String[] array, arraycopy;
        imei = TelephonyManager.getDeviceId(); //source
        array = new String[1];
        array[0] = imei;
        arraycopy = new String[1];
        System.arraycopy(array, arraycopy);
        
        Log.i("DroidBench", arraycopy[0]); //sink
    }
}
