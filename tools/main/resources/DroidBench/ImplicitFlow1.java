//package de.ecspride;

//import android.app.Activity;
//import android.content.Context;
//import android.os.Bundle;
//import android.telephony.TelephonyManager;
//import android.util.Log;
/**
 * @testcase_name ImplicitFlow1
 * @version 0.1
 * @author Secure Software Engineering Group (SSE), European Center for Security and Privacy by Design (EC SPRIDE)
 * @author_mail siegfried.rasthofer@cased.de
 *
 * @description A value from a source gets obfuscated by two different ways and is then written to the log
 * @dataflow source -> userInputPassword -> if-condition -> sink
 * @number_of_leaks 2
 * @challenges the analysis must be able to handle implicit flows and
 *  treat the value of password fields as source
 */
class ImplicitFlow1 extends Activity {

    static String obfuscateIMEI(String imei){
        String result, tmp;
        int idx;
        String[] array;
        result = "";
        idx = 0;

        array = toCharArray(imei);
        while (idx < stdlib.length(imei)) {
            tmp = array[idx];
            if (tmp == "0")
                result = result ++ "a";
            elif (tmp == "1")
                result = result ++ "b";
            elif (tmp == "2")
                result = result ++ "c";
            elif (tmp == "3")
                result = result ++ "d";
            elif (tmp == "4")
                result = result ++ "e";
            elif (tmp == "5")
                result = result ++ "f";
            elif (tmp == "6")
                result = result ++ "g";
            elif (tmp == "7")
                result = result ++ "h";
            elif (tmp == "8")
                result = result ++ "i";
            elif (tmp == "9")
                result = result ++ "j";
            else
                skip;
            idx = idx + 1;
        }
        return result;
    }

    static String copyIMEI(String imei){
        //ASCII values for integer: 48-57

        String[] imeiAsChar, newOldIMEI;
        String res;
        // numbers array creation
        int[] numbers;
        int idx;
        idx = 0;
        numbers = new int[58];
        while (idx < 58) {
            numbers[idx] = idx;
            idx = idx + 1;
        }

        imeiAsChar = toCharArray(imei);
        newOldIMEI = new String[18]; // new String[len(imeiAsChar)]; // @FIXME: only fixed value
        idx = 0;
        while (idx < len(imeiAsChar)) {
            int tmp;
            tmp = numbers[stdlib.strToInt(imeiAsChar[idx])];
            newOldIMEI[idx] = stdlib.intToString(tmp);
            idx = idx + 1;
        }

        res = "";
        idx = 0;
        while (idx < len(newOldIMEI)) {
            res = res ++ newOldIMEI[idx];
            idx = idx + 1;
        }

        return res;
    }

    static void writeToLog(String message){
        Log.i("INFO", message); //sink
    }

    //@Override
    static void onCreate(Bundle savedInstanceState) {
        //super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_implicit_flow1);
        //TelephonyManager telephonyManager = (TelephonyManager)getSystemService(Context.TELEPHONY_SERVICE);
        String imei;
        String obfuscatedIMEI;
        imei = TelephonyManager.getDeviceId(); //source
        obfuscatedIMEI = obfuscateIMEI(imei);
        writeToLog(obfuscatedIMEI);

        //hard to detect (implicit flow)
        obfuscatedIMEI = copyIMEI(imei);
        writeToLog(obfuscatedIMEI);

    }
}
