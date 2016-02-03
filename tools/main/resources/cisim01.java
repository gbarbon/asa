class ImplicitFlow1 extends Activity {

    static void main() {
        string key;
        boolean diff;
        diff = true;
        key = "key";
        onCreate(key, diff);
    }

    static void onCreate(string key, boolean diff) {
        string imei, obfuscatedIMEI;
        imei = readlib.readIMEI(); // device id
        if (diff)
            obfuscatedIMEI = stdlib.encrypt(imei, key);
        else
            obfuscatedIMEI = stdlib.hash(imei);
        writeToLog(obfuscatedIMEI);
    }

    //static String obfuscateIMEI(String imei) {
        //String result = "";
        //char[] imeiAsChar = imei.toCharArray();
        //int len = imeiAsChar.length();
        //int i = (0);
        //int shift = (49);

        //while (i < len) {
            //result += (char) (((int) imeiAsChar[i]) + shift);
            // returns ’a’ for ’0’, ’b’ for ’1’, ...
            //i++;
        //}
        //return result ;
    //}

    //private String hardObfIMEI(String imei) {
        //char[] imeiAsChar = imei.toCharArray();
        //String result ="";
        //int len = imeiAsChar.length();
        //int i = 0;

        //while (i < len) {
            //result += (char) (((int) imeiAsChar[is]) + 1 + (i * i) % 62);
            //i++;
        //}
        //return result;
    //}

    static void writeToLog(string message) {
        stdlib.log(message); //sink
        println(message);
    }
}