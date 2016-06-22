class StmDegr extends Activity {

    static void main() {
        String imei;
        String pwd;


        imei = TelephonyManager.getDeviceId();

        pwd = stdlib.hash(readlib.readUsrPwd("5v6QewbIOIMh"));

        log(stdlib.encrypt(imei, pwd));
    }

    static void onCreate(int a, String b) {
    }
}
