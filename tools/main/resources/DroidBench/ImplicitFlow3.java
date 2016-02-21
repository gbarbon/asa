/**
 * Modified version of the ImplicitFlow3 from DroidBench, used on CISIM 2015
 */


// @FIXME: not working...
class ImplicitFlow3 extends Activity {

    //void onCreate() {
        // ...
    //}

    interface Interface {
        static void leakInfo();
    }

    static void leakData() {
        String userINtPwd, superSecure;
        userINtPwd = "user_input_pwd";
        superSecure = "secret_password";
        Interface classTmp;
        if (checkpwd(superSecure, userINtPwd))
            classTmp = new ClassA();
        else
            classTmp = new ClassB();

        classTmp.leakInfo();
    }

    class ClassA implements Interface {
        static void leakinfo() {
            log("pwd_correct")
        }
    }

    class ClassB implements Interface {
        static void leakinfo() {
            log("pwd_correct")
        }
    }

    static void main() {
        leakData();
    }
}