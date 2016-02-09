/**
 * Modified version of the ImplicitFlow2 from DroidBench, used on CISIM 2015
 */

class ImplicitFlow2 extends Activity {
    //boolean passwordCorrect;
    //passwordCorrect = false;

    //void onCreate(Bundle savedInstanceState) {
    //    super.onCreate(savedInstanceState);
    //    setContentView(R.layout.activity_implicit_flow2);
    //}

    static void checkPassword(){
        String superSecure, userInputPassword;
        boolean passwordCorrect;
        superSecure = readlib.readUsrPwd("userName");
        userInputPassword = "stupid_input";

        if(stdlib.checkpwd(superSecure, userInputPassword))
            passwordCorrect = true;
        else
            passwordCorrect = false;

        if(passwordCorrect)
            log("Password is correct"); //sink, leak
        else
            log("Password is not correct"); //sink, leak
    }

    static void main() {
        checkPassword();
    }
}