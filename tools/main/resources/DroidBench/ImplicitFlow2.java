/**
 * Modified version of the ImplicitFlow2 from DroidBench, used on CISIM 2015
 */

class ImplicitFlow2 extends Activity {

    static void checkPassword(){
        String superSecure, userInputPassword;
        boolean passwordCorrect;
        superSecure = readlib.readUsrPwd("userName");
        userInputPassword = "stupid_input";

        if((stdlib.checkpwd(superSecure, userInputPassword) == true))
            passwordCorrect = true;
        else
            passwordCorrect = false;

        if((passwordCorrect == true))
            log("Password is correct"); //sink, leak
        else
            log("Password is not correct"); //sink, leak
    }

    static void onCreate(Bundle savedInstanceState) { checkPassword() };
    }
}