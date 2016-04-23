//package de.ecspride;

//import android.app.Activity;
//import android.os.Bundle;
//import android.util.Log;
//import android.view.View;
//import android.widget.EditText;
/**
 * @testcase_name ImplicitFlow2
 * @version 0.1
 * @author Secure Software Engineering Group (SSE), European Center for Security and Privacy by Design (EC SPRIDE)
 * @author_mail siegfried.rasthofer@cased.de
 *
 * @description Based on an input of a password field a log message is written
 * @dataflow source -> userInputPassword -> if-condition -> sink
 * @number_of_leaks 2
 * @challenges the analysis must be able to handle implicit flows,
 *  detect callbacks from layout xml file and treat the value of password fields as source
 */
class ImplicitFlow2 extends Activity {
    //private boolean passwordCorrect = false;

    static void checkPassword(){
        String superSecure, userInputPassword;
        boolean passwordCorrect;
        superSecure = readlib.readUsrPwd("userName");
        userInputPassword = "user_pwd";

        if (stdlib.checkpwd(superSecure, userInputPassword) == true)
            passwordCorrect = true;
        else
            passwordCorrect = false;

        if((passwordCorrect == true))
            log("Password is correct"); //sink, leak
        else
            log("Password is not correct"); //sink, leak
    }

    //@Override
    static void onCreate(Bundle savedInstanceState) {
        //super.onCreate(savedInstanceState);
        //setContentView(R.layout.activity_implicit_flow2);
        checkPassword();
    }

}
