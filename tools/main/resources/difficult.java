class ImplicitFlow2 extends Activity {
	
	static void onCreate() {}
		
	static void main() {
		string userInputPassword, superSecure;
		boolean passwordCorrect;
		userInputPassword = "password";
		//println("before read");
		superSecure = readlib.readUsrPwd("usr");
		//println("after read");
		if (stdlib.checkpwd(superSecure, userInputPassword)) 
			passwordCorrect = true;
		else 
			passwordCorrect = false;

		if (passwordCorrect)
			stdlib.log("The password is correct");
		else
			stdlib.log("The password is not correct");
		//return superSecure;
	}
}
