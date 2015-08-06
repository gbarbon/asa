class ImplicitFlow2 extends Activity {
	
	void onCreate() {}

	void checkPassword(View view) {
		string userInputPassword ;
		string superSecure ;
		boolean passwordCorrect;
		userInputPassword = "password";
		superSecure = "secret_password";

		if (checkpwd(superSecure, userInputPassword)) then
			{passwordCorrect = true;}
		else 
			{passwordCorrect = false;}

		if (passwordCorrect) then
			{(Log).i("The password is correct");}
		else
			{(Log).i("The password is not correct");}	
	}
}
