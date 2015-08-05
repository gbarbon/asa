class ImplicitFlow2 extends Activity {
	
	void onCreate() {}

	void checkPassword(View view) {
		string userInputPassword = "password";
		string superSecure = "secret_password";
		boolean passwordCorrect;

		if (checkpwd(superSecure, userInputPassword)) then
			passwordCorrect = true;
		else 
			passwordCorrect = else;

		if (passwordCorrect) then
			Log.i("The password is correct");
		else
			Log.i("The password is not correct");	
	}
}
