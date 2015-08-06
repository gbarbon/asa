class ImplicitFlow2 extends Activity {
	
	void onCreate() {}

	void checkPassword(View view) {
		string userInputPassword; 
		string superSecure;
		boolean passwordCorrect;
		userInputPassword = 1;
		superSecure = 2;

		if (checkpwd(superSecure, userInputPassword)) then
			{passwordCorrect = true;}
		else 
			{passwordCorrect = false;}

		if (passwordCorrect) then
			{(Log).i(3);}
		else
			{(Log).i(4);}
	}
}
