class ImplicitFlow2 extends Activity {
	
	static boolean extern;
	
	static void onCreate() { extern = true;}
		
	static void main() {
		string userInputPassword, superSecure, temp;
		int dim;
		boolean passwordCorrect;
		extern = false;
		userInputPassword = "password";
		//println("before read");
		superSecure = readlib.readUsrPwd("usr");
		//println("after read");
		if (stdlib.checkpwd(superSecure, userInputPassword)) 
			passwordCorrect = true;
		else {
			passwordCorrect = false;
			onCreate();
		}
		
		if (passwordCorrect)
			stdlib.log("The password is correct");
		else
			stdlib.log("The password is not correct");
		temp = (userInputPassword ++ superSecure);
		dim = stdlib.length(temp);
		println(extern);
		//println(userInputPassword);
		//println(superSecure);
		//println(temp);
		//println(dim);
	}
}
