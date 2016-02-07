class Test extends Activity {
	
	static void main() {
		boolean a;
		int x;

		x = 10;

		log(stdlib.intToString(x));

		a = stdlib.strToBool("");

		if (a){
			x = 20;
		}
		else{
			x = 50;
		}
		log(x);
		return x;
	}
	
	static void onCreate(int a, String b) {
	}
}
