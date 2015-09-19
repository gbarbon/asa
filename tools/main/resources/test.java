class Test extends Activity {
	
	static void main() {
		string dummy, res, lab;
		//print("before read");
		lab = readlib.readIMEI();
		//print("after read");
		dummy = "dummy string";
		res = (dummy ++ lab);
		return res;
	}
	
	static void onCreate(int a, string b) { 
	}
}
