class ImplicitFlow2 extends Activity {
	
	string geloso;
	
	void main() {
		int a, b;
		boolean c;
		string d;
		c = false;
		d = "polok";
		a = 5;
		b = 10;
		geloso = "gririsi";
		//test
		
		/*another 
		 * longer
		 * test
		 * on
		 * comments
		 */
		print("Stampo a prima dello if: ");
		println(a);
		if (((a * 3) == b))
		{
			print("Stampo b, sono nello if branch: ");
			println(c);
			return d;
		}
		else {
			print("Stampo c, sono nello else branch: ");
			println(c);
			c = (!false);
		}
		print("Stampo a DOPO lo if: ");
		println(a);
		onCreate(10, "pippazzo");
		//print("Ora stampo a:")
		println("Stampo qualcosa DOPO il metodo onCreate");
		return geloso;
	}
		void onCreate(int a, string b) { 
			//int geloso;
			int geloso2;
			geloso2 = 0;
			print("Stampo geloso dentro il metodo onCreate ");
			println(geloso2);
			//geloso = b;
			}
}
