class TestFlow {
	
	static boolean polpetta;
	
	static int gelato()
	{
		int geloso;
		geloso = 10;
		if (polpetta)
			return (geloso * 2);
		else
			return geloso;
	}
	
}

class ImplicitFlow2 extends Activity {
	
	static string geloso;
	
	//@@[obf:"H";implq:"L"]
	static void main() {
		int a, b, i;
		boolean c;
		string d,e;
		c = false;
		d = "polok";
		e = "";
		a = 5;
		b = 10;
		geloso = "gririsi";
		
		TestFlow.polpetta = true;
		println(TestFlow.polpetta);
		
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
			print("Stampo b, sono nel than branch: ");
			println(c);
			return d;
		}
		else {
			print("Stampo c, sono nello else branch: ");
			println(c);
			c = (!false);
		}
		
		i = 0;
		e = "";
		while ((i < 10))
		{
			e = (e ++ "e");
			i = (i + 1);
		}

		println(e);
		println(i);
		
		print("Stampo a DOPO lo if: ");
		println(a);
		onCreate(10, "pippazzo");
		//print("Ora stampo a:")
		println(("Stampo qualcosa DOPO il metodo onCreate"));
		return geloso;
	}
	
	static void onCreate(int a, string b) { 
		//int geloso;
		int geloso2;
		geloso2 = 0;
		print("Stampo geloso dentro il metodo onCreate ");
		println(geloso2);
		//geloso = b;
	}
}
