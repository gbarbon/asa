// simple debug program for if then else statements
class ifelse extends Activity {
    static void test(int[] a) {
        int[] b;

        b = new int[5];

        b[1] = 5;

        x = len(b[10]);
        x = b[40];
    }

    static void main() {
        boolean booleanVal;
        int var;

        log(booleanVal);

        booleanVal = true;
        var = 0;

        while (var < 50) {
            log(var);
            var = var + 1;
        }

        log(var);
    }
}
