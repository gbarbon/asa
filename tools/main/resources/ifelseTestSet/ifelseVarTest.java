// simple debug program for if then else statements
class ifelseVarTest extends Activity {
    static void main() {
        boolean booleanVal;
        int var;
        booleanVal = readlib.readPreciseBool();

        if (booleanVal) {
            int x;
            var = x;
            //x = x;
            //log(x);
        }
        else {
            //var = var - 8;
        }
        log(var);
    }
}
