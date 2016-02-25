// simple debug program for if then else statements
class WhileSimple extends Activity {
    static void main() {
        boolean booleanVal;
        int var;
        booleanVal = true;
        var = 0;

        while (var < 50) {
            //log(booleanVal);
            //log(var);
            booleanVal = !booleanVal;
            var = var + 1;
        }
        log(booleanVal);
        log(var);
    }
}