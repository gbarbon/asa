// simple debug program for if then else statements
class WhileComplex extends Activity {
    static void main() {
        boolean booleanVal;
        int var;

        booleanVal = readlib.readPreciseBool();
        var = readlib.readPreciseInt();

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