// simple debug program for if then else statements
class ifelse extends Activity {
    static void main() {
        boolean booleanVal;
        int var;

        log(booleanVal);

        booleanVal = true;
        var = 0;

        while ((var < 50)) {
            println(var);
            var = (var + 1);
        }

        log(var);
    }
}
