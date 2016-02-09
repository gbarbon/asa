// simple debug program for if then else statements
class ifelse extends Activity {
    static void main() {
        boolean booleanVal;
        int var;
        booleanVal = readlib.readBool("aBool");

        log(booleanVal);

        if (((5 == 5) == true)) {
            var = 1;
        }
        else {
            var = 2;
        }
        log(var);
    }
}