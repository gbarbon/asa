// simple debug program for if then else statements
class ifelse extends Activity {
    static void main() {
        boolean booleanVal;
        int var;
        booleanVal = readlib.readBool("aBool");
        var = 5;
        log(booleanVal);

        if (((5 == var) == true)) {
            var = 1;
        }
        else {
            var = 2;
        }
        log(var);
    }
}