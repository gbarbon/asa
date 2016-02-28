// simple debug program for if then else statements
class ifelseStrange extends Activity {
    static void main() {
        boolean booleanVal;
        int var;
        booleanVal = readlib.readBool("aBool");
        var = 5;

        if (booleanVal == true) {
            log(var); // @FIXME: the boolVal does not appear in the log!! Why?
            var = var + 7;
        }
        else {
            var = var - 8;
        }
    }
}
