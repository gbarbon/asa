// simple debug program for if then else statements
class ifelse extends Activity {
    static void main() {
        boolean booleanVal;
        int var, third;
        booleanVal = readlib.readBool("aBool");
        var = readlib.readPreciseInt();

        if (booleanVal == true) {
            var = var + 7;
        }
        else {
            var = var - 8;
        }
        log(var);
    }
}
