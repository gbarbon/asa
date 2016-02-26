// simple debug program for if then else statements
class ifelse extends Activity {
    static void main() {
        boolean booleanVal;
        int var;
        booleanVal = readlib.readBool("aBool");
        var = 5; //readlib.readInt("anInt");
        //log(booleanVal);

        if (booleanVal == true) {
            var = var + 7;
        }
        else {
            var = var - 8;
        }
        log(var);
    }
}
