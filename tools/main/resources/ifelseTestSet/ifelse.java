// simple debug program for if then else statements
class ifelse extends Activity {
    static void main() {
        boolean booleanVal;
        int var;
        booleanVal = readlib.readBool("aBool");
        //var = 5; //readlib.readInt("anInt");
        var = readlib.readPreciseInt();
        //log(booleanVal);

        if (booleanVal == true) {
            //log(var);
            var = var + 7;
            //log(12);
            //log(var);
        }
        else {
            var = var - 8;
        }
        log(var);
    }
}
