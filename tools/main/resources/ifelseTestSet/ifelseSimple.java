// simple debug program for if then else statements
class ifelseSimple extends Activity {
    static void main() {
        int var;
        var = readlib.readPreciseInt();
        //var = 5;

        if (var > 10) {
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
