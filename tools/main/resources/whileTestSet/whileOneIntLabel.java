// simple debug program for if then else statements
class whileOneIntLabel extends Activity {

    static void main() {
        int var;
        var = readlib.readPreciseInt();

        while (var < 50) {
            log(var);
            var = var + 1;
        }
        log(var);
    }
}