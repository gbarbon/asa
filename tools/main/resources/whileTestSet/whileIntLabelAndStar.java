// simple debug program for if then else statements
class whileIntLabelAndStar extends Activity {
    static void main() {
        int var, label;
        var = 0;
        label = readlib.readPreciseInt();

        while (var < 6) {
            log(var);
            var = var + label;
        }
        log(var);
    }
}