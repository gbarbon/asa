class moreLabels extends Activity {
    static void main() {
        boolean booleanVal;
        int var, third;
        /*booleanVal = readlib.readBool("aBool");
        var = readlib.readPreciseInt();
        third = readlib.readInt("blabla");

        third = third + 5;
        //log(third);
        var = var + third;
        log(var);*/
        int[] elem;

        elem = new int[10];
        var = 0;

        while (var < 50) {
            elem[0] = var;
            var = var + 1;
        }
        println(elem);


    }
}
