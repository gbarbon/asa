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
        int v;

        /*elem = new int[50];
        var = 0;

        while (var < 50) {
            elem[var] = var;
            var = var + 1;
        }
        println(elem);*/

        /*v = readlib.readInt("blabla");

        if (v == 5){
            println("asd");
        }
        elif (true) {
            println("no");
        }
        else {
            println("No");
        }*/

        String s;
        String s1;
        boolean b;

        /*imeiAsChar = toCharArray(imei);
        newOldIMEI = new String[18]; // new String[len(imeiAsChar)]; // @FIXME: only fixed value
        idx = 0;
        while (idx < len(imeiAsChar)) {
            int tmp;
            tmp = numbers[stdlib.strToInt(imeiAsChar[idx])];
            newOldIMEI[idx] = stdlib.intToString(tmp);
            idx = idx + 1;
        }

        res = "";
        idx = 0;
        while (idx < len(newOldIMEI)) {
            res = newOldIMEI[idx] ++ res;
            println(res);
            idx = idx +1;
        }*/

        b = readlib.readBool("abc");

        s1 = "";

        if (b)
            s = "";


        s1 = s ++ s1;
        log(s1);



    }
}
