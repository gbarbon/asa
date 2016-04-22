class arrayMulti extends Activity {
    static void main() {
        boolean booleanVal;
        int cnt;
        int[][] arr;

        cnt = 0;

        arr = new int[][5];

        while (cnt < len(arr)) {
            arr[cnt] = new int[2];
            cnt = cnt + 1;
        }

        println(arr);
    }
}
