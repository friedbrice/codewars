import java.util.ArrayList;
import java.util.List;

// Given an array, find the int that appears an odd number of times.
// There will always be only one integer that appears an odd number of times.

public class FindOdd {
    public static int findIt(int[] xs) {
        int output = 0;

        ArrayList<Integer> ints = new ArrayList<>();
        ArrayList<Integer> freq = new ArrayList<>();

        for (int x : xs) {
            if (!ints.contains(x)) {
                ints.add(x);
                freq.add(0);
            }

            for (int i = 0; i < ints.size(); i++) {
                if (x == ints.get(i)) {
                    int newFreq = freq.get(i) + 1;
                    freq.set(i, newFreq);
                }
            }
        }

        for (int i = 0; i < freq.size(); i++) {
            if (freq.get(i) % 2 == 1) {
                output = ints.get(i);
            }
        }

        return output;
    }
}
