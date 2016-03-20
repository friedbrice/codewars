import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

// Given two arrays of strings, return the array of all members of the first
// array that are substrings of a member of the second array, in lexicographic
// order.

public class WhichAreIn {

    public static String[] inArray(String[] subs, String[] sups) {
        List<String> preout = new ArrayList<>();

        for (String sub : subs) {
            boolean flag = false;

            for (String sup : sups) {
                if (sup.contains(sub)) {
                    flag = true;
                }
            }

            if (flag) {
                preout.add(sub);
            }
        }

        Collections.sort(preout);
        String[] output = new String[preout.size()];
        preout.toArray(output);
        return output;
    }
}
