package sci.impl;
import java.lang.reflect.Method;

public class Main {

    public static void main(String[] args) {
        Method m = Reflector.findMatchingMethod("toString", "foo", new Object[]{}, false);
        System.out.println(m);
        m = Reflector.findMatchingMethod("notify", "foo", new Object[]{}, false);
        System.out.println(m);
    }

}
