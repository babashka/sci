package sci.impl;

import java.lang.reflect.Method;

public class Repro {

    public static void main (String [] args) {
        Class x = String.class;
        Method[] methods = x.getMethods();
        for (Method m: methods) {
            System.out.println(m);
        }
    }

}
