package borkdude.sci;

import java.util.HashMap;
import clojure.java.api.Clojure;
import clojure.lang.IFn;
import borkdude.sci.options.Options;

public class Sci {

    private static IFn require;
    private static IFn convertOptions;
    private static IFn evalString;
    static {
        require = Clojure.var("clojure.core", "require");
        require.invoke(Clojure.read("sci.impl.java"));
        convertOptions = Clojure.var("sci.impl.java","Options->map");
        evalString = Clojure.var("sci.impl.java","eval-string");
    }

    public static Object evalString(String expression) {
        return evalString.invoke(expression);
    }

    public static Object evalString(String expression, Options opts) {
        return evalString.invoke(expression, opts);
    }

}
