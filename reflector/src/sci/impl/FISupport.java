package sci.impl;

import clojure.lang.RT;
import clojure.lang.IPersistentSet;
import java.util.concurrent.Callable;
import java.lang.reflect.Modifier;
import java.util.Comparator;

class FISupport {
    private static final IPersistentSet AFN_FIS = RT.set(Callable.class, Runnable.class, Comparator.class);
    private static final IPersistentSet OBJECT_METHODS = RT.set("equals", "toString", "hashCode");

    // Return FI method if:
    // 1) Target is a functional interface and not already implemented by AFn
    // 2) Target method matches one of our fn invoker methods (0 <= arity <= 10)
    protected static java.lang.reflect.Method maybeFIMethod(Class target) {
        if (target != null && target.isAnnotationPresent(FunctionalInterface.class)
            && !AFN_FIS.contains(target)) {

            java.lang.reflect.Method[] methods = target.getMethods();
            for (java.lang.reflect.Method method : methods) {
                if (method.getParameterCount() >= 0 && method.getParameterCount() <= 10
                    && Modifier.isAbstract(method.getModifiers())
                    && !OBJECT_METHODS.contains(method.getName()))
                    return method;
            }
        }
        return null;
    }
}


