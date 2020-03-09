import org.graalvm.nativeimage.Platform;
import org.graalvm.nativeimage.Platforms;
import com.oracle.svm.core.annotate.*;
import com.oracle.svm.core.jdk.JDK11OrLater;


import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.stream.Collectors;

public final class ReflectorSubstitutions {
}

@TargetClass(className = "clojure.lang.Reflector")
final class Target_clojure_lang_Reflector {

    @Substitute
    @TargetElement(onlyWith = JDK11OrLater.class)
    private static boolean canAccess(Method m, Object target) {
        // JDK9+ use j.l.r.AccessibleObject::canAccess, which respects module rules
        try {
            return (boolean) m.canAccess(target);
        } catch (Throwable t) {
            // throw Util.sneakyThrow(t);
            return false;
        }
    }
}
