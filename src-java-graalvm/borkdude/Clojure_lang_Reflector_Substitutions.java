import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetElement;
import com.oracle.svm.core.annotate.TargetClass;
import com.oracle.svm.core.jdk.JDK11OrLater;

import java.lang.reflect.Method;
import clojure.lang.Util;

public final class Clojure_lang_Reflector_Substitutions {
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
            throw Util.sneakyThrow(t);
        }
    }
}
