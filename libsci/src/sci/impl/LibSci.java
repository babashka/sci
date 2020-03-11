package sci.impl;

import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import com.oracle.svm.core.c.CConst;

public final class LibSci {
    @CEntryPoint(name = "eval_string")
    public static @CConst CCharPointer evalString(@CEntryPoint.IsolateThreadContext long isolateId, @CConst CCharPointer s) {
        String expr = CTypeConversion.toJavaString(s);
        String result = sci.impl.libsci.evalString(expr);
        CTypeConversion.CCharPointerHolder holder = CTypeConversion.toCString(result);
        CCharPointer value = holder.get();
        return value;
    }
}
