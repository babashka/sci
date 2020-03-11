package sci.impl;

import org.graalvm.nativeimage.c.function.CEntryPoint;
import org.graalvm.word.Pointer;
import org.graalvm.nativeimage.c.type.CCharPointer;
import org.graalvm.nativeimage.c.type.CTypeConversion;
import com.oracle.svm.core.c.CConst;

// reference
// https://github.com/oracle/graal/blob/6639edf945f9775e7fb7de3b58d4d6b3c374a0b3/substratevm/src/org.graalvm.polyglot.nativeapi/src/org/graalvm/polyglot/nativeapi/PolyglotNativeAPI.java#L260

// also https://towardsdatascience.com/code-in-java-execute-as-c-921f5db45f20

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
