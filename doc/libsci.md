# Libsci

To use sci as a shared libary from e.g. C++, follow along with this tutorial. We illustrate what is happening when you run the script `libsci/compile`.

In `libsci/src` we have the following Clojure file:

``` clojure
(ns sci.impl.libsci
  (:require [cheshire.core :as cheshire]
            [sci.core :as sci])
  (:gen-class
   :methods [^{:static true} [evalString [String] String]]))

(defn -evalString [s]
  (sci/binding [sci/out *out*] ;; this enables println etc.
    (str (sci/eval-string
          s
          ;; this brings cheshire.core into sci
          {:namespaces {'cheshire.core {'generate-string cheshire/generate-string}}}))))
```

This file is compiled into a Java class with one static method,
`evalString`. This will be our API for the native library. To make this library
more interesting, we enable `println` by providing a value for `*out*` in the
interpreter. Also we make the [cheshire](https://github.com/dakrone/cheshire)
library available, just to show that you can bring in your own Clojure
functions.

Now let's have a look at the bridging class between Java and C++:

``` java
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
```

Here we wrap the static method `evalString` into a native library function that
is given the name `eval_string`. We use GraalVM's API to convert between Java
and C types.

The Clojure and Java code is compiled into .class files. Next, we compile those
.class files into a shared library using `native-image`:

``` shell
$ $GRAALVM_HOME/bin/native-image \
  -jar $SCI_JAR \
  -cp libsci/src \
  -H:Name=libsci \
  --shared \
  ...
```

This begets the files `graal_isolate_dynamic.h`, `graal_isolate.h`, `libsci.h`,
`libsci.dylib` (on linux `libsci.so`) and `libsci_dynamic.h`. We move all these files to `libsci/target`.

Let's use the library from a C++ program now. Here's the code:

``` c++
#include <iostream>
#include <libsci.h>

int main(int argc, char* argv[]) {
  graal_isolate_t *isolate = NULL;
  graal_isolatethread_t *thread = NULL;

  if (graal_create_isolate(NULL, &isolate, &thread) != 0) {
    fprintf(stderr, "initialization error\n");
    return 1;
  }

  char *result = eval_string((long)thread, &argv[1][0]);
  std::cout << result << std::endl;
  return 0;
}
```

This code gets the first command line argument and feeds it into `libsci`'s
function `eval_string`. We compile this code as follows:

``` shell
$ g++ libsci/src/from_cpp.cpp -L libsci/target -I libsci/target -lsci -o libsci/target/from_cpp
```

To run, we first have to set an environment variable to locate the shared libary:

``` shell
$ export DYLD_LIBRARY_PATH=libsci/target
```

On linux this environment variable is called `LD_LIBRARY_PATH`.

Now, let's run it.

``` shell
$ time libsci/target/from_cpp "
(println :foo)
(require '[cheshire.core :as cheshire])
(cheshire/generate-string {:a 1})"

:foo
{"a":1}
libsci/target/from_cpp   0.01s user 0.01s system 64% cpu 0.026 total
```

It worked. First we printed a keyword from within the interpreter. Then we
returned a Clojure hash-map that was converted into JSON by cheshire. And then
we printed the JSON string from the C++ program.

## References

- [Implementing native methods in Java with SVM](https://github.com/oracle/graal/blob/master/substratevm/ImplementingNativeMethodsInJavaWithSVM.md)
- [Code in Java, execute as C++](https://towardsdatascience.com/code-in-java-execute-as-c-921f5db45f20)
- [Top 10 Things To Do With GraalVM](https://chrisseaton.com/truffleruby/tenthings/)
- [PolyglotNativeAPI](https://github.com/oracle/graal/blob/6639edf945f9775e7fb7de3b58d4d6b3c374a0b3/substratevm/src/org.graalvm.polyglot.nativeapi/src/org/graalvm/polyglot/nativeapi/PolyglotNativeAPI.java#L260)
