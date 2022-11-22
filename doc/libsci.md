# Libsci

Table of contents:
- [Libsci](#libsci)
  - [Prerequisites](#prerequisites)
  - [Walkthrough](#walkthrough)
    - [Compiling sci as shared library](#compiling-sci-as-shared-library)
    - [Using libsci from C++](#using-libsci-from-c)
    - [Using libsci from Rust](#using-libsci-from-rust)
    - [Using libsci from Python](#using-libsci-from-python)
  - [References](#references)

To use sci as a shared library from e.g. C++, follow along with this
tutorial. We illustrate what is happening when you run the script
`libsci/compile-libsci` and `libsci/compile-cpp`.

There are also instructions at the end for using the shared library from
Python using ctypes.

## Prerequisites

If you want to run this script yourself, prepare as follows:

- Download [GraalVM](https://github.com/graalvm/graalvm-ce-builds/releases) and
  set `GRAALVM_HOME`. Currently 20.3.0 Java 11 must be used.
- Install [lein](https://github.com/technomancy/leiningen). This is used for
  compiling Clojure code.
- You should have `g++` available to compile C++ code.

## Babashka tasks

Convenient `babashka` tasks are provided to compile `libsci` and most
of the examples mentioned here, see `bb tasks` for the full list.

## Walkthrough

### Compiling sci as shared library

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
`libsci.dylib` (on linux `libsci.so`, on MS-Windows `libsci.dll`) and `libsci_dynamic.h`.
We move all these files to `libsci/target`.

In addtion, on MS-Windows, there is one more library file,
`libsci.lib`, which should be copied over as `sci.lib`.

### Using libsci from C++

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

### Using libsci from Rust

To use `libsci` from a Rust program, we use the same shared lib generated in the
previous section (produced by running `libsci/compile-libsci`).  Here we
describe what happens when you run `libsci/compile-rust`.

To build Rust language bindings to `libsci`, we use
[bindgen](https://rust-lang.github.io/rust-bindgen/) which need a `build.rs`
file.

This file is located in `libsci/from-rust/build.rs`.

``` rust
extern crate bindgen;

use std::env;
use std::path::PathBuf;

fn main() {
    let path = env::var("LIBSCI_PATH").unwrap();

    println!("cargo:rustc-link-lib=sci");

    println!("cargo:rustc-link-search={path}", path = path);

    let bindings = bindgen::Builder::default()
        .header(format!("{path}/libsci.h", path = path))
        .clang_arg(format!("-I{path}", path = path))
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("bindings.rs"))
        .expect("Couldn't write bindings!");
}
```

Learn [more](https://doc.rust-lang.org/cargo/reference/build-scripts.html) about `build.rs` files here.

Secondly we write a main program that uses these bindings to call `libsci`. This
code is located in `libsci/from-rust/src/main.rs`.

``` rust
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::ffi::{CStr, CString};
use std::str::Utf8Error;
use std::{env, ptr};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

fn eval(expr: String) -> Result<&'static str, Utf8Error> {
    unsafe {
        let mut isolate: *mut graal_isolate_t = ptr::null_mut();
        let mut thread: *mut graal_isolatethread_t = ptr::null_mut();

        graal_create_isolate(ptr::null_mut(), &mut isolate, &mut thread);

        let result = eval_string(
            thread as i64,
            CString::new(expr).expect("CString::new failed").as_ptr(),
        );

        CStr::from_ptr(result).to_str()
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let result = eval(args[1].to_owned());

    match result {
        Ok(output) => println!("{}", output),
        Err(_) => println!("Failed."),
    };
}
```

After running `libsci/compile-rust` and exporting `DYLD_LIBRARY_PATH`
(`LD_LIBRARY_PATH` on linux) to `libsci/target`, you should be able to run as
follows:

``` shell
$ libsci/target/from-rust "(require '[cheshire.core :as json]) (json/generate-string (range 10))"
[0,1,2,3,4,5,6,7,8,9]
```

### Using libsci from Python

To use the shared library from Python via ctypes, do the following from the directory
containing the shared object:
``` python
$ python
Python 3.8.5 (default, Sep  5 2020, 10:50:12)
[GCC 10.2.0] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> from ctypes import *
>>> dll = CDLL("./libsci.so")
>>> isolate = c_void_p()
>>> isolatethread = c_void_p()
>>> dll.graal_create_isolate(None, byref(isolate), byref(isolatethread))
0
>>> dll.eval_string.restype = c_char_p
>>> result = dll.eval_string(isolatethread, c_char_p(bytes("(+ 1 8)", "utf8")))
>>> result
b'9'
```

The above instructions are for a Linux system.

For macos, the file extension of the shared library should be different, probably `.dylib`.

For Windows, the file extension of the shared library should be different, probably `.dll`.
Also it may be necessary to use `WinDLL` instead of `CDLL`.

N.B. Testing has only been done on Linux.

## References

- [Implementing native methods in Java with SVM](https://github.com/oracle/graal/blob/master/substratevm/ImplementingNativeMethodsInJavaWithSVM.md)
- [Code in Java, execute as C++](https://towardsdatascience.com/code-in-java-execute-as-c-921f5db45f20)
- [Top 10 Things To Do With GraalVM](https://chrisseaton.com/truffleruby/tenthings/)
- [PolyglotNativeAPI](https://github.com/oracle/graal/blob/6639edf945f9775e7fb7de3b58d4d6b3c374a0b3/substratevm/src/org.graalvm.polyglot.nativeapi/src/org/graalvm/polyglot/nativeapi/PolyglotNativeAPI.java#L260)
