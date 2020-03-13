#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::ffi::{CStr, CString};
use std::str::Utf8Error;
use std::{env, ptr};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

fn my_string_safe(ptr: *mut i8) -> String {
    unsafe {
        CStr::from_ptr(ptr).to_string_lossy().into_owned()
    }
}

fn eval(expr: String) -> String {
    unsafe {
        let mut isolate: *mut graal_isolate_t = ptr::null_mut();
        let mut thread: *mut graal_isolatethread_t = ptr::null_mut();

        graal_create_isolate(ptr::null_mut(), &mut isolate, &mut thread);

        let result = eval_string(
            thread as i64,
            CString::new(expr).expect("CString::new failed").as_ptr(),
        );

        my_string_safe(result)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let result = eval(args[1].to_owned());
    println!("{}", result)
}
