#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::ffi::{CStr, CString};
use std::str::Utf8Error;
use std::{env, ptr};

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

fn my_string_safe(ptr: *mut i8) -> Result<String,Utf8Error> {
    let s = unsafe {
        CStr::from_ptr(ptr).to_str()
    };
    match s {
        Ok(s) => Ok(String::from(s)),
        Err(x) => Err(x)
    }
}

fn eval(expr: String) -> String {
    unsafe {
        let mut isolate: *mut graal_isolate_t = ptr::null_mut();
        let mut thread: *mut graal_isolatethread_t = ptr::null_mut();

        graal_create_isolate(ptr::null_mut(), &mut isolate, &mut thread);

	let cexpr = CString::new(expr).expect("CString::new failed");
        let result = eval_string(
            thread as i64,
            cexpr.as_ptr(),
        );
        let s = my_string_safe(result).unwrap();
        graal_tear_down_isolate(thread);
        // let s = my_string_safe(result).unwrap(); => segmentation fault, so it seems tear_down_isolate cleans stuff up.
        // see https://github.com/oracle/graal/blob/master/substratevm/C-API.md
        s
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let result = eval(args[1].to_owned());
    println!("{}", result);
}
