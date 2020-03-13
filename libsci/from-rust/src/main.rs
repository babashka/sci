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

struct EvalResult {
    ptr: *mut i8,
}

impl Drop for EvalResult {
    fn drop(&mut self) {
        unsafe {
            libc::free(self.ptr as *mut libc::c_void);
            // error for object 0x10a8d1158: pointer being freed was not allocated
        }
    }
}

fn eval(expr: String) -> EvalResult {
    unsafe {
        let mut isolate: *mut graal_isolate_t = ptr::null_mut();
        let mut thread: *mut graal_isolatethread_t = ptr::null_mut();

        graal_create_isolate(ptr::null_mut(), &mut isolate, &mut thread);

        let result = eval_string(
            thread as i64,
            CString::new(expr).expect("CString::new failed").as_ptr(),
        );

        EvalResult { ptr: result }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let result = eval(args[1].to_owned());
    // {
    //     let s = my_string_safe(result.ptr).unwrap();
    //     println!("{}", s)
    // }

    let s = my_string_safe(result.ptr).unwrap();
    println!("{}", s) // works the second time... when will the data at the pointer be cleaned up?
}
