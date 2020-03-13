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
