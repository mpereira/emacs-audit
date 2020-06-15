fn main() {
    println!(
        "cargo:rustc-env=EMACS_AUDIT_GITHUB_ACCESS_TOKEN={}",
        option_env!("EMACS_AUDIT_GITHUB_ACCESS_TOKEN").unwrap_or("deadbeef")
    );

    let mut options = built::Options::default();
    options.set_dependencies(true);

    let manifest = std::env::var("CARGO_MANIFEST_DIR").unwrap();
    let output = std::path::Path::new(&std::env::var("OUT_DIR").unwrap())
        .join("built.rs");
    built::write_built_file_with_opts(&options, manifest.as_ref(), &output)
        .expect("Failed to acquire build-time information");
}
