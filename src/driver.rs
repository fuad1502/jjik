//! JJIK parser generator driver.

use std::path::Path;

use crate::{code_gen::CodeGen, gg::Gg};
///
/// Generates Rust modules for parsing grammars specified with a GG file.
///
/// This function generates three Rust modules: `parser.rs`, `lexer.rs`, and `symbol.rs` at the
/// directory path given in `output_directory` for parsing the grammar specified with a GG file
/// located at `gg_file_path`.
///
/// # Examples
///
/// ```
/// use std::path::PathBuf;
///
/// let gg_file_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
///     .join("gg")
///     .join("simple_calculator.gg");
/// let output_directory = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src");
///
/// jjik::driver::run(&gg_file_path, &output_directory).unwrap();
/// ```
pub fn run(gg_file_path: &Path, output_directory: &Path) -> Result<(), String> {
    let gg = Gg::new(gg_file_path)?;
    let code_gen = CodeGen::new(gg);
    code_gen.generate(output_directory)
}
