use std::path::Path;

use crate::{code_gen::CodeGen, gg::Gg};

pub fn run(gg_file_path: &Path, output_directory: &Path) -> Result<(), String> {
    let gg = Gg::new(gg_file_path)?;
    let code_gen = CodeGen::new(gg);
    code_gen.generate(output_directory)
}
