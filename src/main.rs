use std::{env, path::PathBuf, process::ExitCode};

static BINARY_NAME: &str = "jjik";

fn main() -> ExitCode {
    let (input_file_path, output_file_path) = match parse_arguments() {
        Ok(res) => res,
        Err(code) => return code,
    };
    match jjik::driver::run(&input_file_path, &output_file_path) {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("{message}");
            ExitCode::FAILURE
        }
    }
}

fn parse_arguments() -> Result<(PathBuf, PathBuf), ExitCode> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("incorrect number of argument(s) given");
        println!();
        println!("{}", usage());
        return Err(ExitCode::FAILURE);
    }
    Ok((PathBuf::from(&args[1]), PathBuf::from(&args[2])))
}

fn usage() -> String {
    format!(
        r#"{BINARY_NAME}: Generate Rust code for parsing grammars specified in a .gg file
usage: {BINARY_NAME} <gg file> <output directory>"#
    )
}
