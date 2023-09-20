//#region Imports
use std::{
    env::args,
    fs::{self, OpenOptions},
    process::{exit, Command},
};

use parser::{lexer::Lexer, Program};
pub mod parser;
//#endregion

const DEBUG: bool = false;
const DEBUG_FILENAME: &str = "examples/helloworld.ns";

fn usage(args: Vec<String>) -> String {
    let mut usage = format!("Usage: {} <filepath> [option]\n", args.get(0).unwrap());
    usage.push_str(format!("options:\n").as_str());
    usage.push_str(
        format!("\t-o <filepath>\t\tThe executable will be output to <filepath>\n").as_str(),
    );
    usage
}

fn main() {
    let args = args().collect::<Vec<_>>();
    if args.len() < 2 && !DEBUG {
        eprintln!("ERROR: Not enough argument.");
        eprintln!("{}", usage(args));
        exit(1);
    }
    let filepath = if DEBUG {
        DEBUG_FILENAME.to_string()
    } else {
        args.get(1).unwrap().to_string()
    };

    let file_content = match fs::read_to_string(filepath.clone()) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("ERROR: cannot read the file at {filepath}: {err}");
            exit(1)
        }
    };
    let char_content = &file_content.chars().collect::<Vec<char>>();
    let mut tokens = Lexer::new(filepath, char_content).collect::<Vec<_>>();
    let mut program = Program::parse(&mut tokens);

    let mut output_filepath = "a.out".to_string();
    if let Some(option) = args.get(2) {
        match option.as_str() {
            "-o" => {
                if let Some(filepath) = args.get(3) {
                    output_filepath = filepath.to_string();
                } else {
                    eprintln!("ERROR: Invalid option.");
                    eprintln!("{}", usage(args));
                    exit(1);
                }
            }
            _ => {
                eprintln!("ERROR: Invalid option.");
                eprintln!("{}", usage(args));
                exit(1);
            }
        }
    }

    let mut asm = match OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("tmp.asm")
    {
        Ok(file) => file,
        Err(err) => {
            eprintln!("ERROR: Cannot open asm file: {err}");
            exit(1);
        }
    };

    println!("INFO: Starting compilation...");
    program.compile(&mut asm).expect("Cannot write to asm");

    //#region Compilation Commands
    println!("INFO: Running `nasm` -felf64 tmp.asm");
    if !Command::new("nasm")
        .arg("-felf64")
        .arg("tmp.asm")
        .status()
        .expect("Cannot run `nasm`")
        .success()
    {
        eprintln!("ERROR: `nasm` ended with error.");
        exit(1);
    }
    println!("INFO: Running `ld` tmp.o -o {output_filepath}");
    if !Command::new("ld")
        .arg("tmp.o")
        .arg("-o")
        .arg(format!("{output_filepath}"))
        .status()
        .expect("Cannot run `ld`")
        .success()
    {
        eprintln!("ERROR: `ld` ended with error.");
        exit(1);
    }
    println!("INFO: Running `rm` tmp.o");
    if !Command::new("rm")
        .arg("tmp.o")
        .status()
        .expect("Cannot run `rm`")
        .success()
    {
        eprintln!("ERROR: `rm` ended with error.");
        exit(1);
    }
    println!("SUCCESS: Compilation Successful");
    //#endregion
}
