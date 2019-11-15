extern crate scp;

use scp::{Command, CommandLine, ExecResult, ParseError};
use scp::{ParamAccessor, OptionAccessor};

fn main() {
    let mut command_line = CommandLine::new();

    command_line.register(Command::new(vec!["greet"]).set_syntax_format("s-(f, from)s"));

    match command_line.run("greet \"John Doe\" -fMrs.\\ Doe") {
        ExecResult::Err(e) => handle_error(e),
        ExecResult::Ok {
            command,
            subcommand: _,
            parameters,
            options,
        } => {
            match command {
                "greet" => {
                    println!("Hello, {}.", parameters.iter().poll());

                    if let Some(o) = options.by_long_flag("from") {
                        println!("It's {}!", o.parameter());
                    }
                }
                _ => {}
            }
        }
    }
}

fn handle_error(error: ParseError) {
    match error {
        ParseError::InvalidSyntax(e) => eprintln!("{}", e),
        ParseError::UnknownCommand => eprintln!("Unknown command"),
        ParseError::MissingSubcommand => eprintln!("Missing subcommand"),
        ParseError::MissingParameter(kind) => eprintln!("Missing parameter of type {}", kind.to_string()),
        ParseError::InvalidParameter(s) => eprintln!("Invalid parameter: {}", s),
        ParseError::UnnecessaryFlag(s) => eprintln!("Unknown flag: {}", s),
        ParseError::UnnecessaryParameter(s) => eprintln!("Unnecessary parameter: {}", s),
    }
}
