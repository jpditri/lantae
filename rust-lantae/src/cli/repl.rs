use crate::cli::commands::{Command, CommandHandler};
use crate::utils::Result;
use colored::*;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result as RustylineResult};

pub struct Repl {
    editor: DefaultEditor,
    command_handler: CommandHandler,
}

impl Repl {
    pub fn new() -> RustylineResult<Self> {
        let editor = DefaultEditor::new()?;
        let command_handler = CommandHandler::new();
        
        Ok(Self {
            editor,
            command_handler,
        })
    }
    
    pub async fn run(&mut self) -> Result<()> {
        println!("{}", "🚀 Starting interactive mode...".bright_green());
        println!("Type {} for help or {} to exit", "/help".bright_blue(), "/exit".bright_blue());
        println!();
        
        loop {
            match self.editor.readline(&self.get_prompt()) {
                Ok(line) => {
                    if line.trim().is_empty() {
                        continue;
                    }
                    
                    // Add to history
                    let _ = self.editor.add_history_entry(&line);
                    
                    // Parse and handle command
                    let command = Command::parse(&line);
                    
                    match self.command_handler.handle_command(command).await {
                        Ok(should_continue) => {
                            if !should_continue {
                                break;
                            }
                        },
                        Err(e) => {
                            println!("{}: {}", "Error".red(), e);
                        }
                    }
                    
                    println!(); // Add spacing between interactions
                },
                Err(ReadlineError::Interrupted) => {
                    println!("{}", "^C".bright_yellow());
                    println!("Use {} to exit", "/exit".bright_blue());
                },
                Err(ReadlineError::Eof) => {
                    println!("{}", "👋 Goodbye!".bright_green());
                    break;
                },
                Err(err) => {
                    println!("{}: {}", "Error".red(), err);
                    break;
                }
            }
        }
        
        Ok(())
    }
    
    fn get_prompt(&self) -> String {
        format!("{} ", ">".bright_cyan())
    }
}