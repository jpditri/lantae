use clap::{Arg, ArgAction, Command};
use std::process;

fn main() {
    let matches = Command::new("lantae")
        .version("1.0.0")
        .author("Lantae Contributors")
        .about("Multi-Provider LLM Interface - Rust Implementation")
        .arg(
            Arg::new("provider")
                .short('p')
                .long("provider")
                .value_name("PROVIDER")
                .help("LLM provider to use (ollama, openai, anthropic, gemini)")
                .default_value("ollama"),
        )
        .arg(
            Arg::new("model")
                .short('m')
                .long("model")
                .value_name("MODEL")
                .help("Model to use for the specified provider")
                .default_value("cogito:latest"),
        )
        .arg(
            Arg::new("temperature")
                .short('t')
                .long("temperature")
                .value_name("TEMP")
                .help("Temperature for response randomness (0.0-1.0)")
                .value_parser(clap::value_parser!(f32)),
        )
        .arg(
            Arg::new("auto_accept")
                .short('y')
                .long("auto-accept")
                .help("Automatically accept all prompts")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("planning_mode")
                .long("planning-mode")
                .help("Enable detailed task planning")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("agent")
                .long("agent")
                .help("Enable autonomous agent execution")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("no_banner")
                .long("no-banner")
                .help("Disable startup banner")
                .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("prompt")
                .help("Single prompt to process (if not provided, starts interactive mode)")
                .index(1),
        )
        .get_matches();

    // Show banner unless disabled
    if !matches.get_flag("no_banner") {
        show_banner();
    }

    // Extract arguments
    let provider = matches.get_one::<String>("provider").unwrap();
    let model = matches.get_one::<String>("model").unwrap();
    let temperature = matches.get_one::<f32>("temperature");
    let auto_accept = matches.get_flag("auto_accept");
    let planning_mode = matches.get_flag("planning_mode");
    let agent_mode = matches.get_flag("agent");

    println!("🦀 Lantae Rust Implementation");
    println!("Provider: {}", provider);
    println!("Model: {}", model);
    
    if let Some(temp) = temperature {
        println!("Temperature: {}", temp);
    }
    
    if auto_accept {
        println!("Auto-accept mode enabled");
    }
    
    if planning_mode {
        println!("Planning mode enabled");
    }
    
    if agent_mode {
        println!("Agent mode enabled");
    }

    // Check for single prompt mode
    if let Some(prompt) = matches.get_one::<String>("prompt") {
        println!("\nProcessing prompt: {}", prompt);
        // TODO: Implement single prompt processing
        println!("⚠️  Single prompt mode not yet implemented");
        process::exit(1);
    } else {
        // Start interactive mode
        println!("\nStarting interactive mode...");
        // TODO: Implement REPL
        println!("⚠️  Interactive mode not yet implemented");
        println!("💡 This is a placeholder for the Rust implementation");
        println!("📖 See the feature parity document for implementation status");
    }
}

fn show_banner() {
    use colored::*;
    
    println!("{}", "╔══════════════════════════════════════════════════════════════╗".bright_purple());
    println!("{}", "║  🦀 Lantae Rust Implementation v1.0.0                       ║".bright_purple());
    println!("{}", "║  ⚡ High-Performance Multi-Provider LLM Interface           ║".bright_purple());
    println!("{}", "║  🚧 Under Development - See Feature Parity Document        ║".bright_purple());
    println!("{}", "╚══════════════════════════════════════════════════════════════╝".bright_purple());
    println!();
}