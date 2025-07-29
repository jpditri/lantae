use crate::providers::{ProviderManager, ChatRequest, Message};
use crate::utils::Result;
use colored::*;

pub enum Command {
    Chat(String),
    Help,
    Provider(String),
    Model(String),
    Models,
    Clear,
    Info,
    Exit,
    Unknown(String),
}

impl Command {
    pub fn parse(input: &str) -> Self {
        let input = input.trim();
        
        if input.is_empty() {
            return Command::Unknown("empty".to_string());
        }
        
        if input.starts_with('/') {
            let parts: Vec<&str> = input[1..].split_whitespace().collect();
            if parts.is_empty() {
                return Command::Unknown(input.to_string());
            }
            
            match parts[0] {
                "help" | "h" => Command::Help,
                "provider" | "p" => {
                    if parts.len() > 1 {
                        Command::Provider(parts[1].to_string())
                    } else {
                        Command::Unknown("provider requires argument".to_string())
                    }
                },
                "model" | "m" => {
                    if parts.len() > 1 {
                        Command::Model(parts[1].to_string())
                    } else {
                        Command::Unknown("model requires argument".to_string())
                    }
                },
                "models" => Command::Models,
                "clear" | "c" => Command::Clear,
                "info" | "i" => Command::Info,
                "exit" | "quit" | "q" => Command::Exit,
                _ => Command::Unknown(input.to_string()),
            }
        } else {
            Command::Chat(input.to_string())
        }
    }
}

pub struct CommandHandler {
    provider_manager: ProviderManager,
    current_model: String,
    conversation: Vec<Message>,
}

impl CommandHandler {
    pub fn new() -> Self {
        Self {
            provider_manager: ProviderManager::new(),
            current_model: "cogito:latest".to_string(),
            conversation: Vec::new(),
        }
    }
    
    pub async fn handle_command(&mut self, command: Command) -> Result<bool> {
        match command {
            Command::Chat(message) => {
                self.handle_chat(message).await?;
            },
            Command::Help => {
                self.show_help();
            },
            Command::Provider(name) => {
                self.set_provider(&name)?;
            },
            Command::Model(name) => {
                self.set_model(name);
            },
            Command::Models => {
                self.list_models().await?;
            },
            Command::Clear => {
                self.clear_conversation();
            },
            Command::Info => {
                self.show_info();
            },
            Command::Exit => {
                println!("{}", "👋 Goodbye!".bright_green());
                return Ok(false);
            },
            Command::Unknown(input) => {
                println!("{}: {}", "Unknown command".red(), input);
                println!("Type {} for help", "/help".bright_blue());
            },
        }
        Ok(true)
    }
    
    async fn handle_chat(&mut self, message: String) -> Result<()> {
        // Add user message to conversation
        self.conversation.push(Message {
            role: "user".to_string(),
            content: message,
        });
        
        // Create chat request
        let request = ChatRequest {
            model: self.current_model.clone(),
            messages: self.conversation.clone(),
            temperature: Some(0.7),
            stream: Some(false),
        };
        
        print!("{}", "🤖 Thinking...".bright_yellow());
        std::io::Write::flush(&mut std::io::stdout()).unwrap();
        
        // Send to provider
        let response = self.provider_manager.get_current_provider().chat(request).await?;
        
        // Clear thinking message
        print!("\r{}\r", " ".repeat(15));
        
        // Display response
        println!("{}", response.content);
        
        // Add assistant response to conversation
        self.conversation.push(Message {
            role: "assistant".to_string(),
            content: response.content,
        });
        
        // Show usage if available
        if let Some(usage) = response.usage {
            println!(
                "{} {} tokens in, {} tokens out, {} total",
                "💡".bright_blue(),
                usage.input_tokens.to_string().bright_cyan(),
                usage.output_tokens.to_string().bright_cyan(),
                usage.total_tokens.to_string().bright_cyan()
            );
        }
        
        Ok(())
    }
    
    fn show_help(&self) {
        println!("{}", "🦀 Lantae Rust Commands:".bright_purple().bold());
        println!();
        println!("  {}           Show this help", "/help".bright_blue());
        println!("  {}      Switch provider (ollama, openai, anthropic)", "/provider <name>".bright_blue());
        println!("  {}         Switch model", "/model <name>".bright_blue());
        println!("  {}          List available models", "/models".bright_blue());
        println!("  {}           Clear conversation history", "/clear".bright_blue());
        println!("  {}            Show current provider/model info", "/info".bright_blue());
        println!("  {}            Exit the application", "/exit".bright_blue());
        println!();
        println!("  {}           Send a message to the LLM", "<message>".bright_green());
    }
    
    fn set_provider(&mut self, name: &str) -> Result<()> {
        self.provider_manager.set_provider(name)?;
        println!("Switched to provider: {}", name.bright_green());
        Ok(())
    }
    
    fn set_model(&mut self, name: String) {
        self.current_model = name.clone();
        println!("Switched to model: {}", name.bright_green());
    }
    
    async fn list_models(&self) -> Result<()> {
        println!("{}", "📋 Available models:".bright_blue());
        
        let models = self.provider_manager.get_current_provider().list_models().await?;
        
        for model in models {
            println!("  • {}", model);
        }
        
        Ok(())
    }
    
    fn clear_conversation(&mut self) {
        self.conversation.clear();
        println!("{}", "🗑️  Conversation cleared".bright_yellow());
    }
    
    fn show_info(&self) {
        println!("{}", "📊 Current Configuration:".bright_blue());
        println!("  Provider: {}", self.provider_manager.current_provider_name().bright_green());
        println!("  Model: {}", self.current_model.bright_green());
        println!("  Messages: {}", self.conversation.len().to_string().bright_cyan());
    }
}