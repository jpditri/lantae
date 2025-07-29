pub mod ollama;
pub mod openai;
pub mod anthropic;
pub mod base;

pub use base::*;
pub use ollama::*;
pub use openai::*;
pub use anthropic::*;

use crate::utils::Result;
use std::collections::HashMap;

pub struct ProviderManager {
    providers: HashMap<String, Box<dyn Provider + Send + Sync>>,
    current_provider: String,
}

impl ProviderManager {
    pub fn new() -> Self {
        let mut providers: HashMap<String, Box<dyn Provider + Send + Sync>> = HashMap::new();
        
        // Register all providers
        providers.insert("ollama".to_string(), Box::new(OllamaProvider::new()));
        providers.insert("openai".to_string(), Box::new(OpenAIProvider::new()));
        providers.insert("anthropic".to_string(), Box::new(AnthropicProvider::new()));
        
        Self {
            providers,
            current_provider: "ollama".to_string(),
        }
    }
    
    pub fn set_provider(&mut self, name: &str) -> Result<()> {
        if self.providers.contains_key(name) {
            self.current_provider = name.to_string();
            Ok(())
        } else {
            Err(crate::utils::LantaeError::ProviderError(
                format!("Provider '{}' not found", name)
            ))
        }
    }
    
    pub fn get_current_provider(&self) -> &dyn Provider {
        self.providers.get(&self.current_provider).unwrap().as_ref()
    }
    
    pub fn get_current_provider_mut(&mut self) -> &mut dyn Provider {
        self.providers.get_mut(&self.current_provider).unwrap().as_mut()
    }
    
    pub fn list_providers(&self) -> Vec<String> {
        self.providers.keys().cloned().collect()
    }
    
    pub fn current_provider_name(&self) -> &str {
        &self.current_provider
    }
}