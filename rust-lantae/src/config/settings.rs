use crate::utils::Result;
use config::{Config, ConfigError, Environment, File};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Settings {
    pub default: DefaultSettings,
    pub providers: HashMap<String, ProviderSettings>,
    pub ui: UiSettings,
    pub performance: PerformanceSettings,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefaultSettings {
    pub provider: String,
    pub model: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProviderSettings {
    pub api_key_env: Option<String>,
    pub base_url: Option<String>,
    pub timeout: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UiSettings {
    pub colors: bool,
    pub progress_bars: bool,
    pub streaming: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceSettings {
    pub max_concurrent_requests: u32,
    pub request_timeout: u64,
    pub retry_attempts: u32,
}

impl Default for Settings {
    fn default() -> Self {
        let mut providers = HashMap::new();
        
        providers.insert("ollama".to_string(), ProviderSettings {
            api_key_env: None,
            base_url: Some("http://localhost:11434".to_string()),
            timeout: Some(60),
        });
        
        providers.insert("openai".to_string(), ProviderSettings {
            api_key_env: Some("OPENAI_API_KEY".to_string()),
            base_url: Some("https://api.openai.com/v1".to_string()),
            timeout: Some(60),
        });
        
        providers.insert("anthropic".to_string(), ProviderSettings {
            api_key_env: Some("ANTHROPIC_API_KEY".to_string()),
            base_url: Some("https://api.anthropic.com/v1".to_string()),
            timeout: Some(60),
        });
        
        Self {
            default: DefaultSettings {
                provider: "ollama".to_string(),
                model: "cogito:latest".to_string(),
            },
            providers,
            ui: UiSettings {
                colors: true,
                progress_bars: true,
                streaming: true,
            },
            performance: PerformanceSettings {
                max_concurrent_requests: 4,
                request_timeout: 120,
                retry_attempts: 3,
            },
        }
    }
}

impl Settings {
    pub fn new() -> Result<Self> {
        let config_dir = Self::get_config_dir()?;
        let config_file = config_dir.join("config.toml");
        
        let builder = Config::builder()
            .add_source(Config::try_from(&Settings::default())?)
            .add_source(File::from(config_file).required(false))
            .add_source(Environment::with_prefix("LANTAE"));
            
        let config = builder.build()?;
        
        Ok(config.try_deserialize()?)
    }
    
    pub fn save(&self) -> Result<()> {
        let config_dir = Self::get_config_dir()?;
        std::fs::create_dir_all(&config_dir)?;
        
        let config_file = config_dir.join("config.toml");
        let toml_content = toml::to_string_pretty(self)?;
        
        std::fs::write(config_file, toml_content)?;
        
        Ok(())
    }
    
    fn get_config_dir() -> Result<PathBuf> {
        let config_dir = dirs::config_dir()
            .ok_or_else(|| ConfigError::Message("Could not find config directory".to_string()))?
            .join("lantae");
            
        Ok(config_dir)
    }
}