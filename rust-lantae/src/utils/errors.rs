use thiserror::Error;

#[derive(Error, Debug)]
pub enum LantaeError {
    #[error("HTTP request failed: {0}")]
    HttpError(#[from] reqwest::Error),
    
    #[error("JSON parsing error: {0}")]
    JsonError(#[from] serde_json::Error),
    
    #[error("Configuration error: {0}")]
    ConfigError(#[from] config::ConfigError),
    
    #[error("TOML serialization error: {0}")]
    TomlError(#[from] toml::ser::Error),
    
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    
    #[error("Provider error: {0}")]
    ProviderError(String),
    
    #[error("Invalid response: {0}")]
    InvalidResponse(String),
    
    #[error("API key not found for provider: {0}")]
    ApiKeyNotFound(String),
    
    #[error("Network timeout")]
    Timeout,
    
    #[error("Request aborted")]
    Aborted,
    
    #[error("Unknown error: {0}")]
    Unknown(String),
}

pub type Result<T> = std::result::Result<T, LantaeError>;