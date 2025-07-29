use crate::utils::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub role: String,
    pub content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatRequest {
    pub model: String,
    pub messages: Vec<Message>,
    pub temperature: Option<f32>,
    pub stream: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChatResponse {
    pub content: String,
    pub model: String,
    pub usage: Option<Usage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Usage {
    pub input_tokens: u32,
    pub output_tokens: u32,
    pub total_tokens: u32,
}

#[derive(Debug, Clone)]
pub struct ProviderConfig {
    pub api_key: Option<String>,
    pub base_url: String,
    pub timeout: u64,
    pub max_retries: u32,
}

impl Default for ProviderConfig {
    fn default() -> Self {
        Self {
            api_key: None,
            base_url: String::new(),
            timeout: 60,
            max_retries: 3,
        }
    }
}

#[async_trait]
pub trait Provider {
    fn name(&self) -> &str;
    fn set_config(&mut self, config: ProviderConfig);
    fn get_config(&self) -> &ProviderConfig;
    async fn chat(&self, request: ChatRequest) -> Result<ChatResponse>;
    async fn list_models(&self) -> Result<Vec<String>>;
    fn supports_streaming(&self) -> bool { false }
    
    // Optional streaming support
    async fn chat_stream(&self, _request: ChatRequest) -> Result<tokio::sync::mpsc::Receiver<String>> {
        Err(crate::utils::LantaeError::ProviderError(
            "Streaming not supported by this provider".to_string()
        ))
    }
}