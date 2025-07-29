use super::base::{Provider, ProviderConfig, ChatRequest, ChatResponse, Usage};
use crate::utils::{Result, LantaeError};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::time::Duration;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AnthropicRequest {
    model: String,
    messages: Vec<AnthropicMessage>,
    max_tokens: u32,
    temperature: Option<f32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AnthropicMessage {
    role: String,
    content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AnthropicResponse {
    content: Vec<AnthropicContent>,
    model: String,
    usage: Option<AnthropicUsage>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AnthropicContent {
    #[serde(rename = "type")]
    content_type: String,
    text: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct AnthropicUsage {
    input_tokens: u32,
    output_tokens: u32,
}

pub struct AnthropicProvider {
    config: ProviderConfig,
    client: reqwest::Client,
}

impl AnthropicProvider {
    pub fn new() -> Self {
        let mut config = ProviderConfig::default();
        config.base_url = "https://api.anthropic.com/v1".to_string();
        
        // Try to get API key from environment
        if let Ok(api_key) = std::env::var("ANTHROPIC_API_KEY") {
            config.api_key = Some(api_key);
        }
        
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(config.timeout))
            .build()
            .expect("Failed to create HTTP client");
            
        Self { config, client }
    }
}

#[async_trait]
impl Provider for AnthropicProvider {
    fn name(&self) -> &str {
        "anthropic"
    }
    
    fn set_config(&mut self, config: ProviderConfig) {
        self.config = config;
        self.client = reqwest::Client::builder()
            .timeout(Duration::from_secs(self.config.timeout))
            .build()
            .expect("Failed to create HTTP client");
    }
    
    fn get_config(&self) -> &ProviderConfig {
        &self.config
    }
    
    async fn chat(&self, request: ChatRequest) -> Result<ChatResponse> {
        let api_key = self.config.api_key.as_ref()
            .ok_or_else(|| LantaeError::ApiKeyNotFound("anthropic".to_string()))?;
            
        let url = format!("{}/messages", self.config.base_url);
        
        let anthropic_messages: Vec<AnthropicMessage> = request.messages
            .iter()
            .map(|msg| AnthropicMessage {
                role: msg.role.clone(),
                content: msg.content.clone(),
            })
            .collect();
            
        let anthropic_request = AnthropicRequest {
            model: request.model.clone(),
            messages: anthropic_messages,
            max_tokens: 4096, // Default max tokens
            temperature: request.temperature,
        };
        
        tracing::debug!("Sending Anthropic request: {:?}", anthropic_request);
        
        let response = self.client
            .post(&url)
            .header("x-api-key", api_key)
            .header("anthropic-version", "2023-06-01")
            .header("Content-Type", "application/json")
            .json(&anthropic_request)
            .send()
            .await?;
            
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            return Err(LantaeError::ProviderError(
                format!("Anthropic request failed with status {}: {}", status, text)
            ));
        }
        
        let anthropic_response: AnthropicResponse = response.json().await?;
        
        tracing::debug!("Received Anthropic response: {:?}", anthropic_response);
        
        let content = anthropic_response.content.first()
            .ok_or_else(|| LantaeError::InvalidResponse("No content in response".to_string()))?;
            
        let usage = anthropic_response.usage.map(|u| Usage {
            input_tokens: u.input_tokens,
            output_tokens: u.output_tokens,
            total_tokens: u.input_tokens + u.output_tokens,
        });
        
        Ok(ChatResponse {
            content: content.text.clone(),
            model: anthropic_response.model,
            usage,
        })
    }
    
    async fn list_models(&self) -> Result<Vec<String>> {
        // Basic model list for Anthropic
        Ok(vec![
            "claude-3-5-sonnet-20241022".to_string(),
            "claude-3-5-haiku-20241022".to_string(),
            "claude-3-opus-20240229".to_string(),
            "claude-3-sonnet-20240229".to_string(),
            "claude-3-haiku-20240307".to_string(),
        ])
    }
}