use super::base::{Provider, ProviderConfig, ChatRequest, ChatResponse, Usage};
use crate::utils::{Result, LantaeError};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::time::Duration;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OpenAIRequest {
    model: String,
    messages: Vec<OpenAIMessage>,
    temperature: Option<f32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OpenAIMessage {
    role: String,
    content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OpenAIResponse {
    choices: Vec<OpenAIChoice>,
    usage: Option<OpenAIUsage>,
    model: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OpenAIChoice {
    message: OpenAIMessage,
    index: u32,
    finish_reason: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OpenAIUsage {
    prompt_tokens: u32,
    completion_tokens: u32,
    total_tokens: u32,
}

pub struct OpenAIProvider {
    config: ProviderConfig,
    client: reqwest::Client,
}

impl OpenAIProvider {
    pub fn new() -> Self {
        let mut config = ProviderConfig::default();
        config.base_url = "https://api.openai.com/v1".to_string();
        
        // Try to get API key from environment
        if let Ok(api_key) = std::env::var("OPENAI_API_KEY") {
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
impl Provider for OpenAIProvider {
    fn name(&self) -> &str {
        "openai"
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
            .ok_or_else(|| LantaeError::ApiKeyNotFound("openai".to_string()))?;
            
        let url = format!("{}/chat/completions", self.config.base_url);
        
        let openai_messages: Vec<OpenAIMessage> = request.messages
            .iter()
            .map(|msg| OpenAIMessage {
                role: msg.role.clone(),
                content: msg.content.clone(),
            })
            .collect();
            
        let openai_request = OpenAIRequest {
            model: request.model.clone(),
            messages: openai_messages,
            temperature: request.temperature,
        };
        
        tracing::debug!("Sending OpenAI request: {:?}", openai_request);
        
        let response = self.client
            .post(&url)
            .header("Authorization", format!("Bearer {}", api_key))
            .header("Content-Type", "application/json")
            .json(&openai_request)
            .send()
            .await?;
            
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            return Err(LantaeError::ProviderError(
                format!("OpenAI request failed with status {}: {}", status, text)
            ));
        }
        
        let openai_response: OpenAIResponse = response.json().await?;
        
        tracing::debug!("Received OpenAI response: {:?}", openai_response);
        
        let choice = openai_response.choices.first()
            .ok_or_else(|| LantaeError::InvalidResponse("No choices in response".to_string()))?;
            
        let usage = openai_response.usage.map(|u| Usage {
            input_tokens: u.prompt_tokens,
            output_tokens: u.completion_tokens,
            total_tokens: u.total_tokens,
        });
        
        Ok(ChatResponse {
            content: choice.message.content.clone(),
            model: openai_response.model,
            usage,
        })
    }
    
    async fn list_models(&self) -> Result<Vec<String>> {
        // Basic model list for OpenAI
        Ok(vec![
            "gpt-4o".to_string(),
            "gpt-4o-mini".to_string(),
            "gpt-4-turbo".to_string(),
            "gpt-3.5-turbo".to_string(),
            "o1-preview".to_string(),
            "o1-mini".to_string(),
        ])
    }
}