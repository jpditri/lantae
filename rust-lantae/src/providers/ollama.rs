use super::base::{Provider, ProviderConfig, ChatRequest, ChatResponse, Usage};
use crate::utils::{Result, LantaeError};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::time::Duration;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OllamaRequest {
    model: String,
    messages: Vec<OllamaMessage>,
    stream: bool,
    options: Option<OllamaOptions>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OllamaMessage {
    role: String,
    content: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OllamaOptions {
    temperature: Option<f32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OllamaResponse {
    message: OllamaMessage,
    model: String,
    done: bool,
    #[serde(rename = "eval_count")]
    eval_count: Option<u32>,
    #[serde(rename = "prompt_eval_count")]
    prompt_eval_count: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OllamaModelsResponse {
    models: Vec<OllamaModel>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OllamaModel {
    name: String,
    model: String,
    modified_at: String,
    size: u64,
}

pub struct OllamaProvider {
    config: ProviderConfig,
    client: reqwest::Client,
}

impl OllamaProvider {
    pub fn new() -> Self {
        let mut config = ProviderConfig::default();
        config.base_url = "http://localhost:11434".to_string();
        
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(config.timeout))
            .build()
            .expect("Failed to create HTTP client");
            
        Self { config, client }
    }
}

#[async_trait]
impl Provider for OllamaProvider {
    fn name(&self) -> &str {
        "ollama"
    }
    
    fn set_config(&mut self, config: ProviderConfig) {
        self.config = config;
        // Recreate client with new timeout
        self.client = reqwest::Client::builder()
            .timeout(Duration::from_secs(self.config.timeout))
            .build()
            .expect("Failed to create HTTP client");
    }
    
    fn get_config(&self) -> &ProviderConfig {
        &self.config
    }
    
    async fn chat(&self, request: ChatRequest) -> Result<ChatResponse> {
        let url = format!("{}/api/chat", self.config.base_url);
        
        let ollama_messages: Vec<OllamaMessage> = request.messages
            .iter()
            .map(|msg| OllamaMessage {
                role: msg.role.clone(),
                content: msg.content.clone(),
            })
            .collect();
            
        let ollama_request = OllamaRequest {
            model: request.model.clone(),
            messages: ollama_messages,
            stream: false,
            options: request.temperature.map(|temp| OllamaOptions {
                temperature: Some(temp),
            }),
        };
        
        tracing::debug!("Sending Ollama request: {:?}", ollama_request);
        
        let response = self.client
            .post(&url)
            .json(&ollama_request)
            .send()
            .await?;
            
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            return Err(LantaeError::ProviderError(
                format!("Ollama request failed with status {}: {}", status, text)
            ));
        }
        
        let ollama_response: OllamaResponse = response.json().await?;
        
        tracing::debug!("Received Ollama response: {:?}", ollama_response);
        
        let usage = Usage {
            input_tokens: ollama_response.prompt_eval_count.unwrap_or(0),
            output_tokens: ollama_response.eval_count.unwrap_or(0),
            total_tokens: ollama_response.prompt_eval_count.unwrap_or(0) + ollama_response.eval_count.unwrap_or(0),
        };
        
        Ok(ChatResponse {
            content: ollama_response.message.content,
            model: ollama_response.model,
            usage: Some(usage),
        })
    }
    
    async fn list_models(&self) -> Result<Vec<String>> {
        let url = format!("{}/api/tags", self.config.base_url);
        
        let response = self.client
            .get(&url)
            .send()
            .await?;
            
        if !response.status().is_success() {
            let status = response.status();
            let text = response.text().await.unwrap_or_default();
            return Err(LantaeError::ProviderError(
                format!("Failed to list Ollama models with status {}: {}", status, text)
            ));
        }
        
        let models_response: OllamaModelsResponse = response.json().await?;
        
        Ok(models_response.models
            .into_iter()
            .map(|model| model.name)
            .collect())
    }
    
    fn supports_streaming(&self) -> bool {
        true
    }
}