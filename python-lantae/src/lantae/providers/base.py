"""
Base provider interface for Python implementation
"""

from abc import ABC, abstractmethod
from typing import Dict, List, Optional, AsyncIterator
from dataclasses import dataclass
from enum import Enum

class MessageRole(Enum):
    """Message roles for chat conversations."""
    USER = "user"
    ASSISTANT = "assistant"
    SYSTEM = "system"

@dataclass
class ChatMessage:
    """A single chat message."""
    role: MessageRole
    content: str

@dataclass 
class Usage:
    """Token usage information."""
    prompt_tokens: int
    completion_tokens: int
    total_tokens: int
    cost: Optional[float] = None

@dataclass
class ChatRequest:
    """Request for chat completion."""
    messages: List[ChatMessage]
    model: Optional[str] = None
    temperature: Optional[float] = None
    max_tokens: Optional[int] = None
    stream: bool = False

@dataclass
class ChatResponse:
    """Response from chat completion."""
    message: str
    provider: str
    model: str
    finish_reason: Optional[str] = None
    usage: Optional[Dict] = None

@dataclass
class StreamChunk:
    """A chunk of streaming response."""
    delta: str
    done: bool
    usage: Optional[Usage] = None

class BaseProvider(ABC):
    """Abstract base class for all LLM providers."""
    
    def __init__(self, config: Dict):
        """Initialize provider with configuration.
        
        Args:
            config: Provider-specific configuration
        """
        self.config = config
        self.name = self.get_name()
    
    @abstractmethod
    def get_name(self) -> str:
        """Get the provider name."""
        pass
    
    @abstractmethod
    async def chat(self, request: ChatRequest) -> ChatResponse:
        """Send chat request and get response.
        
        Args:
            request: The chat request
            
        Returns:
            ChatResponse with the AI's reply
        """
        pass
    
    @abstractmethod
    async def stream(self, request: ChatRequest) -> AsyncIterator[StreamChunk]:
        """Send chat request and stream response.
        
        Args:
            request: The chat request
            
        Yields:
            StreamChunk objects as the response streams
        """
        pass
    
    @abstractmethod
    async def get_models(self) -> List[str]:
        """Get list of available models for this provider.
        
        Returns:
            List of model names
        """
        pass
    
    def validate_config(self) -> bool:
        """Validate provider configuration.
        
        Returns:
            True if configuration is valid
        """
        # Default implementation - can be overridden
        return True
    
    def _validate_request(self, request: ChatRequest) -> None:
        """Validate chat request.
        
        Args:
            request: The request to validate
            
        Raises:
            ValueError: If request is invalid
        """
        if not request.messages:
            raise ValueError("Messages list cannot be empty")
        
        for message in request.messages:
            if not message.content.strip():
                raise ValueError("Message content cannot be empty")
    
    def _handle_error(self, error: Exception) -> Exception:
        """Handle and format errors.
        
        Args:
            error: The original error
            
        Returns:
            Formatted error
        """
        if isinstance(error, Exception):
            return error
        return Exception(f"Unknown error: {str(error)}")