"""
Pydantic models for configuration management
"""

from typing import Dict, Optional, Any
from pydantic import BaseModel, Field

class ProviderConfig(BaseModel):
    """Configuration for a specific provider."""
    api_key_env: Optional[str] = None
    api_key: Optional[str] = None
    base_url: Optional[str] = None
    timeout: Optional[int] = 60
    default_model: Optional[str] = None
    extra_params: Optional[Dict[str, Any]] = Field(default_factory=dict)

class UIConfig(BaseModel):
    """UI configuration options."""
    colors: bool = True
    progress_bars: bool = True
    rich_formatting: bool = True
    banner: bool = True

class DataScienceConfig(BaseModel):
    """Data science specific configuration."""
    default_plot_backend: str = "matplotlib"
    jupyter_integration: bool = True
    auto_save_plots: bool = True
    plot_directory: str = "./plots"
    default_figsize: tuple = (10, 6)

class LoggingConfig(BaseModel):
    """Logging configuration."""
    level: str = "INFO"
    file: Optional[str] = None
    console: bool = True
    format: str = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"

class DefaultConfig(BaseModel):
    """Default configuration options."""
    provider: str = "ollama"
    model: str = "cogito:latest"
    temperature: float = 0.7
    max_tokens: Optional[int] = None

class LantaeConfig(BaseModel):
    """Main Lantae configuration."""
    default: DefaultConfig = Field(default_factory=DefaultConfig)
    providers: Dict[str, ProviderConfig] = Field(default_factory=dict)
    ui: UIConfig = Field(default_factory=UIConfig)
    data_science: DataScienceConfig = Field(default_factory=DataScienceConfig)
    logging: LoggingConfig = Field(default_factory=LoggingConfig)
    
    class Config:
        """Pydantic configuration."""
        extra = "allow"  # Allow extra fields for extensibility