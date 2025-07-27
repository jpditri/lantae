# Lantae Python Implementation

Python implementation of Lantae optimized for data science workflows and ML integration.

## 🐍 Python Features

### Implementation Status
- 🔄 **In Development** - See [Feature Parity Status](#feature-parity-status)
- 📊 **Data Science Ready** - Pandas, NumPy, Jupyter integration
- 🤖 **ML Workflow** - Seamless ML pipeline integration
- 📈 **Analytics Focus** - Built for data analysis and visualization

### Python-Specific Advantages
- **Rich Ecosystem** - Access to extensive ML and data science libraries
- **Jupyter Integration** - Native notebook support for interactive development
- **Data Processing** - Pandas and NumPy for data manipulation
- **Visualization** - Matplotlib, Seaborn, Plotly integration
- **Scientific Computing** - SciPy, scikit-learn compatibility

## 🚀 Quick Start

### Prerequisites
- **Python** 3.9+ (install via [python.org](https://python.org/))
- **Poetry** (recommended) or **pip**

### Installation
```bash
# Clone the Python implementation
git clone -b python-implementation https://github.com/jpditri/lantae-cli.git
cd lantae-cli/python-lantae

# Install with Poetry (recommended)
poetry install

# Or with pip
pip install -e .
```

### Development Setup
```bash
# Install with development dependencies
poetry install --with dev

# Activate virtual environment
poetry shell

# Install pre-commit hooks
pre-commit install

# Run tests
pytest

# Format code
black src/ tests/
isort src/ tests/

# Type checking
mypy src/
```

## 📖 Usage

### Command Line Interface
```bash
# Interactive mode
lantae

# Single prompt
lantae "Analyze this dataset for trends"

# Specify provider and model
lantae --provider openai --model gpt-4o "Explain pandas DataFrame operations"

# Help
lantae --help
```

### Python API
```python
from lantae import Lantae
import pandas as pd

# Initialize client
client = Lantae(provider="openai")

# Basic chat
response = await client.chat("Hello, world!")
print(response.message)

# Data analysis workflow
df = pd.read_csv("data.csv")
analysis_prompt = f"Analyze this dataset:\n{df.describe()}"
insights = await client.chat(analysis_prompt)
```

### Jupyter Notebook Integration
```python
# In Jupyter notebook
%load_ext lantae
%%lantae openai gpt-4o
Analyze the correlation between these variables and suggest
next steps for our machine learning model.
```

### Data Science Workflow
```python
import pandas as pd
import numpy as np
from lantae import Lantae, DataAnalyzer

# Load data
df = pd.read_csv("sales_data.csv")

# Initialize AI analyst
analyzer = DataAnalyzer(provider="anthropic")

# Automated analysis
report = await analyzer.analyze_dataframe(
    df, 
    focus="sales trends",
    include_visualizations=True
)

# Generate insights
insights = await analyzer.generate_insights(df)
recommendations = await analyzer.suggest_next_steps(insights)
```

## 🔧 Configuration

### Configuration File
```yaml
# ~/.config/lantae/config.yaml
default:
  provider: ollama
  model: cogito:latest
  temperature: 0.7

providers:
  ollama:
    host: http://localhost:11434
    timeout: 30

  openai:
    api_key_env: OPENAI_API_KEY
    base_url: https://api.openai.com/v1
    timeout: 60

  anthropic:
    api_key_env: ANTHROPIC_API_KEY
    base_url: https://api.anthropic.com
    timeout: 60

data_science:
  default_plot_backend: matplotlib
  jupyter_integration: true
  auto_save_plots: true
  plot_directory: ./plots

ui:
  colors: true
  progress_bars: true
  rich_formatting: true
```

### Environment Variables
```bash
# API Keys
export OPENAI_API_KEY="your-openai-key"
export ANTHROPIC_API_KEY="your-anthropic-key"
export GEMINI_API_KEY="your-gemini-key"

# Python-specific
export LANTAE_CONFIG_PATH="~/.config/lantae/config.yaml"
export LANTAE_DATA_DIR="./data"
export LANTAE_PLOTS_DIR="./plots"
```

## 🏗️ Architecture

### Project Structure
```
python-lantae/
├── pyproject.toml          # Project configuration
├── README.md               # Documentation
├── src/
│   └── lantae/
│       ├── __init__.py     # Package initialization
│       ├── cli/
│       │   ├── __init__.py
│       │   ├── main.py     # CLI entry point
│       │   ├── commands.py # Command definitions
│       │   └── repl.py     # REPL implementation
│       ├── providers/
│       │   ├── __init__.py
│       │   ├── base.py     # Provider base class
│       │   ├── ollama.py   # Ollama implementation
│       │   ├── openai.py   # OpenAI implementation
│       │   └── anthropic.py # Anthropic implementation
│       ├── config/
│       │   ├── __init__.py
│       │   ├── manager.py  # Configuration management
│       │   └── models.py   # Pydantic models
│       ├── tools/
│       │   ├── __init__.py
│       │   ├── executor.py # Tool execution
│       │   └── data_tools.py # Data science tools
│       ├── agent/
│       │   ├── __init__.py
│       │   ├── planner.py  # Task planning
│       │   └── data_analyzer.py # Data analysis agent
│       ├── utils/
│       │   ├── __init__.py
│       │   ├── logging.py  # Logging utilities
│       │   ├── errors.py   # Exception classes
│       │   └── helpers.py  # Helper functions
│       └── jupyter/
│           ├── __init__.py
│           ├── magic.py    # Jupyter magic commands
│           └── widgets.py  # Interactive widgets
├── tests/                  # Test files
├── docs/                   # Documentation
└── notebooks/              # Example notebooks
```

### Core Modules
- **CLI**: Rich command-line interface with Click
- **Providers**: Multi-provider LLM interface
- **Config**: YAML/Pydantic configuration management
- **Tools**: Data science and general tools
- **Agent**: AI agents for data analysis
- **Jupyter**: Notebook integration and magic commands

## 🔄 Feature Parity Status

See the main [Feature Parity Document](../docs/FEATURE_PARITY.md) for detailed status compared to other implementations.

### Python Implementation Roadmap

#### Phase 1: Core Python Setup
- [ ] **Poetry Project** - Package management and dependencies
- [ ] **CLI with Click** - Rich command-line interface
- [ ] **Pydantic Config** - Type-safe configuration
- [ ] **Basic Provider** - Ollama integration
- [ ] **Rich REPL** - Beautiful interactive interface

#### Phase 2: Data Science Integration
- [ ] **Pandas Integration** - DataFrame analysis
- [ ] **NumPy Support** - Numerical computations
- [ ] **Jupyter Magic** - Notebook magic commands
- [ ] **Visualization** - Matplotlib/Plotly integration
- [ ] **Data Tools** - CSV, JSON, database connectors

#### Phase 3: Provider Ecosystem
- [ ] **OpenAI Provider** - GPT model support
- [ ] **Anthropic Provider** - Claude integration
- [ ] **Gemini Provider** - Google AI support
- [ ] **Async Support** - Asyncio for performance
- [ ] **Streaming** - Real-time response handling

#### Phase 4: ML Workflow Integration
- [ ] **scikit-learn** - ML model integration
- [ ] **Data Analyzer Agent** - Automated analysis
- [ ] **Pipeline Integration** - ML pipeline support
- [ ] **Model Evaluation** - Performance metrics
- [ ] **Report Generation** - Automated reporting

## 🛠️ Development

### Building and Testing
```bash
# Install dependencies
poetry install --with dev

# Run tests
pytest

# Run tests with coverage
pytest --cov=src/lantae --cov-report=html

# Type checking
mypy src/

# Linting and formatting
flake8 src/
black src/ tests/
isort src/ tests/
```

### Package Management
```bash
# Add dependency
poetry add pandas

# Add development dependency
poetry add --group dev pytest

# Update dependencies
poetry update

# Build package
poetry build

# Publish to PyPI
poetry publish
```

### Data Science Features
```bash
# Install with data science extras
poetry install --extras "data jupyter"

# Start Jupyter with Lantae
jupyter lab

# Run data analysis example
python examples/data_analysis.py
```

## 📊 Data Science Examples

### DataFrame Analysis
```python
import pandas as pd
from lantae import DataAnalyzer

# Load your data
df = pd.read_csv("sales.csv")

# AI-powered analysis
analyzer = DataAnalyzer()
insights = await analyzer.quick_analysis(df)
print(insights.summary)
```

### Jupyter Magic Commands
```python
# In Jupyter notebook
%load_ext lantae

# Analyze data with AI
%%lantae_analyze
df = pd.read_csv("data.csv")
# AI will analyze this DataFrame
```

### ML Pipeline Integration
```python
from sklearn.model_selection import train_test_split
from lantae import MLAgent

# Prepare data
X_train, X_test, y_train, y_test = train_test_split(X, y)

# AI-assisted model selection
ml_agent = MLAgent()
best_model = await ml_agent.suggest_model(X_train, y_train)
evaluation = await ml_agent.evaluate_model(best_model, X_test, y_test)
```

## 🤝 Contributing

### Python-Specific Guidelines
1. **Follow PEP 8** - Use black and isort for formatting
2. **Type annotations** - Use mypy for type checking
3. **Docstrings** - Use Google-style docstrings
4. **Testing** - Write pytest tests with good coverage
5. **Data science focus** - Consider pandas/numpy integration

### Code Style
- Use Poetry for dependency management
- Follow Black formatting (100 char line length)
- Use type hints everywhere
- Write comprehensive docstrings
- Test data science functionality

## 📚 Resources

- [Python Documentation](https://docs.python.org/3/)
- [Poetry Dependency Management](https://python-poetry.org/docs/)
- [Pandas Documentation](https://pandas.pydata.org/docs/)
- [Jupyter Notebook](https://jupyter.org/documentation)
- [Rich CLI Library](https://rich.readthedocs.io/)

---

*This Python implementation focuses on data science workflows and ML integration while maintaining feature parity with the Ruby reference implementation.*