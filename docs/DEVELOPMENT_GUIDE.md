# Lantae Development Guide

## Overview

This guide provides comprehensive information for developers working on Lantae, covering both Ruby and LISP implementations.

## Development Setup

### Ruby Development

#### Prerequisites
```bash
# Ruby version manager (choose one)
curl -fsSL https://github.com/rbenv/rbenv-installer/raw/main/bin/rbenv-installer | bash
# OR
curl -sSL https://get.rvm.io | bash

# Install Ruby 3.0+
rbenv install 3.2.0
rbenv local 3.2.0
```

#### Dependencies
```bash
# Install bundler
gem install bundler

# Install project dependencies
bundle install

# Install development dependencies
bundle install --with development test
```

#### Running Tests
```bash
# Run all tests
bundle exec rspec

# Run specific test
bundle exec rspec spec/providers/ollama_spec.rb

# Run with coverage
COVERAGE=true bundle exec rspec
```

### LISP Development

#### Prerequisites
```bash
# Install SBCL
brew install sbcl              # macOS
apt-get install sbcl          # Ubuntu/Debian
pacman -S sbcl               # Arch Linux

# Install Quicklisp (optional, for dependencies)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)'
```

#### Development REPL
```bash
# Start development REPL
cd lisp/
sbcl --load start-repl.lisp

# In REPL, reload changes
> (reload)
```

#### Running Tests
```lisp
;; Load test framework
(load "test/test-framework.lisp")

;; Run all tests
(run-all-tests)

;; Run specific test suite
(run-test-suite :providers)
```

## Architecture Guidelines

### Code Organization

#### Ruby Structure
```
lib/
├── lantae.rb           # Main entry point
├── cli/                # CLI components
├── providers/          # Provider implementations
├── plugins/            # Plugin system
├── utils/              # Utility functions
└── models/             # Data models
```

#### LISP Structure
```
lisp/
├── lantae.lisp         # Main entry point
├── src/
│   ├── cli/           # CLI components
│   ├── providers/     # Provider implementations
│   ├── config/        # Configuration management
│   └── utils/         # Utility functions
└── test/              # Test suites
```

### Coding Standards

#### Ruby Style Guide
```ruby
# Use 2 spaces for indentation
def example_method
  if condition
    do_something
  end
end

# Use meaningful variable names
user_response = get_user_input
model_output = process_with_ai(user_response)

# Document public methods
# @param messages [Array<Hash>] Chat messages
# @return [String] AI response
def chat(messages)
  # Implementation
end
```

#### LISP Style Guide
```lisp
;; Use descriptive function names
(defun process-user-input (input)
  "Process user input and return formatted message"
  (trim-whitespace (downcase input)))

;; Use proper indentation
(defun complex-function (arg1 arg2)
  (let ((result (process arg1)))
    (if (valid-p result)
        (combine result arg2)
        (error "Invalid result"))))

;; Document functions
(defun chat-with-provider (provider messages &key (temperature 0.1))
  "Send chat messages to the specified provider.
   Returns a result object with the response."
  ...)
```

## Adding New Features

### Adding a New Provider

#### Ruby Implementation

1. Create provider class:
```ruby
# lib/providers/new_provider.rb
module Lantae
  module Providers
    class NewProvider < Base
      def initialize(config = {})
        super
        @api_key = config[:api_key] || ENV['NEW_PROVIDER_API_KEY']
        @base_url = config[:base_url] || 'https://api.newprovider.com'
      end

      def chat(messages, model: nil, temperature: nil)
        model ||= default_model
        temperature ||= @config[:temperature]
        
        response = post_request('/chat', {
          model: model,
          messages: messages,
          temperature: temperature
        })
        
        parse_chat_response(response)
      end

      def models
        response = get_request('/models')
        parse_models_response(response)
      end

      private

      def default_model
        'newprovider-base'
      end
    end
  end
end
```

2. Register provider:
```ruby
# lib/providers.rb
Lantae::Providers.register(:new_provider, Lantae::Providers::NewProvider)
```

#### LISP Implementation

1. Create provider functions:
```lisp
;; lisp/src/providers/new-provider.lisp
(defpackage :lantae-providers-new
  (:use :cl :lantae-providers))

(in-package :lantae-providers-new)

(defun make-new-provider (&key api-key base-url)
  "Create a new provider instance"
  (create-provider
   :name "new-provider"
   :chat-fn (lambda (model messages temperature)
              (new-provider-chat api-key base-url model messages temperature))
   :models-fn (lambda ()
                (new-provider-list-models api-key base-url))))

(defun new-provider-chat (api-key base-url model messages temperature)
  "Send chat request to new provider"
  (let ((request-body (create-chat-request model messages temperature)))
    (http-post (format nil "~A/chat" base-url)
               :headers `(("Authorization" . ,(format nil "Bearer ~A" api-key)))
               :body request-body)))
```

2. Register in initialization:
```lisp
;; Add to initialize-providers function
(when-let ((api-key (getenv "NEW_PROVIDER_API_KEY")))
  (register-provider (make-new-provider :api-key api-key)))
```

### Adding a New Command

#### Ruby CLI Command
```ruby
# lib/cli/commands/new_command.rb
module Lantae
  module CLI
    module Commands
      class NewCommand < Base
        def execute(args)
          # Parse arguments
          options = parse_options(args)
          
          # Execute command logic
          result = perform_action(options)
          
          # Display result
          output_result(result)
        end
        
        def help
          <<~HELP
            Usage: /newcommand [options]
            
            Description of what this command does
            
            Options:
              --option1    Description of option1
              --option2    Description of option2
          HELP
        end
      end
    end
  end
end
```

#### LISP CLI Command
```lisp
;; Add to commands.lisp
(defcommand newcommand (&rest args)
  "Description of what this command does"
  (let ((parsed-args (parse-command-args args)))
    (perform-new-command-action parsed-args)
    (format t "Command executed successfully~%")))

;; Register command alias
(define-command-alias nc newcommand)
```

## Testing Guidelines

### Unit Testing

#### Ruby Testing with RSpec
```ruby
# spec/providers/new_provider_spec.rb
RSpec.describe Lantae::Providers::NewProvider do
  let(:provider) { described_class.new(api_key: 'test-key') }
  
  describe '#chat' do
    it 'sends chat request to provider' do
      messages = [{ role: 'user', content: 'Hello' }]
      
      stub_request(:post, 'https://api.newprovider.com/chat')
        .with(body: hash_including(messages: messages))
        .to_return(status: 200, body: { response: 'Hi there!' }.to_json)
      
      response = provider.chat(messages)
      
      expect(response).to eq('Hi there!')
    end
  end
end
```

#### LISP Testing
```lisp
;; test/providers/new-provider-test.lisp
(defpackage :lantae-test-new-provider
  (:use :cl :lantae-test))

(in-package :lantae-test-new-provider)

(deftest test-new-provider-chat ()
  "Test new provider chat functionality"
  (with-mock-http-requests
    (mock-post "https://api.newprovider.com/chat"
               :response '(:status 200 :body "{\"response\": \"Hi there!\"}"))
    
    (let* ((provider (make-new-provider :api-key "test-key"))
           (result (provider-chat "new-provider" "model"
                                '((:role "user" :content "Hello")))))
      (assert-equal "Hi there!" (get-response-content result)))))
```

### Integration Testing

Create integration tests that verify end-to-end functionality:

```ruby
# spec/integration/chat_flow_spec.rb
RSpec.describe 'Chat Flow Integration' do
  it 'completes full chat interaction' do
    # Setup
    provider = Lantae::Providers::Ollama.new
    
    # Execute
    response = provider.chat([
      { role: 'user', content: 'What is 2+2?' }
    ])
    
    # Verify
    expect(response).to include('4')
  end
end
```

## Debugging

### Ruby Debugging

```ruby
# Add debugging statements
require 'debug'  # Ruby 3.0+

def problematic_method
  debugger  # Execution stops here
  complex_logic
end

# Or use pry
require 'pry'
binding.pry  # Interactive debugging session
```

### LISP Debugging

```lisp
;; Enable debugging
(declaim (optimize (debug 3)))

;; Add breakpoints
(defun problematic-function ()
  (break "Debugging problematic-function")  ; Stops here
  (complex-logic))

;; Trace function calls
(trace problematic-function)
(untrace problematic-function)

;; Inspect values
(inspect *current-config*)
```

## Performance Optimization

### Profiling

#### Ruby Profiling
```ruby
# Use ruby-prof
require 'ruby-prof'

RubyProf.start
# Code to profile
result = RubyProf.stop

printer = RubyProf::FlatPrinter.new(result)
printer.print(STDOUT)
```

#### LISP Profiling
```lisp
;; Use SBCL's statistical profiler
(require :sb-sprof)

(sb-sprof:with-profiling (:report :flat)
  (function-to-profile))

;; Time specific operations
(time (complex-operation))
```

### Optimization Tips

1. **Caching**: Cache expensive operations
2. **Lazy Loading**: Load providers on demand
3. **Connection Pooling**: Reuse HTTP connections
4. **Async Operations**: Use concurrent processing where appropriate

## Release Process

### Version Bumping

1. Update version in:
   - `lib/lantae/version.rb` (Ruby)
   - `lisp/lantae.lisp` (LISP version constant)
   - `README.md`

2. Update CHANGELOG.md

3. Create git tag:
```bash
git tag -a v1.2.3 -m "Release version 1.2.3"
git push origin v1.2.3
```

### Release Checklist

- [ ] All tests passing
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] Version numbers updated
- [ ] Git tag created
- [ ] Release notes written
- [ ] Binaries built (if applicable)

## Contributing

### Pull Request Process

1. Fork the repository
2. Create feature branch: `git checkout -b feature/amazing-feature`
3. Make changes and commit: `git commit -m 'Add amazing feature'`
4. Push to branch: `git push origin feature/amazing-feature`
5. Open Pull Request

### Code Review Guidelines

- Ensure tests pass
- Follow coding standards
- Update documentation
- Add tests for new features
- Keep commits focused and atomic

## Resources

### Documentation
- [Architecture Guide](ARCHITECTURE.md)
- [Migration Guide](MIGRATION_GUIDE.md)
- [API Reference](API_REFERENCE.md)

### Tools
- Ruby: RSpec, Pry, RuboCop
- LISP: SBCL, Quicklisp, SLIME
- General: Git, Docker, CI/CD

### Community
- GitHub Issues: Bug reports and features
- Discussions: General questions
- Wiki: Extended documentation