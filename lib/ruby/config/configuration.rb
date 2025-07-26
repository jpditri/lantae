require 'yaml'
require 'json'

module Lantae
  module Config
    class Configuration
      DEFAULT_CONFIG = {
        model: 'cogito:latest',
        provider: 'ollama',
        url: 'http://localhost:11434',
        region: 'us-east-1',
        secret: 'lantae/api-keys',
        temperature: 0.1,
        auto_accept: false,
        planning_mode: false,
        agent_mode: false,
        no_banner: false,
        enable_mcp: false,
        enable_lsp: false,
        mcp_config: nil,
        max_retries: 3,
        retry_delay: 1.0,
        circuit_breaker: {
          failure_threshold: 5,
          timeout: 60,
          half_open_max_calls: 3
        },
        performance: {
          enable_caching: true,
          cache_ttl: 300,
          enable_pooling: true,
          pool_size: 5,
          request_timeout: 30
        },
        security: {
          enable_rate_limiting: true,
          requests_per_minute: 60,
          enable_audit_log: false,
          audit_log_path: 'logs/audit.log'
        }
      }.freeze

      attr_reader :config, :config_file

      def initialize(config_file = nil)
        @config_file = config_file || find_config_file
        @config = DEFAULT_CONFIG.dup
        @watchers = []
        
        load_configuration
        setup_environment_overrides
      end

      def get(key, default = nil)
        keys = key.to_s.split('.')
        value = @config
        
        keys.each do |k|
          if value.is_a?(Hash)
            value = value[k.to_sym] || value[k.to_s]
          else
            return default
          end
        end
        
        value.nil? ? default : value
      end

      def set(key, value)
        keys = key.to_s.split('.')
        target = @config
        
        keys[0..-2].each do |k|
          target[k.to_sym] ||= {}
          target = target[k.to_sym]
        end
        
        target[keys.last.to_sym] = value
        notify_watchers(key, value)
      end

      def merge!(hash)
        @config = deep_merge(@config, hash)
        notify_watchers('config.merged', hash)
      end

      def reload!
        old_config = @config.dup
        @config = DEFAULT_CONFIG.dup
        
        load_configuration
        setup_environment_overrides
        
        notify_watchers('config.reloaded', { old: old_config, new: @config })
      end

      def save!
        return unless @config_file
        
        begin
          File.write(@config_file, YAML.dump(@config))
          notify_watchers('config.saved', @config_file)
        rescue => e
          raise "Failed to save configuration to #{@config_file}: #{e.message}"
        end
      end

      def watch(pattern = nil, &block)
        watcher = { pattern: pattern, callback: block }
        @watchers << watcher
        watcher
      end

      def unwatch(watcher)
        @watchers.delete(watcher)
      end

      def validate!
        errors = []
        
        # Validate required fields
        errors << "Provider '#{get(:provider)}' is not supported" unless valid_provider?
        errors << "Temperature must be between 0 and 2" unless valid_temperature?
        errors << "Region must be a valid AWS region" unless valid_region?
        
        # Validate performance settings
        perf = get(:performance, {})
        errors << "Cache TTL must be positive" if perf[:cache_ttl] && perf[:cache_ttl] <= 0
        errors << "Pool size must be positive" if perf[:pool_size] && perf[:pool_size] <= 0
        
        # Validate security settings
        security = get(:security, {})
        if security[:requests_per_minute] && security[:requests_per_minute] <= 0
          errors << "Requests per minute must be positive"
        end
        
        raise "Configuration validation failed: #{errors.join(', ')}" unless errors.empty?
      end

      def to_h
        @config.dup
      end

      def to_yaml
        YAML.dump(@config)
      end

      def to_json
        JSON.pretty_generate(@config)
      end

      private

      def find_config_file
        config_paths = [
          'lantae.yml',
          'config/lantae.yml',
          File.expand_path('~/.lantae/config.yml'),
          '/etc/lantae/config.yml'
        ]
        
        config_paths.find { |path| File.exist?(path) }
      end

      def load_configuration
        return unless @config_file && File.exist?(@config_file)
        
        begin
          file_config = YAML.load_file(@config_file)
          @config = deep_merge(@config, file_config) if file_config.is_a?(Hash)
        rescue => e
          puts "Warning: Failed to load configuration from #{@config_file}: #{e.message}"
        end
      end

      def setup_environment_overrides
        # Override with environment variables
        env_mappings = {
          'LANTAE_MODEL' => :model,
          'LANTAE_PROVIDER' => :provider,
          'LANTAE_TEMPERATURE' => :temperature,
          'LANTAE_AUTO_ACCEPT' => :auto_accept,
          'LANTAE_ENABLE_MCP' => :enable_mcp,
          'LANTAE_ENABLE_LSP' => :enable_lsp,
          'AWS_REGION' => :region
        }
        
        env_mappings.each do |env_var, config_key|
          value = ENV[env_var]
          next unless value
          
          # Convert string values to appropriate types
          @config[config_key] = convert_env_value(value)
        end
      end

      def convert_env_value(value)
        case value.downcase
        when 'true', 'yes', '1'
          true
        when 'false', 'no', '0'
          false
        else
          # Try to convert to number
          if value.match?(/^\d+$/)
            value.to_i
          elsif value.match?(/^\d+\.\d+$/)
            value.to_f
          else
            value
          end
        end
      end

      def deep_merge(hash1, hash2)
        result = hash1.dup
        
        hash2.each do |key, value|
          key = key.to_sym
          
          if result[key].is_a?(Hash) && value.is_a?(Hash)
            result[key] = deep_merge(result[key], value)
          else
            result[key] = value
          end
        end
        
        result
      end

      def notify_watchers(key, value)
        @watchers.each do |watcher|
          if watcher[:pattern].nil? || key.to_s.match?(watcher[:pattern])
            begin
              watcher[:callback].call(key, value)
            rescue => e
              puts "Warning: Configuration watcher error: #{e.message}"
            end
          end
        end
      end

      def valid_provider?
        valid_providers = %w[ollama openai anthropic bedrock gemini mistral perplexity]
        valid_providers.include?(get(:provider))
      end

      def valid_temperature?
        temp = get(:temperature)
        temp.is_a?(Numeric) && temp >= 0 && temp <= 2
      end

      def valid_region?
        region = get(:region)
        return true unless region # Optional field
        
        # Basic AWS region validation
        region.match?(/^[a-z]{2}-[a-z]+-\d{1}$/)
      end
    end
  end
end