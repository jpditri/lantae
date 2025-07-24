#!/usr/bin/env ruby

require_relative 'mcp_client'
require 'yaml'
require 'json'

# MCP Server Discovery and Connection Management
class MCPManager
  attr_reader :servers, :connected_servers
  
  def initialize
    @servers = {}
    @connected_servers = {}
    @server_configs = []
  end

  def load_server_configs(config_path = nil)
    config_path ||= find_default_config_path
    
    unless config_path && File.exist?(config_path)
      puts "No MCP server config found at #{config_path || 'default locations'}"
      return false
    end
    
    begin
      config_data = YAML.load_file(config_path)
      @server_configs = config_data['mcp_servers'] || []
      
      puts "Loaded #{@server_configs.length} MCP server configurations"
      true
    rescue => e
      puts "Failed to load MCP config: #{e.message}"
      false
    end
  end

  def discover_servers
    puts "Discovering MCP servers..."
    
    discovered_count = 0
    
    @server_configs.each do |config|
      server_config = normalize_config(config)
      server_name = server_config[:name]
      
      begin
        client = MCPClient.new(server_config)
        @servers[server_name] = {
          config: server_config,
          client: client,
          status: :discovered,
          discovered_at: Time.now
        }
        
        discovered_count += 1
        puts "  ✓ Discovered: #{server_name}"
        
      rescue => e
        puts "  ✗ Failed to discover #{server_name}: #{e.message}"
      end
    end
    
    puts "Discovered #{discovered_count}/#{@server_configs.length} servers"
    discovered_count
  end

  def connect_to_server(server_name)
    server = @servers[server_name]
    unless server
      puts "Unknown MCP server: #{server_name}"
      return false
    end
    
    if @connected_servers[server_name]
      puts "MCP server #{server_name} already connected"
      return true
    end
    
    begin
      puts "Connecting to MCP server: #{server_name}..."
      
      success = server[:client].connect
      
      if success
        @connected_servers[server_name] = server
        server[:status] = :connected
        server[:connected_at] = Time.now
        
        tools_count = server[:client].list_tools.length
        resources_count = server[:client].list_resources.length
        
        puts "  ✓ Connected to #{server_name} (#{tools_count} tools, #{resources_count} resources)"
        true
      else
        server[:status] = :failed
        server[:last_error] = "Connection failed"
        puts "  ✗ Failed to connect to #{server_name}"
        false
      end
      
    rescue => e
      server[:status] = :error
      server[:last_error] = e.message
      puts "  ✗ Connection error for #{server_name}: #{e.message}"
      false
    end
  end

  def connect_all_servers
    puts "Connecting to all discovered MCP servers..."
    
    connected_count = 0
    
    @servers.each do |server_name, _|
      if connect_to_server(server_name)
        connected_count += 1
      end
    end
    
    puts "Connected to #{connected_count}/#{@servers.length} servers"
    connected_count
  end

  def disconnect_server(server_name)
    server = @connected_servers[server_name]
    unless server
      puts "MCP server #{server_name} not connected"
      return false
    end
    
    begin
      server[:client].disconnect
      @connected_servers.delete(server_name)
      server[:status] = :disconnected
      
      puts "Disconnected from MCP server: #{server_name}"
      true
      
    rescue => e
      puts "Error disconnecting from #{server_name}: #{e.message}"
      false
    end
  end

  def disconnect_all_servers
    puts "Disconnecting all MCP servers..."
    
    @connected_servers.keys.each do |server_name|
      disconnect_server(server_name)
    end
  end

  def get_available_tools
    tools = {}
    
    @connected_servers.each do |server_name, server|
      server_tools = server[:client].list_tools
      
      server_tools.each do |tool_name|
        tool_info = server[:client].get_tool_info(tool_name)
        qualified_name = "#{server_name}__#{tool_name}"
        
        tools[qualified_name] = {
          server: server_name,
          name: tool_name,
          info: tool_info,
          qualified_name: qualified_name
        }
      end
    end
    
    tools
  end

  def call_mcp_tool(qualified_tool_name, arguments = {})
    server_name, tool_name = parse_qualified_tool_name(qualified_tool_name)
    
    server = @connected_servers[server_name]
    unless server
      raise "MCP server '#{server_name}' not connected"
    end
    
    # Basic security: validate arguments are reasonable
    validate_tool_arguments(arguments)
    
    puts "Calling MCP tool: #{server_name}::#{tool_name}"
    
    result = server[:client].call_tool(tool_name, arguments)
    
    if result[:success]
      puts "  ✓ Tool call successful"
    else
      puts "  ✗ Tool call failed: #{result[:error][:message] rescue 'Unknown error'}"
    end
    
    result
  end

  def get_server_status
    status = {
      discovered: @servers.length,
      connected: @connected_servers.length,
      servers: {}
    }
    
    @servers.each do |server_name, server|
      status[:servers][server_name] = {
        status: server[:status],
        transport: server[:config][:transport],
        discovered_at: server[:discovered_at],
        connected_at: server[:connected_at],
        last_error: server[:last_error],
        tools_count: server[:status] == :connected ? server[:client].list_tools.length : 0,
        resources_count: server[:status] == :connected ? server[:client].list_resources.length : 0
      }
    end
    
    status
  end

  def health_check
    puts "Performing MCP health check..."
    
    healthy_servers = 0
    issues = []
    
    @connected_servers.each do |server_name, server|
      begin
        # Simple connectivity check by listing tools
        server[:client].list_tools
        healthy_servers += 1
        puts "  ✓ #{server_name}: healthy"
        
      rescue => e
        issues << {
          server: server_name,
          error: e.message
        }
        puts "  ✗ #{server_name}: #{e.message}"
      end
    end
    
    health_status = {
      timestamp: Time.now.iso8601,
      total_servers: @connected_servers.length,
      healthy_servers: healthy_servers,
      unhealthy_servers: @connected_servers.length - healthy_servers,
      health_rate: @connected_servers.empty? ? 100 : (healthy_servers.to_f / @connected_servers.length * 100).round(1),
      issues: issues
    }
    
    puts "Health check complete: #{healthy_servers}/#{@connected_servers.length} servers healthy"
    health_status
  end

  def reload_configuration
    puts "Reloading MCP configuration..."
    
    # Disconnect existing servers
    disconnect_all_servers
    
    # Clear current state
    @servers.clear
    @connected_servers.clear
    
    # Reload and reconnect
    if load_server_configs
      discover_servers
      connect_all_servers
      true
    else
      false
    end
  end

  private

  def find_default_config_path
    config_paths = [
      'mcp_servers.yml',
      'config/mcp_servers.yml',
      File.expand_path('~/.lantae/mcp_servers.yml'),
      '/etc/lantae/mcp_servers.yml'
    ]
    
    config_paths.find { |path| File.exist?(path) }
  end

  def normalize_config(config)
    # Convert string keys to symbols for consistency
    normalized = {}
    
    config.each do |key, value|
      symbol_key = key.to_sym
      normalized[symbol_key] = value
    end
    
    # Set defaults
    normalized[:transport] ||= 'stdio'
    normalized[:timeout] ||= 30
    
    # Validate required fields
    unless normalized[:name]
      raise "MCP server config missing required 'name' field"
    end
    
    case normalized[:transport]
    when 'stdio'
      unless normalized[:command]
        raise "MCP stdio server config missing required 'command' field"
      end
    when 'http'
      unless normalized[:url]
        raise "MCP HTTP server config missing required 'url' field"
      end
    else
      raise "Unsupported MCP transport: #{normalized[:transport]}"
    end
    
    normalized
  end

  def validate_tool_arguments(arguments)
    # Basic security validations
    arguments.each do |key, value|
      next unless value.is_a?(String)
      
      # Check for obvious path traversal attempts
      if value.include?('../') || value.include?('..\\')
        raise "Path traversal detected in argument '#{key}': #{value}"
      end
      
      # Check for suspicious commands (basic check)
      suspicious_patterns = [
        /rm\s+-rf\s+\//,
        /sudo\s+/,
        /chmod\s+777/,
        />\s*\/dev\/null.*&/,
        /curl.*\|\s*sh/,
        /wget.*\|\s*sh/
      ]
      
      suspicious_patterns.each do |pattern|
        if value.match?(pattern)
          raise "Potentially dangerous command detected in argument '#{key}': #{value}"
        end
      end
    end
  end

  def parse_qualified_tool_name(qualified_name)
    parts = qualified_name.split('__', 2)
    
    if parts.length != 2
      raise "Invalid qualified tool name: #{qualified_name}. Expected format: server__tool"
    end
    
    parts
  end
end