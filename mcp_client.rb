#!/usr/bin/env ruby

require 'json'
require 'net/http'
require 'uri'
require 'open3'
require 'securerandom'

# Model Context Protocol (MCP) Client Implementation
class MCPClient
  attr_reader :server_info, :capabilities, :tools, :resources, :prompts
  
  def initialize(server_config)
    @server_config = server_config
    @request_id = 0
    @connected = false
    
    # MCP capabilities
    @capabilities = {}
    @tools = {}
    @resources = {}
    @prompts = {}
    
    # Transport setup
    @transport = create_transport(server_config)
  end

  def connect
    begin
      # Initialize transport connection
      @transport.connect if @transport.respond_to?(:connect)
      
      # Perform MCP handshake
      handshake_result = perform_handshake
      
      if handshake_result[:success]
        @connected = true
        @server_info = handshake_result[:server_info]
        
        # Discover capabilities
        discover_capabilities
        
        true
      else
        false
      end
      
    rescue => e
      puts "MCP connection failed: #{e.message}"
      false
    end
  end

  def disconnect
    begin
      @transport.disconnect if @transport.respond_to?(:disconnect)
      @connected = false
      true
    rescue => e
      puts "MCP disconnection error: #{e.message}"
      false
    end
  end

  def connected?
    @connected
  end

  def call_tool(tool_name, arguments = {})
    unless connected?
      raise "MCP server not connected"
    end
    
    unless @tools[tool_name]
      raise "Tool '#{tool_name}' not available on MCP server"
    end
    
    request_id = next_request_id
    
    request = {
      jsonrpc: "2.0",
      id: request_id,
      method: "tools/call",
      params: {
        name: tool_name,
        arguments: arguments
      }
    }
    
    begin
      response = send_request(request)
      
      if response[:error]
        { success: false, error: response[:error] }
      else
        { success: true, result: response[:result] }
      end
      
    rescue => e
      { success: false, error: { message: e.message } }
    end
  end

  def list_tools
    @tools.keys
  end

  def get_tool_info(tool_name)
    @tools[tool_name]
  end

  def list_resources
    @resources.keys
  end

  def get_resource(resource_uri)
    unless connected?
      raise "MCP server not connected"
    end
    
    request_id = next_request_id
    
    request = {
      jsonrpc: "2.0",
      id: request_id,
      method: "resources/read",
      params: {
        uri: resource_uri
      }
    }
    
    response = send_request(request)
    
    if response[:error]
      { success: false, error: response[:error] }
    else
      { success: true, result: response[:result] }
    end
  end

  private

  def create_transport(config)
    case config[:transport]
    when 'stdio'
      MCPStdioTransport.new(config)
    when 'http'
      MCPHttpTransport.new(config)
    else
      raise "Unsupported MCP transport: #{config[:transport]}"
    end
  end

  def perform_handshake
    request_id = next_request_id
    
    # Initialize request
    initialize_request = {
      jsonrpc: "2.0",
      id: request_id,
      method: "initialize",
      params: {
        protocolVersion: "2024-11-05",
        capabilities: {
          tools: { listChanged: true },
          resources: { subscribe: true },
          logging: {}
        },
        clientInfo: {
          name: "lantae-mcp-client",
          version: "1.0.0"
        }
      }
    }
    
    response = send_request(initialize_request)
    
    if response[:error]
      return { success: false, error: response[:error] }
    end
    
    # Send initialized notification
    initialized_notification = {
      jsonrpc: "2.0",
      method: "notifications/initialized"
    }
    
    send_notification(initialized_notification)
    
    {
      success: true,
      server_info: response[:result][:serverInfo],
      capabilities: response[:result][:capabilities]
    }
  end

  def discover_capabilities
    # List tools
    discover_tools
    
    # List resources
    discover_resources
    
    # List prompts
    discover_prompts
  end

  def discover_tools
    request_id = next_request_id
    
    request = {
      jsonrpc: "2.0",
      id: request_id,
      method: "tools/list"
    }
    
    response = send_request(request)
    
    if response[:result] && response[:result][:tools]
      response[:result][:tools].each do |tool|
        @tools[tool[:name]] = tool
      end
    end
  rescue => e
    puts "Failed to discover tools: #{e.message}"
  end

  def discover_resources
    request_id = next_request_id
    
    request = {
      jsonrpc: "2.0",
      id: request_id,
      method: "resources/list"
    }
    
    response = send_request(request)
    
    if response[:result] && response[:result][:resources]
      response[:result][:resources].each do |resource|
        @resources[resource[:uri]] = resource
      end
    end
  rescue => e
    puts "Failed to discover resources: #{e.message}"
  end

  def discover_prompts
    request_id = next_request_id
    
    request = {
      jsonrpc: "2.0",
      id: request_id,
      method: "prompts/list"
    }
    
    response = send_request(request)
    
    if response[:result] && response[:result][:prompts]
      response[:result][:prompts].each do |prompt|
        @prompts[prompt[:name]] = prompt
      end
    end
  rescue => e
    puts "Failed to discover prompts: #{e.message}"
  end

  def send_request(request)
    @transport.send_request(request)
  end

  def send_notification(notification)
    @transport.send_notification(notification)
  end

  def next_request_id
    @request_id += 1
  end
end

# Stdio Transport for MCP servers
class MCPStdioTransport
  def initialize(config)
    @config = config
    @process = nil
    @stdin = nil
    @stdout = nil
    @stderr = nil
  end

  def connect
    command = @config[:command]
    args = @config[:args] || []
    
    @stdin, @stdout, @stderr, @process = Open3.popen3(command, *args)
    
    # Set non-blocking mode for stderr to avoid hanging
    @stderr.fcntl(Fcntl::F_SETFL, Fcntl::O_NONBLOCK) if defined?(Fcntl)
    
    true
  end

  def disconnect
    if @process
      @stdin.close if @stdin && !@stdin.closed?
      @stdout.close if @stdout && !@stdout.closed?
      @stderr.close if @stderr && !@stderr.closed?
      
      begin
        @process.terminate
        @process.wait
      rescue => e
        puts "Warning: Error terminating MCP process: #{e.message}"
      end
      
      @process = nil
    end
  end

  def send_request(request)
    send_message(request)
    receive_response
  end

  def send_notification(notification)
    send_message(notification)
    nil
  end

  private

  def send_message(message)
    json_message = message.to_json
    @stdin.puts(json_message)
    @stdin.flush
  end

  def receive_response
    line = @stdout.readline.strip
    JSON.parse(line, symbolize_names: true)
  rescue JSON::ParserError => e
    puts "Invalid JSON response from MCP server: #{line}"
    { error: { code: -32700, message: "Parse error" } }
  rescue EOFError
    puts "MCP server closed connection unexpectedly"
    { error: { code: -32603, message: "Connection closed" } }
  end
end

# HTTP Transport for MCP servers
class MCPHttpTransport
  def initialize(config)
    @config = config
    @base_uri = URI(config[:url])
    @http = nil
  end

  def connect
    @http = Net::HTTP.new(@base_uri.host, @base_uri.port)
    @http.use_ssl = (@base_uri.scheme == 'https')
    @http.open_timeout = @config[:timeout] || 30
    @http.read_timeout = @config[:timeout] || 30
    
    # Test connection
    @http.start
    true
  end

  def disconnect
    @http.finish if @http&.started?
    @http = nil
  end

  def send_request(request)
    http_request = Net::HTTP::Post.new(@base_uri.request_uri)
    http_request['Content-Type'] = 'application/json'
    
    # Add authentication if configured
    if @config[:auth]
      case @config[:auth][:type]
      when 'bearer'
        http_request['Authorization'] = "Bearer #{@config[:auth][:token]}"
      when 'basic'
        http_request.basic_auth(@config[:auth][:username], @config[:auth][:password])
      end
    end
    
    http_request.body = request.to_json
    
    response = @http.request(http_request)
    
    if response.code == '200'
      JSON.parse(response.body, symbolize_names: true)
    else
      {
        error: {
          code: -32603,
          message: "HTTP error #{response.code}",
          data: { body: response.body }
        }
      }
    end
  end

  def send_notification(notification)
    # HTTP notifications are sent as POST requests without expecting responses
    send_request(notification)
    nil
  end
end