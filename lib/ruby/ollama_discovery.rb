require 'socket'
require 'net/http'
require 'json'
require 'timeout'

module Lantae
  class OllamaDiscovery
    DEFAULT_PORT = 11434
    COMMON_PORTS = [11434, 11435, 11436].freeze
    
    def self.scan_local_network(timeout: 2, max_concurrent: 20, debug: false)
      discovered = []
      discovered_mutex = Mutex.new
      
      puts "🔍 Checking localhost..." if debug
      
      # Always check localhost first
      COMMON_PORTS.each do |port|
        if check_ollama_instance("localhost", port, timeout)
          models = get_models("http://localhost:#{port}")
          discovered << {
            host: "localhost",
            port: port,
            url: "http://localhost:#{port}",
            models: models
          }
          puts "  ✅ Found localhost:#{port} (#{models.size} models)" if debug
        end
      end
      
      # Scan local network efficiently
      begin
        # Get local IP to determine network range
        local_ip = get_local_ip
        if local_ip && (local_ip.start_with?('192.168.') || local_ip.start_with?('10.') || local_ip.start_with?('172.'))
          network_base = local_ip.split('.')[0..2].join('.')
          local_last_octet = local_ip.split('.').last.to_i
          
          # Create a list of IPs to scan (prioritize common ranges)
          ips_to_scan = []
          
          # Priority 1: Common server IPs
          [1, 2, 10, 50, 100, 200, 254].each do |i|
            next if i == local_last_octet
            ips_to_scan << "#{network_base}.#{i}"
          end
          
          # Priority 2: IPs around our own IP
          (-5..5).each do |offset|
            ip = local_last_octet + offset
            next if ip <= 0 || ip > 254 || ip == local_last_octet
            next if ips_to_scan.include?("#{network_base}.#{ip}")
            ips_to_scan << "#{network_base}.#{ip}"
          end
          
          # Priority 3: Some random IPs for broader coverage
          [20, 30, 40, 60, 70, 80, 120, 150, 180, 220].each do |i|
            next if i == local_last_octet
            next if ips_to_scan.include?("#{network_base}.#{i}")
            ips_to_scan << "#{network_base}.#{i}"
          end
          
          # Use a thread pool for scanning
          require 'thread'
          queue = Queue.new
          ips_to_scan.each { |ip| queue << ip }
          
          threads = []
          max_concurrent.times do
            threads << Thread.new do
              while !queue.empty?
                begin
                  host = queue.pop(true) # non-blocking pop
                  
                  COMMON_PORTS.each do |port|
                    if check_ollama_instance(host, port, timeout)
                      discovered_mutex.synchronize do
                        discovered << {
                          host: host,
                          port: port,
                          url: "http://#{host}:#{port}",
                          models: get_models("http://#{host}:#{port}")
                        }
                      end
                    end
                  end
                rescue ThreadError
                  # Queue is empty, exit thread
                  break
                end
              end
            end
          end
          
          # Wait for all threads with a reasonable timeout
          threads.each { |t| t.join(30) } # Max 30 seconds total
          
        end
      rescue => e
        # Network scanning failed, continue with what we have
      end
      
      # Remove duplicates and sort
      discovered.uniq { |d| d[:url] }.sort_by { |instance| [instance[:host], instance[:port]] }
    end
    
    # Quick scan - only check localhost and a few common IPs
    def self.quick_scan(timeout: 1)
      discovered = []
      
      # Check localhost
      COMMON_PORTS.each do |port|
        if check_ollama_instance("localhost", port, timeout)
          discovered << {
            host: "localhost",
            port: port,
            url: "http://localhost:#{port}",
            models: get_models("http://localhost:#{port}")
          }
        end
      end
      
      # Check a few common local IPs
      local_ip = get_local_ip
      if local_ip && local_ip.start_with?('192.168.')
        network_base = local_ip.split('.')[0..2].join('.')
        
        # Only check gateway and a couple common server IPs
        common_ips = ["#{network_base}.1", "#{network_base}.2", "#{network_base}.10"]
        
        common_ips.each do |ip|
          COMMON_PORTS.each do |port|
            if check_ollama_instance(ip, port, timeout)
              discovered << {
                host: ip,
                port: port,
                url: "http://#{ip}:#{port}",
                models: get_models("http://#{ip}:#{port}")
              }
            end
          end
        end
      end
      
      discovered.uniq { |d| d[:url] }.sort_by { |instance| [instance[:host], instance[:port]] }
    end
    
    def self.check_ollama_instance(host, port, timeout = 2)
      begin
        Timeout::timeout(timeout) do
          uri = URI("http://#{host}:#{port}/api/version")
          http = Net::HTTP.new(uri.host, uri.port)
          http.read_timeout = timeout
          http.open_timeout = timeout
          
          response = http.get(uri)
          return response.code == '200'
        end
      rescue
        false
      end
    end
    
    def self.get_models(base_url)
      begin
        uri = URI("#{base_url}/api/tags")
        http = Net::HTTP.new(uri.host, uri.port)
        http.read_timeout = 5
        http.open_timeout = 2
        
        response = http.get(uri)
        if response.code == '200'
          data = JSON.parse(response.body)
          return (data['models'] || []).map { |m| m['name'] }.sort
        end
      rescue
        # Ignore errors, return empty array
      end
      []
    end
    
    def self.get_instance_info(base_url)
      begin
        uri = URI("#{base_url}/api/version")
        http = Net::HTTP.new(uri.host, uri.port)
        http.read_timeout = 5
        http.open_timeout = 2
        
        response = http.get(uri)
        if response.code == '200'
          version_data = JSON.parse(response.body)
          models = get_models(base_url)
          
          return {
            version: version_data['version'] || 'unknown',
            models: models,
            model_count: models.size,
            status: 'online'
          }
        end
      rescue
        # Ignore errors
      end
      
      {
        version: 'unknown',
        models: [],
        model_count: 0,
        status: 'offline'
      }
    end
    
    private
    
    def self.get_local_ip
      begin
        # Try to connect to a remote address to determine local IP
        socket = UDPSocket.new
        socket.connect('8.8.8.8', 80)
        local_ip = socket.addr.last
        socket.close
        local_ip
      rescue
        nil
      end
    end
  end
end