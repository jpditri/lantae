require 'fileutils'
require 'json'

module Lantae
  class WorkspaceAuthenticator
    WORKSPACE_DIR = File.expand_path('~/.lantae/workspaces')
    
    class << self
      def ensure_workspace_dir
        FileUtils.mkdir_p(WORKSPACE_DIR) unless Dir.exist?(WORKSPACE_DIR)
      end
      
      def workspace_file(workspace_name)
        File.join(WORKSPACE_DIR, "#{workspace_name}.json")
      end
      
      def list_workspaces
        ensure_workspace_dir
        Dir.glob(File.join(WORKSPACE_DIR, '*.json')).map do |file|
          File.basename(file, '.json')
        end
      end
      
      def has_key?(provider, workspace_name)
        ensure_workspace_dir
        file_path = workspace_file(workspace_name)
        return false unless File.exist?(file_path)
        
        begin
          data = JSON.parse(File.read(file_path))
          data['providers'] && data['providers'][provider] && data['providers'][provider]['api_key']
        rescue JSON::ParserError
          false
        end
      end
      
      def get_key(provider, workspace_name)
        ensure_workspace_dir
        file_path = workspace_file(workspace_name)
        return nil unless File.exist?(file_path)
        
        begin
          data = JSON.parse(File.read(file_path))
          data.dig('providers', provider, 'api_key')
        rescue JSON::ParserError
          nil
        end
      end
      
      def set_key(provider, api_key, workspace_name, metadata = {})
        ensure_workspace_dir
        file_path = workspace_file(workspace_name)
        
        # Load existing data or create new
        data = if File.exist?(file_path)
          JSON.parse(File.read(file_path)) rescue { 'providers' => {} }
        else
          { 'providers' => {} }
        end
        
        # Update provider data
        data['providers'][provider] = {
          'api_key' => api_key,
          'updated_at' => Time.now.to_s,
          'metadata' => metadata
        }
        
        # Write back
        File.write(file_path, JSON.pretty_generate(data))
        File.chmod(0600, file_path)
        
        # Also set in environment for current session
        ENV["#{provider.upcase}_API_KEY"] = api_key
        
        true
      end
      
      def available_providers(workspace_name)
        ensure_workspace_dir
        file_path = workspace_file(workspace_name)
        return [] unless File.exist?(file_path)
        
        begin
          data = JSON.parse(File.read(file_path))
          data['providers']&.keys || []
        rescue JSON::ParserError
          []
        end
      end
      
      def delete_workspace(workspace_name)
        ensure_workspace_dir
        file_path = workspace_file(workspace_name)
        FileUtils.rm_f(file_path)
      end
      
      def export_workspace(workspace_name)
        ensure_workspace_dir
        file_path = workspace_file(workspace_name)
        return nil unless File.exist?(file_path)
        
        begin
          data = JSON.parse(File.read(file_path))
          # Remove sensitive data for export
          export_data = data.dup
          export_data['providers']&.each do |provider, config|
            config['api_key'] = '*' * 8 + config['api_key'][-4..-1] if config['api_key']
          end
          export_data
        rescue JSON::ParserError
          nil
        end
      end
      
      def import_workspace(workspace_name, data)
        ensure_workspace_dir
        file_path = workspace_file(workspace_name)
        
        # Validate data structure
        unless data.is_a?(Hash) && data['providers'].is_a?(Hash)
          raise "Invalid workspace data format"
        end
        
        # Don't import masked keys
        data['providers'].each do |provider, config|
          if config['api_key']&.start_with?('****')
            raise "Cannot import masked API keys. Please provide actual keys."
          end
        end
        
        File.write(file_path, JSON.pretty_generate(data))
        File.chmod(0600, file_path)
        true
      end
      
      def load_workspace_keys(workspace_name)
        ensure_workspace_dir
        file_path = workspace_file(workspace_name)
        return unless File.exist?(file_path)
        
        begin
          data = JSON.parse(File.read(file_path))
          data['providers']&.each do |provider, config|
            if config['api_key']
              ENV["#{provider.upcase}_API_KEY"] = config['api_key']
            end
          end
        rescue JSON::ParserError
          # Ignore
        end
      end
    end
  end
end