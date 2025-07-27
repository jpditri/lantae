require 'json'
require 'fileutils'
require 'time'

module Lantae
  class CostTracker
    DEFAULT_DIR = File.expand_path('~/.lantae/usage')
    
    # Token pricing per 1M tokens (as of 2024)
    PROVIDER_PRICING = {
      'openai' => {
        'gpt-4o' => { input: 2.50, output: 10.00 },
        'gpt-4o-mini' => { input: 0.15, output: 0.60 },
        'o1-preview' => { input: 15.00, output: 60.00 },
        'o1-mini' => { input: 3.00, output: 12.00 }
      },
      'anthropic' => {
        'claude-3-5-sonnet-20241022' => { input: 3.00, output: 15.00 },
        'claude-3-5-haiku-20241022' => { input: 0.80, output: 4.00 },
        'claude-3-opus-20240229' => { input: 15.00, output: 75.00 },
        'claude-3-sonnet-20240229' => { input: 3.00, output: 15.00 },
        'claude-3-haiku-20240307' => { input: 0.25, output: 1.25 }
      },
      'gemini' => {
        'gemini-1.5-pro' => { input: 1.25, output: 5.00 },
        'gemini-1.5-flash' => { input: 0.075, output: 0.30 },
        'gemini-1.0-pro' => { input: 0.50, output: 1.50 }
      },
      'mistral' => {
        'mistral-large-latest' => { input: 2.00, output: 6.00 },
        'mistral-medium-latest' => { input: 1.00, output: 3.00 },
        'mistral-small-latest' => { input: 0.20, output: 0.60 }
      },
      'perplexity' => {
        'llama-3.1-sonar-large-128k-online' => { input: 1.00, output: 1.00 },
        'llama-3.1-sonar-small-128k-online' => { input: 0.20, output: 0.20 }
      },
      'ollama' => {
        'default' => { input: 0.00, output: 0.00 } # Free for local models
      }
    }
    
    def initialize(base_dir = DEFAULT_DIR)
      @base_dir = base_dir
      FileUtils.mkdir_p(@base_dir)
      @current_session = {
        start_time: Time.now,
        usage: {}
      }
      @budget_limits = load_budget_limits
    end
    
    def track_usage(provider, model, input_tokens, output_tokens, metadata = {})
      # Initialize provider usage if needed
      @current_session[:usage][provider] ||= {}
      @current_session[:usage][provider][model] ||= {
        requests: 0,
        input_tokens: 0,
        output_tokens: 0,
        cost: 0.0
      }
      
      # Update usage
      usage = @current_session[:usage][provider][model]
      usage[:requests] += 1
      usage[:input_tokens] += input_tokens
      usage[:output_tokens] += output_tokens
      
      # Calculate cost
      cost = calculate_cost(provider, model, input_tokens, output_tokens)
      usage[:cost] += cost
      
      # Check budget limits
      check_budget_alerts(provider, usage[:cost])
      
      # Save to daily log
      save_usage_log(provider, model, input_tokens, output_tokens, cost, metadata)
      
      {
        cost: cost,
        total_session_cost: calculate_session_cost,
        budget_remaining: get_budget_remaining(provider)
      }
    end
    
    def calculate_cost(provider, model, input_tokens, output_tokens)
      pricing = get_pricing(provider, model)
      return 0.0 unless pricing
      
      input_cost = (input_tokens / 1_000_000.0) * pricing[:input]
      output_cost = (output_tokens / 1_000_000.0) * pricing[:output]
      
      input_cost + output_cost
    end
    
    def get_session_summary
      summary = {
        duration: Time.now - @current_session[:start_time],
        total_cost: calculate_session_cost,
        providers: {}
      }
      
      @current_session[:usage].each do |provider, models|
        summary[:providers][provider] = {
          total_cost: 0.0,
          total_requests: 0,
          models: {}
        }
        
        models.each do |model, usage|
          summary[:providers][provider][:total_cost] += usage[:cost]
          summary[:providers][provider][:total_requests] += usage[:requests]
          summary[:providers][provider][:models][model] = usage
        end
      end
      
      summary
    end
    
    def get_usage_report(period = :daily)
      case period
      when :daily
        get_daily_report
      when :weekly
        get_weekly_report
      when :monthly
        get_monthly_report
      else
        raise "Unknown period: #{period}"
      end
    end
    
    def set_budget_limit(provider, limit, period = :daily)
      @budget_limits[provider] ||= {}
      @budget_limits[provider][period] = limit
      save_budget_limits
    end
    
    def get_budget_status(provider)
      return nil unless @budget_limits[provider]
      
      spent_today = get_provider_cost_today(provider)
      daily_limit = @budget_limits[provider][:daily]
      
      if daily_limit
        {
          limit: daily_limit,
          spent: spent_today,
          remaining: daily_limit - spent_today,
          percentage: (spent_today / daily_limit * 100).round(2)
        }
      else
        nil
      end
    end
    
    def export_usage_data(format = :csv)
      case format
      when :csv
        export_to_csv
      when :json
        export_to_json
      else
        raise "Unsupported export format: #{format}"
      end
    end
    
    private
    
    def get_pricing(provider, model)
      provider_pricing = PROVIDER_PRICING[provider]
      return nil unless provider_pricing
      
      # Try exact model match first
      return provider_pricing[model] if provider_pricing[model]
      
      # Try to find a matching model by prefix
      provider_pricing.each do |model_name, pricing|
        return pricing if model.start_with?(model_name)
      end
      
      # Use default if available
      provider_pricing['default']
    end
    
    def calculate_session_cost
      total = 0.0
      @current_session[:usage].each do |provider, models|
        models.each do |model, usage|
          total += usage[:cost]
        end
      end
      total
    end
    
    def save_usage_log(provider, model, input_tokens, output_tokens, cost, metadata)
      today = Date.today.to_s
      log_file = File.join(@base_dir, "usage_#{today}.json")
      
      # Load existing log
      log = if File.exist?(log_file)
        JSON.parse(File.read(log_file), symbolize_names: true)
      else
        { date: today, entries: [] }
      end
      
      # Add new entry
      log[:entries] << {
        timestamp: Time.now.iso8601,
        provider: provider,
        model: model,
        input_tokens: input_tokens,
        output_tokens: output_tokens,
        cost: cost,
        metadata: metadata
      }
      
      # Save log
      File.write(log_file, JSON.pretty_generate(log))
    end
    
    def check_budget_alerts(provider, current_cost)
      return unless @budget_limits[provider]
      
      daily_limit = @budget_limits[provider][:daily]
      return unless daily_limit
      
      spent_today = get_provider_cost_today(provider)
      percentage = (spent_today / daily_limit * 100).round(2)
      
      if percentage >= 90 && !@current_session[:alerts_sent]&.include?("90_#{provider}")
        puts "\n⚠️  WARNING: You've used #{percentage}% of your daily #{provider} budget!"
        @current_session[:alerts_sent] ||= []
        @current_session[:alerts_sent] << "90_#{provider}"
      elsif percentage >= 75 && !@current_session[:alerts_sent]&.include?("75_#{provider}")
        puts "\n⚠️  Alert: You've used #{percentage}% of your daily #{provider} budget."
        @current_session[:alerts_sent] ||= []
        @current_session[:alerts_sent] << "75_#{provider}"
      end
    end
    
    def get_provider_cost_today(provider)
      today = Date.today.to_s
      log_file = File.join(@base_dir, "usage_#{today}.json")
      
      return 0.0 unless File.exist?(log_file)
      
      log = JSON.parse(File.read(log_file), symbolize_names: true)
      
      total = 0.0
      log[:entries].each do |entry|
        total += entry[:cost] if entry[:provider] == provider
      end
      
      total
    end
    
    def get_budget_remaining(provider)
      budget_status = get_budget_status(provider)
      budget_status ? budget_status[:remaining] : nil
    end
    
    def load_budget_limits
      budget_file = File.join(@base_dir, 'budgets.json')
      return {} unless File.exist?(budget_file)
      
      JSON.parse(File.read(budget_file), symbolize_names: true)
    end
    
    def save_budget_limits
      budget_file = File.join(@base_dir, 'budgets.json')
      File.write(budget_file, JSON.pretty_generate(@budget_limits))
    end
    
    def get_daily_report
      today = Date.today.to_s
      log_file = File.join(@base_dir, "usage_#{today}.json")
      
      return { date: today, total_cost: 0.0, providers: {} } unless File.exist?(log_file)
      
      log = JSON.parse(File.read(log_file), symbolize_names: true)
      
      report = {
        date: today,
        total_cost: 0.0,
        total_requests: log[:entries].size,
        providers: {}
      }
      
      log[:entries].each do |entry|
        provider = entry[:provider]
        report[:providers][provider] ||= {
          cost: 0.0,
          requests: 0,
          input_tokens: 0,
          output_tokens: 0
        }
        
        report[:providers][provider][:cost] += entry[:cost]
        report[:providers][provider][:requests] += 1
        report[:providers][provider][:input_tokens] += entry[:input_tokens]
        report[:providers][provider][:output_tokens] += entry[:output_tokens]
        report[:total_cost] += entry[:cost]
      end
      
      report
    end
    
    def get_weekly_report
      reports = []
      7.times do |i|
        date = (Date.today - i).to_s
        log_file = File.join(@base_dir, "usage_#{date}.json")
        next unless File.exist?(log_file)
        
        log = JSON.parse(File.read(log_file), symbolize_names: true)
        daily_cost = log[:entries].sum { |e| e[:cost] }
        reports << { date: date, cost: daily_cost }
      end
      
      {
        period: "#{(Date.today - 6).to_s} to #{Date.today.to_s}",
        total_cost: reports.sum { |r| r[:cost] },
        daily_breakdown: reports.reverse
      }
    end
    
    def get_monthly_report
      reports = []
      30.times do |i|
        date = (Date.today - i).to_s
        log_file = File.join(@base_dir, "usage_#{date}.json")
        next unless File.exist?(log_file)
        
        log = JSON.parse(File.read(log_file), symbolize_names: true)
        daily_cost = log[:entries].sum { |e| e[:cost] }
        reports << { date: date, cost: daily_cost }
      end
      
      {
        period: "#{(Date.today - 29).to_s} to #{Date.today.to_s}",
        total_cost: reports.sum { |r| r[:cost] },
        daily_average: reports.sum { |r| r[:cost] } / reports.size.to_f,
        peak_day: reports.max_by { |r| r[:cost] }
      }
    end
    
    def export_to_csv
      require 'csv'
      
      csv_data = CSV.generate do |csv|
        csv << ['Date', 'Time', 'Provider', 'Model', 'Input Tokens', 'Output Tokens', 'Cost']
        
        Dir.glob(File.join(@base_dir, 'usage_*.json')).sort.each do |log_file|
          log = JSON.parse(File.read(log_file), symbolize_names: true)
          
          log[:entries].each do |entry|
            csv << [
              log[:date],
              entry[:timestamp],
              entry[:provider],
              entry[:model],
              entry[:input_tokens],
              entry[:output_tokens],
              sprintf('$%.4f', entry[:cost])
            ]
          end
        end
      end
      
      csv_data
    end
    
    def export_to_json
      all_data = []
      
      Dir.glob(File.join(@base_dir, 'usage_*.json')).sort.each do |log_file|
        log = JSON.parse(File.read(log_file), symbolize_names: true)
        all_data << log
      end
      
      JSON.pretty_generate(all_data)
    end
  end
end