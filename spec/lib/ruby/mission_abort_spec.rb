require 'spec_helper'
require_relative '../../../lib/ruby/mission_abort'
require_relative '../../../lib/ruby/providers/base_provider'
require_relative '../../../lib/ruby/providers/abort_aware_provider'
require_relative '../../../lib/ruby/providers/provider_chain'

RSpec.describe Lantae::MissionAbort do
  describe Lantae::MissionAbort::Strategy do
    let(:strategy) { described_class.new }
    let(:mock_provider) { double('provider', name: 'test_provider', default_model: 'test_model') }

    describe '#should_abort?' do
      it 'detects insufficient context' do
        response = "I need more information to answer this question"
        expect(strategy.should_abort?(response)).to be true
      end

      it 'detects capability limits' do
        response = "I'm unable to complete this complex task"
        expect(strategy.should_abort?(response)).to be true
      end

      it 'detects hallucination markers' do
        response = "This might be fictional information [HALLUCINATION]"
        expect(strategy.should_abort?(response)).to be true
      end

      it 'does not abort on normal responses' do
        response = "Here is the answer to your question: 42"
        expect(strategy.should_abort?(response)).to be false
      end

      it 'checks confidence threshold for hash responses' do
        response = { content: "Answer", confidence: 0.5 }
        expect(strategy.should_abort?(response)).to be true

        response = { content: "Answer", confidence: 0.8 }
        expect(strategy.should_abort?(response)).to be false
      end
    end

    describe '#execute_with_abort_handling' do
      it 'returns response when no abort condition is met' do
        allow(mock_provider).to receive(:chat).and_return("Normal response")
        
        result = strategy.execute_with_abort_handling(mock_provider, "test request") do |provider, request|
          provider.chat('model', request)
        end
        
        expect(result).to eq("Normal response")
      end

      it 'handles abort conditions' do
        allow(mock_provider).to receive(:chat).and_return("I cannot understand this request")
        
        expect {
          strategy.execute_with_abort_handling(mock_provider, "test request") do |provider, request|
            provider.chat('model', request)
          end
        }.to raise_error(Lantae::MissionAbort::AbortError)
      end

      it 'handles exceptions as abort conditions' do
        allow(mock_provider).to receive(:chat).and_raise(StandardError.new("API Error"))
        
        expect {
          strategy.execute_with_abort_handling(mock_provider, "test request") do |provider, request|
            provider.chat('model', request)
          end
        }.to raise_error(Lantae::MissionAbort::AbortError)
      end
    end

    describe '#add_escalation_provider' do
      let(:escalation_provider) { double('escalation_provider', name: 'escalation') }

      it 'adds provider to escalation chain' do
        strategy.add_escalation_provider(escalation_provider, priority: 10)
        expect(strategy.escalation_chain.size).to eq(1)
        expect(strategy.escalation_chain.first[:priority]).to eq(10)
      end

      it 'sorts providers by priority' do
        provider1 = double('provider1')
        provider2 = double('provider2')
        
        strategy.add_escalation_provider(provider1, priority: 5)
        strategy.add_escalation_provider(provider2, priority: 10)
        
        expect(strategy.escalation_chain.first[:provider]).to eq(provider2)
      end
    end
  end

  describe Lantae::Providers::AbortAwareProvider do
    let(:base_provider) { double('base_provider', name: 'test', default_model: 'model') }
    let(:abort_strategy) { Lantae::MissionAbort::Strategy.new }
    let(:aware_provider) { described_class.new(base_provider, abort_strategy) }

    describe '#chat' do
      it 'delegates to base provider for normal responses' do
        allow(base_provider).to receive(:chat).and_return("Normal response")
        
        result = aware_provider.chat('model', [{ role: 'user', content: 'Hello' }])
        expect(result).to eq("Normal response")
      end

      it 'handles abort conditions gracefully' do
        allow(base_provider).to receive(:chat).and_return("I cannot understand this")
        
        result = aware_provider.chat('model', [{ role: 'user', content: 'Complex query' }])
        expect(result).to include("I apologize")
        expect(result).to include("unable to properly handle")
      end

      it 'uses escalation handler when provided' do
        allow(base_provider).to receive(:chat).and_return("Cannot perform this task")
        escalation_called = false
        
        aware_provider.chat(
          'model',
          [{ role: 'user', content: 'Task' }],
          escalation_handler: ->(error, messages) { escalation_called = true; "Escalated" }
        )
        
        expect(escalation_called).to be true
      end

      it 'tracks metrics' do
        allow(base_provider).to receive(:chat).and_return("Response")
        
        3.times { aware_provider.chat('model', [{ role: 'user', content: 'Test' }]) }
        
        metrics = aware_provider.metrics
        expect(metrics[:total_requests]).to eq(3)
        expect(metrics[:abort_rate]).to eq(0.0)
      end
    end

    describe '#stream' do
      before do
        allow(base_provider).to receive(:supports_streaming?).and_return(true)
      end

      it 'delegates streaming to base provider' do
        chunks = ["Hello", " world"]
        allow(base_provider).to receive(:stream).and_yield(chunks[0]).and_yield(chunks[1])
        
        received = []
        aware_provider.stream('model', [], {}) { |chunk| received << chunk }
        
        expect(received).to eq(chunks)
      end

      it 'detects abort conditions during streaming' do
        problem_chunk = "I cannot understand"
        allow(base_provider).to receive(:stream).and_yield(problem_chunk)
        
        received = []
        aware_provider.stream('model', [], { abort_check_interval: 1 }) { |chunk| received << chunk }
        
        expect(received.last).to include("[Stream aborted")
      end
    end
  end

  describe Lantae::Providers::ProviderChain do
    let(:chain) { described_class.new }
    let(:provider1) { double('provider1', name: 'primary', default_model: 'model1') }
    let(:provider2) { double('provider2', name: 'secondary', default_model: 'model2') }

    before do
      allow(provider1).to receive(:supports_streaming?).and_return(false)
      allow(provider1).to receive(:supports_tools?).and_return(false)
      allow(provider2).to receive(:supports_streaming?).and_return(true)
      allow(provider2).to receive(:supports_tools?).and_return(true)
    end

    describe '#add_provider' do
      it 'adds providers to chain' do
        chain.add_provider(provider1, priority: 10)
        chain.add_provider(provider2, priority: 5)
        
        expect(chain.list_providers).to eq(['primary', 'secondary'])
      end

      it 'wraps non-aware providers' do
        chain.add_provider(provider1)
        provider = chain.get_provider('primary')
        
        expect(provider).to be_a(Lantae::Providers::AbortAwareProvider)
      end
    end

    describe '#chat' do
      before do
        chain.add_provider(provider1, priority: 10)
        chain.add_provider(provider2, priority: 5)
      end

      it 'uses primary provider for successful requests' do
        allow(provider1).to receive(:chat).and_return("Success from primary")
        
        result = chain.chat([{ role: 'user', content: 'Hello' }])
        expect(result).to eq("Success from primary")
      end

      it 'escalates to secondary provider on primary failure' do
        allow(provider1).to receive(:chat).and_raise(Lantae::MissionAbort::AbortError.new("Failed"))
        allow(provider2).to receive(:chat).and_return("Success from secondary")
        
        result = chain.chat([{ role: 'user', content: 'Hello' }])
        expect(result).to eq("Success from secondary")
      end

      it 'selects provider based on requirements' do
        allow(provider2).to receive(:chat).and_return("Streaming response")
        
        result = chain.chat(
          [{ role: 'user', content: 'Stream this' }],
          require_streaming: true
        )
        
        expect(provider2).to have_received(:chat)
      end

      it 'handles chain exhaustion' do
        allow(provider1).to receive(:chat).and_raise(StandardError.new("Error 1"))
        allow(provider2).to receive(:chat).and_raise(StandardError.new("Error 2"))
        
        expect {
          chain.chat([{ role: 'user', content: 'Test' }])
        }.to raise_error(Lantae::MissionAbort::AbortError, /All available providers failed/)
      end
    end

    describe '#metrics' do
      it 'tracks chain metrics' do
        allow(provider1).to receive(:chat).and_return("Response")
        allow(provider1).to receive(:metrics).and_return({})
        allow(provider2).to receive(:metrics).and_return({})
        
        chain.add_provider(provider1)
        chain.chat([{ role: 'user', content: 'Test' }])
        
        metrics = chain.metrics
        expect(metrics[:chain_metrics][:total_requests]).to eq(1)
        expect(metrics[:chain_metrics][:provider_switches]).to eq(0)
      end
    end
  end
end