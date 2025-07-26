require 'spec_helper'

RSpec.describe ExecutionEngine do
  let(:provider_manager) { double('ProviderManager') }
  let(:analyzer) { double('TaskAnalyzer') }
  let(:auto_fixer) { double('AutoFixer') }
  let(:database) { double('TaskDatabase') }
  let(:engine) do
    described_class.new(
      provider_manager: provider_manager,
      analyzer: analyzer,
      auto_fixer: auto_fixer,
      database: database
    )
  end

  let(:simple_task) do
    Task.new(
      description: "Print hello world",
      context: {},
      depth: 0,
      parent: nil
    )
  end

  before do
    allow(analyzer).to receive(:analyze_code).and_return(
      TaskAnalyzer::CodeAnalysis.new(
        language: 'ruby',
        patterns: {},
        issues: [],
        metrics: { lines: 1 }
      )
    )
    allow(auto_fixer).to receive(:fix_code).and_return(nil)
    allow(database).to receive(:record_execution)
    allow(database).to receive(:find_similar_tasks).and_return([])
  end

  describe '#execute_task' do
    context 'with successful execution' do
      before do
        allow(provider_manager).to receive(:chat).and_return(
          mock_provider_response('puts "Hello World"')
        )
      end

      it 'returns successful result' do
        result = engine.execute_task(simple_task)
        
        expect(result).to be_a(ExecutionEngine::ExecutionResult)
        expect(result).to be_success
        expect(result.output).to eq('puts "Hello World"')
        expect(result.task).to eq(simple_task)
      end

      it 'records execution in database' do
        expect(database).to receive(:record_execution).with(
          hash_including(
            task_description: "Print hello world",
            success: true
          )
        )

        engine.execute_task(simple_task)
      end

      it 'updates task status' do
        engine.execute_task(simple_task)
        expect(simple_task.status).to eq(:completed)
      end
    end

    context 'with code that needs fixing' do
      let(:broken_code) { 'puts "Hello World' }
      let(:fixed_code) { 'puts "Hello World"' }

      before do
        allow(provider_manager).to receive(:chat).and_return(
          mock_provider_response(broken_code)
        )
        allow(analyzer).to receive(:analyze_code).and_return(
          TaskAnalyzer::CodeAnalysis.new(
            language: 'ruby',
            patterns: {},
            issues: [{ type: :unclosed_string, line: 1 }],
            metrics: { lines: 1 }
          )
        )
        allow(auto_fixer).to receive(:fix_code).and_return(
          AutoFixer::FixResult.new(
            fixed_code: fixed_code,
            fixes_applied: [{ type: :unclosed_string, description: "Closed string" }]
          )
        )
      end

      it 'applies auto-fixes' do
        result = engine.execute_task(simple_task)
        
        expect(result).to be_success
        expect(result.output).to eq(fixed_code)
        expect(result.fixes_applied).not_to be_empty
      end

      it 'tracks fix effectiveness' do
        expect(auto_fixer).to receive(:track_effectiveness).with(
          :unclosed_string,
          'ruby',
          true
        )

        engine.execute_task(simple_task)
      end
    end

    context 'with verification enabled' do
      before do
        engine.instance_variable_set(:@verify_execution, true)
        allow(provider_manager).to receive(:chat)
          .and_return(mock_provider_response('def add(a, b); a + b; end'))
      end

      it 'verifies generated code' do
        # Mock verification response
        allow(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("verify")))))
          .and_return(mock_provider_response('{"valid": true, "issues": []}'))

        result = engine.execute_task(simple_task)
        
        expect(result).to be_success
        expect(result.verification_passed).to be true
      end

      it 'handles verification failures' do
        allow(provider_manager).to receive(:chat)
          .with(hash_including(messages: include(hash_including(content: include("verify")))))
          .and_return(mock_provider_response('{"valid": false, "issues": ["Syntax error"]}'))

        result = engine.execute_task(simple_task)
        
        expect(result).not_to be_success
        expect(result.error).to include("Verification failed")
      end
    end

    context 'with retry logic' do
      before do
        engine.instance_variable_set(:@max_retries, 2)
      end

      it 'retries on failure' do
        call_count = 0
        allow(provider_manager).to receive(:chat) do
          call_count += 1
          if call_count == 1
            raise StandardError, "Network error"
          else
            mock_provider_response('puts "Success"')
          end
        end

        result = engine.execute_task(simple_task)
        
        expect(result).to be_success
        expect(call_count).to eq(2)
      end

      it 'gives up after max retries' do
        allow(provider_manager).to receive(:chat)
          .and_raise(StandardError, "Persistent error")

        result = engine.execute_task(simple_task)
        
        expect(result).not_to be_success
        expect(result.error).to include("Persistent error")
        expect(result.retry_count).to eq(2)
      end
    end

    context 'with checkpoint and rollback' do
      before do
        engine.instance_variable_set(:@rollback_enabled, true)
      end

      it 'creates checkpoint before execution' do
        expect(engine).to receive(:create_checkpoint).and_return({
          timestamp: Time.now,
          environment: {}
        })

        allow(provider_manager).to receive(:chat)
          .and_return(mock_provider_response('puts "Hello"'))

        engine.execute_task(simple_task)
      end

      it 'rolls back on failure' do
        checkpoint = { timestamp: Time.now, environment: {} }
        allow(engine).to receive(:create_checkpoint).and_return(checkpoint)
        allow(provider_manager).to receive(:chat).and_raise(StandardError)

        expect(engine).to receive(:rollback_to_checkpoint).with(checkpoint)

        engine.execute_task(simple_task)
      end
    end

    context 'with similar task optimization' do
      let(:similar_execution) do
        {
          'task_description' => 'Print hello world to console',
          'output' => 'puts "Hello, World!"',
          'success' => true,
          'complexity_score' => 1.5
        }
      end

      before do
        allow(database).to receive(:find_similar_tasks)
          .with("Print hello world", threshold: 0.8)
          .and_return([similar_execution])
      end

      it 'uses similar task output as context' do
        expect(provider_manager).to receive(:chat).with(
          hash_including(
            messages: include(
              hash_including(content: include("similar task"))
            )
          )
        ).and_return(mock_provider_response('puts "Hello World"'))

        engine.execute_task(simple_task)
      end
    end
  end

  describe '#verify_output' do
    it 'validates correct code' do
      code = 'def add(a, b); a + b; end'
      allow(provider_manager).to receive(:chat)
        .and_return(mock_provider_response('{"valid": true, "issues": []}'))

      result = engine.send(:verify_output, code, 'ruby')
      
      expect(result[:valid]).to be true
      expect(result[:issues]).to be_empty
    end

    it 'detects issues in code' do
      code = 'def add(a, b) a + b'  # Missing semicolon/newline
      allow(provider_manager).to receive(:chat)
        .and_return(mock_provider_response(
          '{"valid": false, "issues": ["Missing end keyword", "Syntax error"]}'
        ))

      result = engine.send(:verify_output, code, 'ruby')
      
      expect(result[:valid]).to be false
      expect(result[:issues]).to include("Missing end keyword")
    end
  end

  describe '#create_checkpoint' do
    it 'captures current environment state' do
      with_temp_file("original content") do |path|
        checkpoint = engine.send(:create_checkpoint)
        
        expect(checkpoint[:timestamp]).to be_a(Time)
        expect(checkpoint[:environment]).to include(
          :working_directory,
          :env_vars,
          :files
        )
      end
    end
  end

  describe '#rollback_to_checkpoint' do
    it 'restores environment state' do
      original_dir = Dir.pwd
      checkpoint = {
        timestamp: Time.now,
        environment: {
          working_directory: original_dir,
          env_vars: {},
          files: {}
        }
      }

      # Change directory
      Dir.chdir('/tmp')
      
      engine.send(:rollback_to_checkpoint, checkpoint)
      
      expect(Dir.pwd).to eq(original_dir)
    end
  end

  describe '#generate_solution' do
    it 'includes task context in prompt' do
      task_with_context = Task.new(
        description: "Add method to class",
        context: { class_name: 'User', method_name: 'full_name' },
        depth: 0,
        parent: nil
      )

      expect(provider_manager).to receive(:chat).with(
        hash_including(
          messages: include(
            hash_including(content: include("User", "full_name"))
          )
        )
      ).and_return(mock_provider_response('def full_name; "#{first_name} #{last_name}"; end'))

      engine.send(:generate_solution, task_with_context)
    end
  end

  describe 'edge cases' do
    it 'handles nil task gracefully' do
      expect { engine.execute_task(nil) }.not_to raise_error
    end

    it 'handles provider timeout' do
      allow(provider_manager).to receive(:chat)
        .and_raise(Net::ReadTimeout)

      result = engine.execute_task(simple_task)
      
      expect(result).not_to be_success
      expect(result.error).to include("timeout")
    end

    it 'handles malformed provider response' do
      allow(provider_manager).to receive(:chat)
        .and_return(double('response', body: 'not json'))

      result = engine.execute_task(simple_task)
      
      expect(result).not_to be_success
    end
  end
end