require 'spec_helper'

RSpec.describe TaskAnalyzer do
  let(:analyzer) { described_class.new }

  describe '#assess_complexity' do
    context 'with simple tasks' do
      it 'returns low complexity for basic tasks' do
        result = analyzer.assess_complexity("Print hello world")
        
        expect(result).to be_a(TaskAnalyzer::TaskComplexity)
        expect(result.score).to be_between(1, 3)
        expect(result.factors).to include(:length, :keywords, :technical_terms)
      end

      it 'identifies simple file operations' do
        result = analyzer.assess_complexity("Read a file and print its contents")
        
        expect(result.score).to be_between(2, 4)
        expect(result.factors[:keywords]).to be > 0
      end
    end

    context 'with complex tasks' do
      it 'returns high complexity for architectural tasks' do
        result = analyzer.assess_complexity(
          "Design and implement a microservices architecture with service discovery, " \
          "load balancing, circuit breakers, and distributed tracing"
        )
        
        expect(result.score).to be_between(7, 10)
        expect(result.factors[:technical_terms]).to be > 3
      end

      it 'recognizes integration complexity' do
        result = analyzer.assess_complexity(
          "Integrate OAuth2 authentication with JWT tokens, refresh token rotation, " \
          "and multi-factor authentication support"
        )
        
        expect(result.score).to be >= 6
        expect(result.factors[:integration_points]).to be > 0
      end
    end

    context 'with code-specific tasks' do
      it 'analyzes refactoring complexity' do
        result = analyzer.assess_complexity(
          "Refactor the monolithic application into modules with dependency injection"
        )
        
        expect(result.score).to be_between(5, 8)
        expect(result.factors[:code_operations]).to be > 0
      end

      it 'evaluates algorithm complexity' do
        result = analyzer.assess_complexity(
          "Implement a distributed consensus algorithm using Raft protocol"
        )
        
        expect(result.score).to be >= 7
        expect(result.factors[:algorithm_complexity]).to be > 0
      end
    end
  end

  describe '#analyze_code' do
    context 'with Ruby code' do
      let(:ruby_code) do
        <<~RUBY
          class User
            attr_reader :name, :email
            
            def initialize(name, email)
              @name = name
              @email = email
            end
            
            def valid?
              !name.nil? && !email.nil? && email.include?('@')
            end
          end
        RUBY
      end

      it 'identifies Ruby syntax patterns' do
        result = analyzer.analyze_code(ruby_code, 'ruby')
        
        expect(result).to be_a(TaskAnalyzer::CodeAnalysis)
        expect(result.language).to eq('ruby')
        expect(result.patterns).to include(
          classes: ['User'],
          methods: ['initialize', 'valid?']
        )
      end

      it 'detects Ruby-specific issues' do
        code_with_issues = <<~RUBY
          def process_data
            data = fetch_data
            data.each do |item|
              puts item
          end
        RUBY

        result = analyzer.analyze_code(code_with_issues, 'ruby')
        
        expect(result.issues).not_to be_empty
        expect(result.issues.first[:type]).to eq(:syntax_error)
        expect(result.issues.first[:message]).to include("missing 'end'")
      end
    end

    context 'with JavaScript code' do
      let(:js_code) do
        <<~JS
          class Calculator {
            constructor() {
              this.result = 0;
            }
            
            add(a, b) {
              return a + b;
            }
            
            async fetchData() {
              const response = await fetch('/api/data');
              return response.json();
            }
          }
        JS
      end

      it 'identifies JavaScript patterns' do
        result = analyzer.analyze_code(js_code, 'javascript')
        
        expect(result.patterns).to include(
          classes: ['Calculator'],
          methods: ['constructor', 'add', 'fetchData'],
          async_functions: ['fetchData']
        )
      end

      it 'detects unclosed strings' do
        code_with_issues = <<~JS
          const message = "Hello world;
          console.log(message);
        JS

        result = analyzer.analyze_code(code_with_issues, 'javascript')
        
        expect(result.issues).to include(
          hash_including(
            type: :unclosed_string,
            line: 1
          )
        )
      end
    end

    context 'with Python code' do
      let(:python_code) do
        <<~PYTHON
          import asyncio
          from typing import List, Optional

          class DataProcessor:
              def __init__(self, name: str):
                  self.name = name
                  self._cache: Optional[List[str]] = None
              
              async def process_batch(self, items: List[str]) -> List[str]:
                  results = []
                  for item in items:
                      result = await self._process_item(item)
                      results.append(result)
                  return results
              
              async def _process_item(self, item: str) -> str:
                  await asyncio.sleep(0.1)
                  return item.upper()
        PYTHON
      end

      it 'identifies Python patterns' do
        result = analyzer.analyze_code(python_code, 'python')
        
        expect(result.patterns).to include(
          classes: ['DataProcessor'],
          functions: include('__init__', 'process_batch', '_process_item'),
          imports: include('asyncio', 'typing'),
          async_functions: include('process_batch', '_process_item')
        )
      end

      it 'detects indentation errors' do
        code_with_issues = <<~PYTHON
          def calculate(x, y):
              if x > 0:
                  result = x + y
                 return result
              return 0
        PYTHON

        result = analyzer.analyze_code(code_with_issues, 'python')
        
        expect(result.issues).to include(
          hash_including(
            type: :indentation_error,
            message: include("inconsistent indentation")
          )
        )
      end

      it 'detects missing colons' do
        code_with_issues = <<~PYTHON
          def process(data)
              for item in data:
                  print(item)
        PYTHON

        result = analyzer.analyze_code(code_with_issues, 'python')
        
        expect(result.issues).to include(
          hash_including(
            type: :syntax_error,
            message: include("missing colon")
          )
        )
      end
    end

    context 'with unsupported language' do
      it 'returns basic analysis' do
        result = analyzer.analyze_code("Some code", 'unknown')
        
        expect(result.language).to eq('unknown')
        expect(result.patterns).to be_empty
        expect(result.metrics[:lines]).to eq(1)
      end
    end
  end

  describe '#find_patterns' do
    it 'identifies security concerns' do
      code = <<~RUBY
        password = "hardcoded_password"
        system("rm -rf #{user_input}")
        eval(params[:code])
      RUBY

      patterns = analyzer.find_patterns(code, 'ruby')
      
      expect(patterns[:security_concerns]).to include(
        "Hardcoded credentials detected",
        "Potential command injection",
        "Use of eval detected"
      )
    end

    it 'identifies performance issues' do
      code = <<~RUBY
        # Nested loops
        data.each do |item|
          other_data.each do |other|
            results << process(item, other)
          end
        end
        
        # N+1 query pattern
        users.each do |user|
          puts user.posts.count
        end
      RUBY

      patterns = analyzer.find_patterns(code, 'ruby')
      
      expect(patterns[:performance_concerns]).to include(
        "Nested iteration detected",
        "Potential N+1 query pattern"
      )
    end
  end

  describe '#estimate_tokens' do
    it 'estimates token count for short text' do
      count = analyzer.estimate_tokens("Hello world")
      expect(count).to be_between(2, 4)
    end

    it 'estimates token count for code' do
      code = <<~RUBY
        def factorial(n)
          return 1 if n <= 1
          n * factorial(n - 1)
        end
      RUBY

      count = analyzer.estimate_tokens(code)
      expect(count).to be_between(20, 40)
    end

    it 'handles empty input' do
      count = analyzer.estimate_tokens("")
      expect(count).to eq(0)
    end
  end

  describe 'complexity factors' do
    describe '#analyze_length' do
      it 'scores based on description length' do
        short = analyzer.send(:analyze_length, "Do X")
        medium = analyzer.send(:analyze_length, "Do X and Y with Z considering A, B, and C")
        long = analyzer.send(:analyze_length, "A" * 500)

        expect(short).to be < medium
        expect(medium).to be < long
      end
    end

    describe '#analyze_keywords' do
      it 'identifies complexity keywords' do
        simple = analyzer.send(:analyze_keywords, "print message")
        complex = analyzer.send(:analyze_keywords, "integrate distributed microservices architecture")

        expect(simple).to be < complex
      end

      it 'recognizes operation keywords' do
        score = analyzer.send(:analyze_keywords, "implement refactor optimize integrate")
        expect(score).to be > 3
      end
    end

    describe '#analyze_technical_terms' do
      it 'counts technical terminology' do
        simple = analyzer.send(:analyze_technical_terms, "save file to disk")
        technical = analyzer.send(:analyze_technical_terms, 
          "implement OAuth2 JWT CORS CSRF middleware with Redis caching")

        expect(simple).to be < technical
      end
    end

    describe '#analyze_ambiguity' do
      it 'detects vague requirements' do
        clear = analyzer.send(:analyze_ambiguity, "Create a User class with name and email attributes")
        vague = analyzer.send(:analyze_ambiguity, "Make it better and fix the issues somehow")

        expect(clear).to be < vague
      end
    end
  end

  describe 'edge cases' do
    it 'handles nil input' do
      expect { analyzer.assess_complexity(nil) }.not_to raise_error
      expect { analyzer.analyze_code(nil, 'ruby') }.not_to raise_error
    end

    it 'handles very long input' do
      long_task = "A" * 10000
      result = analyzer.assess_complexity(long_task)
      
      expect(result.score).to be <= 10
    end

    it 'handles malformed code gracefully' do
      malformed = "```\n��invalid UTF-8\n```"
      expect { analyzer.analyze_code(malformed, 'ruby') }.not_to raise_error
    end
  end
end