require 'spec_helper'

RSpec.describe AutoFixer do
  let(:fixer) { described_class.new }

  describe '#fix_code' do
    context 'with Ruby code' do
      it 'fixes unclosed strings' do
        broken = 'puts "Hello World'
        result = fixer.fix_code(broken, 'ruby')
        
        expect(result).to be_a(AutoFixer::FixResult)
        expect(result.fixed_code).to eq('puts "Hello World"')
        expect(result.fixes_applied).to include(
          hash_including(type: :unclosed_string)
        )
      end

      it 'fixes missing end keywords' do
        broken = <<~RUBY
          def hello
            puts "Hello"
          
          def world
            puts "World"
          end
        RUBY

        result = fixer.fix_code(broken, 'ruby')
        
        expect(result.fixed_code).to include("def hello")
        expect(result.fixed_code).to include("end\n\ndef world")
        expect(result.fixes_applied).to include(
          hash_including(type: :missing_end)
        )
      end

      it 'removes EOF markers' do
        broken = <<~RUBY
          puts "Hello"
          <<EOF
        RUBY

        result = fixer.fix_code(broken, 'ruby')
        
        expect(result.fixed_code).not_to include("<<EOF")
        expect(result.fixes_applied).to include(
          hash_including(type: :eof_marker)
        )
      end

      it 'fixes multiple issues in order' do
        broken = <<~RUBY
          def process
            data = "unclosed
            puts data
          <<EOF
        RUBY

        result = fixer.fix_code(broken, 'ruby')
        
        expect(result.fixed_code).to include('"unclosed"')
        expect(result.fixed_code).to include('end')
        expect(result.fixed_code).not_to include('<<EOF')
        expect(result.fixes_applied.length).to eq(3)
      end
    end

    context 'with JavaScript code' do
      it 'fixes unclosed strings' do
        broken = "const msg = 'Hello World"
        result = fixer.fix_code(broken, 'javascript')
        
        expect(result.fixed_code).to eq("const msg = 'Hello World'")
      end

      it 'adds missing semicolons' do
        broken = <<~JS
          const a = 1
          const b = 2
          console.log(a + b)
        JS

        result = fixer.fix_code(broken, 'javascript')
        
        expect(result.fixed_code).to include("const a = 1;")
        expect(result.fixed_code).to include("const b = 2;")
        expect(result.fixed_code).to include("console.log(a + b);")
      end

      it 'fixes unclosed brackets' do
        broken = <<~JS
          function test() {
            if (true) {
              console.log("test");
            }
        JS

        result = fixer.fix_code(broken, 'javascript')
        
        expect(result.fixed_code.count('{')).to eq(result.fixed_code.count('}'))
      end

      it 'handles template literals' do
        broken = 'const msg = `Hello ${name'
        result = fixer.fix_code(broken, 'javascript')
        
        expect(result.fixed_code).to match(/`Hello \$\{name[}`]/)
      end
    end

    context 'with Python code' do
      it 'fixes missing colons' do
        broken = <<~PYTHON
          def hello()
              print("Hello")
          
          if True
              print("True")
        PYTHON

        result = fixer.fix_code(broken, 'python')
        
        expect(result.fixed_code).to include("def hello():")
        expect(result.fixed_code).to include("if True:")
      end

      it 'fixes indentation issues' do
        broken = <<~PYTHON
          def process():
              for i in range(10):
                print(i)
                  if i > 5:
                break
        PYTHON

        result = fixer.fix_code(broken, 'python')
        
        lines = result.fixed_code.split("\n")
        expect(lines[2]).to match(/^    print/)  # 4 spaces
        expect(lines[3]).to match(/^    if/)     # 4 spaces
        expect(lines[4]).to match(/^        break/)  # 8 spaces
      end

      it 'fixes unclosed strings with proper quotes' do
        broken = 'message = "Hello World'
        result = fixer.fix_code(broken, 'python')
        
        expect(result.fixed_code).to eq('message = "Hello World"')
      end

      it 'handles triple quotes' do
        broken = '"""This is a docstring'
        result = fixer.fix_code(broken, 'python')
        
        expect(result.fixed_code).to eq('"""This is a docstring"""')
      end
    end

    context 'with unsupported language' do
      it 'returns original code' do
        code = "some code in unknown language"
        result = fixer.fix_code(code, 'unknown')
        
        expect(result.fixed_code).to eq(code)
        expect(result.fixes_applied).to be_empty
      end
    end
  end

  describe '#track_effectiveness' do
    it 'updates success count for fix type' do
      fixer.track_effectiveness(:unclosed_string, 'ruby', true)
      fixer.track_effectiveness(:unclosed_string, 'ruby', true)
      fixer.track_effectiveness(:unclosed_string, 'ruby', false)

      stats = fixer.effectiveness_stats
      
      expect(stats[:unclosed_string][:ruby][:success_count]).to eq(2)
      expect(stats[:unclosed_string][:ruby][:total_count]).to eq(3)
      expect(stats[:unclosed_string][:ruby][:success_rate]).to be_within(0.01).of(0.667)
    end

    it 'tracks per-language statistics' do
      fixer.track_effectiveness(:missing_semicolon, 'javascript', true)
      fixer.track_effectiveness(:missing_semicolon, 'typescript', true)

      stats = fixer.effectiveness_stats
      
      expect(stats[:missing_semicolon]).to have_key(:javascript)
      expect(stats[:missing_semicolon]).to have_key(:typescript)
    end
  end

  describe '#effectiveness_stats' do
    before do
      # Generate some test data
      5.times { fixer.track_effectiveness(:unclosed_string, 'ruby', true) }
      2.times { fixer.track_effectiveness(:unclosed_string, 'ruby', false) }
      3.times { fixer.track_effectiveness(:missing_end, 'ruby', true) }
      1.times { fixer.track_effectiveness(:missing_end, 'ruby', false) }
    end

    it 'returns comprehensive statistics' do
      stats = fixer.effectiveness_stats
      
      expect(stats[:unclosed_string][:ruby][:success_rate]).to be_within(0.01).of(0.714)
      expect(stats[:missing_end][:ruby][:success_rate]).to eq(0.75)
    end

    it 'includes all tracked fix types' do
      stats = fixer.effectiveness_stats
      
      expect(stats.keys).to include(:unclosed_string, :missing_end)
    end
  end

  describe 'fix detection' do
    describe '#fix_unclosed_strings' do
      it 'handles nested quotes' do
        broken = 'puts "He said \'Hello"'
        result = fixer.send(:fix_unclosed_strings, broken, 'ruby')
        
        expect(result).to include("He said 'Hello'")
      end

      it 'preserves escaped quotes' do
        broken = 'puts "Hello \\"World'
        result = fixer.send(:fix_unclosed_strings, broken, 'ruby')
        
        expect(result).to include('Hello \\"World"')
      end
    end

    describe '#fix_missing_ends' do
      it 'counts nested structures correctly' do
        code = <<~RUBY
          class User
            def initialize
              if valid?
                setup
            end
          end
        RUBY

        result = fixer.send(:fix_missing_ends, code, 'ruby')
        
        expect(result.scan(/\bend\b/).count).to eq(3)
      end
    end

    describe '#fix_eof_markers' do
      it 'removes various EOF patterns' do
        broken = <<~TEXT
          Some content
          <<EOF
          <<<EOF
          << EOF
          <<-EOF
        TEXT

        result = fixer.send(:fix_eof_markers, broken, 'any')
        
        expect(result).not_to match(/<<.*EOF/)
        expect(result).to include("Some content")
      end
    end

    describe '#fix_missing_colons' do
      it 'adds colons to Python control structures' do
        broken = <<~PYTHON
          if x > 0
              while True
                  for item in items
                      pass
        PYTHON

        result = fixer.send(:fix_missing_colons, broken, 'python')
        
        expect(result).to include("if x > 0:")
        expect(result).to include("while True:")
        expect(result).to include("for item in items:")
      end

      it 'preserves existing colons' do
        code = "if x > 0: print(x)"
        result = fixer.send(:fix_missing_colons, code, 'python')
        
        expect(result).to eq(code)
        expect(result.scan(':').count).to eq(1)
      end
    end
  end

  describe 'edge cases' do
    it 'handles empty code' do
      result = fixer.fix_code('', 'ruby')
      
      expect(result.fixed_code).to eq('')
      expect(result.fixes_applied).to be_empty
    end

    it 'handles nil input' do
      result = fixer.fix_code(nil, 'ruby')
      
      expect(result.fixed_code).to eq('')
      expect(result.fixes_applied).to be_empty
    end

    it 'handles very long code' do
      long_code = "puts 'test'\n" * 1000
      result = fixer.fix_code(long_code, 'ruby')
      
      expect(result.fixed_code.lines.count).to eq(1000)
    end

    it 'preserves unicode characters' do
      code = 'puts "Hello 世界"'
      result = fixer.fix_code(code, 'ruby')
      
      expect(result.fixed_code).to include('世界')
    end

    it 'handles code with mixed line endings' do
      code = "line1\r\nline2\nline3\r"
      result = fixer.fix_code(code, 'ruby')
      
      expect(result.fixed_code).to include('line1', 'line2', 'line3')
    end
  end
end