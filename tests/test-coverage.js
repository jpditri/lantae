#!/usr/bin/env node
/**
 * Comprehensive Test Runner with Code Coverage
 * Uses NYC/Istanbul for coverage analysis
 */

const { spawn, execSync } = require('child_process');
const fs = require('fs-extra');
const path = require('path');

class CoverageTestRunner {
  constructor() {
    this.testDir = path.join(__dirname, 'test-coverage-output');
    this.coverageDir = path.join(__dirname, 'coverage');
  }

  async run() {
    console.log('🔬 Running Lantae LSP Tests with Code Coverage Analysis\n');
    
    try {
      await this.setup();
      await this.runTestsWithCoverage();
      await this.generateReport();
      await this.analyzeCoverage();
    } catch (error) {
      console.error('❌ Coverage test failed:', error.message);
      process.exit(1);
    }
  }

  async setup() {
    console.log('🔧 Setting up coverage environment...');
    
    // Clean coverage directories
    await fs.remove(this.coverageDir);
    await fs.remove(this.testDir);
    await fs.ensureDir(this.testDir);
    
    // Clean NYC cache
    try {
      execSync('npx nyc cache clean', { stdio: 'inherit' });
    } catch (e) {
      // Ignore cache clean errors
    }
    
    console.log('✅ Coverage environment ready\n');
  }

  async runTestsWithCoverage() {
    console.log('🧪 Running tests with coverage instrumentation...\n');
    
    // Test 1: Core functionality with coverage
    await this.runSingleTest('Core Functionality Tests', 'test-simple.js');
    
    // Test 2: LSP Server tests with coverage
    await this.runSingleTest('LSP Server Tests', 'test-lsp-basic.js');
    
    // Test 3: Tool functionality tests
    await this.runToolTests();
    
    // Test 4: Provider tests
    await this.runProviderTests();
  }

  async runSingleTest(name, testFile) {
    console.log(`📊 Running ${name} with coverage...`);
    
    try {
      const result = execSync(`npx nyc --silent node ${testFile}`, {
        encoding: 'utf8',
        cwd: __dirname
      });
      console.log(result);
      console.log(`✅ ${name} completed\n`);
    } catch (error) {
      console.log(`⚠️  ${name} had some failures (see above)\n`);
    }
  }

  async runToolTests() {
    console.log('🔧 Testing Tool Manager functionality...');
    
    const testScript = `
      const { ToolManager } = require('./index.js');
      
      async function testTools() {
        const toolManager = new ToolManager();
        
        // Test tool execution
        console.log('Testing pwd:', await toolManager.executeTool('pwd', ''));
        console.log('Testing ls:', await toolManager.executeTool('ls', '.'));
        
        // Test code tracking
        console.log('Code tracker loaded:', toolManager.codeTracker !== undefined);
      }
      
      testTools().catch(console.error);
    `;
    
    await fs.writeFile(path.join(this.testDir, 'test-tools.js'), testScript);
    
    try {
      execSync(`npx nyc --silent node ${path.join(this.testDir, 'test-tools.js')}`, {
        stdio: 'inherit',
        cwd: __dirname
      });
      console.log('✅ Tool tests completed\n');
    } catch (error) {
      console.log('⚠️  Tool tests had issues\n');
    }
  }

  async runProviderTests() {
    console.log('🌐 Testing Provider Manager functionality...');
    
    const testScript = `
      const { ProviderManager, SecretManager } = require('./index.js');
      const secretManager = new SecretManager();
      const providerManager = new ProviderManager(secretManager);
      
      // Test provider info
      console.log('Current provider:', providerManager.getProviderInfo());
      
      // Test model listing
      try {
        const models = await providerManager.listModels();
        console.log('Available models:', models.length);
      } catch (e) {
        console.log('Model listing skipped (no connection)');
      }
    `;
    
    await fs.writeFile(path.join(this.testDir, 'test-providers.js'), testScript);
    
    try {
      execSync(`npx nyc --silent node ${path.join(this.testDir, 'test-providers.js')}`, {
        stdio: 'inherit',
        cwd: __dirname
      });
      console.log('✅ Provider tests completed\n');
    } catch (error) {
      console.log('⚠️  Provider tests had issues\n');
    }
  }

  async generateReport() {
    console.log('📈 Generating coverage report...\n');
    
    try {
      // Generate all report formats
      execSync('npx nyc report --reporter=text --reporter=html --reporter=lcov', {
        stdio: 'inherit',
        cwd: __dirname
      });
      
      console.log('✅ Coverage reports generated in ./coverage/\n');
    } catch (error) {
      console.error('❌ Failed to generate coverage report:', error.message);
    }
  }

  async analyzeCoverage() {
    console.log('📊 Analyzing code coverage results...\n');
    
    try {
      // Read the JSON coverage report
      const coverageFile = path.join(this.coverageDir, 'coverage-final.json');
      if (await fs.pathExists(coverageFile)) {
        const coverage = JSON.parse(await fs.readFile(coverageFile, 'utf8'));
        
        // Analyze coverage for each file
        const results = {};
        for (const [file, data] of Object.entries(coverage)) {
          const filename = path.basename(file);
          const statements = data.s;
          const branches = data.b;
          const functions = data.f;
          
          const stmtCovered = Object.values(statements).filter(v => v > 0).length;
          const stmtTotal = Object.values(statements).length;
          
          const funcCovered = Object.values(functions).filter(v => v > 0).length;
          const funcTotal = Object.values(functions).length;
          
          results[filename] = {
            statements: stmtTotal > 0 ? ((stmtCovered / stmtTotal) * 100).toFixed(1) : 0,
            functions: funcTotal > 0 ? ((funcCovered / funcTotal) * 100).toFixed(1) : 0,
            stmtCovered,
            stmtTotal,
            funcCovered,
            funcTotal
          };
        }
        
        // Display results
        console.log('📋 Coverage Summary by File:\n');
        for (const [file, coverage] of Object.entries(results)) {
          console.log(`📄 ${file}:`);
          console.log(`   Statements: ${coverage.statements}% (${coverage.stmtCovered}/${coverage.stmtTotal})`);
          console.log(`   Functions:  ${coverage.functions}% (${coverage.funcCovered}/${coverage.funcTotal})\n`);
        }
        
        // Calculate overall coverage
        const totalStmtCovered = Object.values(results).reduce((sum, r) => sum + r.stmtCovered, 0);
        const totalStmtTotal = Object.values(results).reduce((sum, r) => sum + r.stmtTotal, 0);
        const totalFuncCovered = Object.values(results).reduce((sum, r) => sum + r.funcCovered, 0);
        const totalFuncTotal = Object.values(results).reduce((sum, r) => sum + r.funcTotal, 0);
        
        const overallStmt = totalStmtTotal > 0 ? ((totalStmtCovered / totalStmtTotal) * 100).toFixed(1) : 0;
        const overallFunc = totalFuncTotal > 0 ? ((totalFuncCovered / totalFuncTotal) * 100).toFixed(1) : 0;
        
        console.log('📊 Overall Coverage:');
        console.log(`   Statements: ${overallStmt}%`);
        console.log(`   Functions:  ${overallFunc}%`);
        
        // Check thresholds
        console.log('\n🎯 Coverage Thresholds:');
        console.log(`   Statements: ${overallStmt}% ${overallStmt >= 60 ? '✅' : '❌'} (minimum: 60%)`);
        console.log(`   Functions:  ${overallFunc}% ${overallFunc >= 50 ? '✅' : '❌'} (minimum: 50%)`);
        
      } else {
        console.log('⚠️  No coverage data found. Check test execution.');
      }
      
      // Show report location
      console.log('\n📂 Detailed HTML report available at:');
      console.log(`   ${path.join(this.coverageDir, 'index.html')}`);
      console.log('\n💡 Open in browser: npx http-server coverage -o');
      
    } catch (error) {
      console.error('❌ Failed to analyze coverage:', error.message);
    }
  }
}

// Run if called directly
if (require.main === module) {
  const runner = new CoverageTestRunner();
  runner.run().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}

module.exports = CoverageTestRunner;