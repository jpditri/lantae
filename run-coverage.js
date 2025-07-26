#!/usr/bin/env node
/**
 * Simple Coverage Runner using c8
 */

const { execSync } = require('child_process');
const fs = require('fs-extra');
const path = require('path');

async function runCoverage() {
  console.log('🔬 Running Code Coverage Analysis with c8\n');
  
  // Clean coverage directory
  await fs.remove('./coverage');
  
  try {
    // Run simple test with coverage
    console.log('📊 Running tests with coverage...\n');
    execSync('npx c8 --reporter=html --reporter=text node test-simple.js', {
      stdio: 'inherit',
      cwd: __dirname
    });
    
    console.log('\n✅ Coverage analysis complete!');
    console.log('\n📂 HTML report available at: ./coverage/index.html');
    console.log('💡 View report: npx http-server coverage -o\n');
    
  } catch (error) {
    console.error('Coverage analysis completed with some test failures\n');
  }
}

runCoverage().catch(console.error);