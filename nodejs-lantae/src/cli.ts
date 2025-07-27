#!/usr/bin/env node

import { Command } from 'commander';
import chalk from 'chalk';
import { config } from 'dotenv';
import { version } from '../package.json';

// Load environment variables
config();

const program = new Command();

function showBanner(): void {
  console.log(chalk.yellow('╔══════════════════════════════════════════════════════════════╗'));
  console.log(chalk.yellow('║  🟨 Lantae Node.js Implementation v1.0.0                    ║'));
  console.log(chalk.yellow('║  ⚡ TypeScript Multi-Provider LLM Interface                 ║'));
  console.log(chalk.yellow('║  🚧 Under Development - See Feature Parity Document        ║'));
  console.log(chalk.yellow('╚══════════════════════════════════════════════════════════════╝'));
  console.log();
}

program
  .name('lantae')
  .version(version)
  .description('Multi-Provider LLM Interface - Node.js Implementation')
  .option('-p, --provider <provider>', 'LLM provider to use', 'ollama')
  .option('-m, --model <model>', 'Model to use for the specified provider', 'cogito:latest')
  .option('-t, --temperature <temp>', 'Temperature for response randomness (0.0-1.0)', parseFloat)
  .option('-y, --auto-accept', 'Automatically accept all prompts')
  .option('--planning-mode', 'Enable detailed task planning')
  .option('--agent', 'Enable autonomous agent execution')
  .option('--no-banner', 'Disable startup banner')
  .argument('[prompt]', 'Single prompt to process (if not provided, starts interactive mode)')
  .action(async (prompt: string | undefined, options) => {
    // Show banner unless disabled
    if (options.banner !== false) {
      showBanner();
    }

    console.log(chalk.cyan('🟨 Lantae Node.js Implementation'));
    console.log(chalk.white(`Provider: ${options.provider}`));
    console.log(chalk.white(`Model: ${options.model}`));
    
    if (options.temperature !== undefined) {
      console.log(chalk.white(`Temperature: ${options.temperature}`));
    }
    
    if (options.autoAccept) {
      console.log(chalk.white('Auto-accept mode enabled'));
    }
    
    if (options.planningMode) {
      console.log(chalk.white('Planning mode enabled'));
    }
    
    if (options.agent) {
      console.log(chalk.white('Agent mode enabled'));
    }

    console.log();

    // Check for single prompt mode
    if (prompt) {
      console.log(chalk.white(`Processing prompt: ${prompt}`));
      // TODO: Implement single prompt processing
      console.log(chalk.yellow('⚠️  Single prompt mode not yet implemented'));
      process.exit(1);
    } else {
      // Start interactive mode
      console.log(chalk.white('Starting interactive mode...'));
      // TODO: Implement REPL
      console.log(chalk.yellow('⚠️  Interactive mode not yet implemented'));
      console.log(chalk.blue('💡 This is a placeholder for the Node.js implementation'));
      console.log(chalk.blue('📖 See the feature parity document for implementation status'));
      console.log();
      console.log(chalk.green('🔧 Development commands:'));
      console.log(chalk.white('  npm run dev     - Run in development mode'));
      console.log(chalk.white('  npm run build   - Build TypeScript'));
      console.log(chalk.white('  npm test        - Run tests'));
      console.log(chalk.white('  npm run lint    - Lint code'));
    }
  });

// Parse command line arguments
program.parse(process.argv);