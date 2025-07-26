
      const { ToolManager } = require('./index.js');
      const toolManager = new ToolManager();
      
      // Test tool execution
      console.log('Testing pwd:', await toolManager.executeTool('pwd', ''));
      console.log('Testing ls:', await toolManager.executeTool('ls', '.'));
      
      // Test code tracking
      console.log('Code tracker loaded:', toolManager.codeTracker !== undefined);
    