
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
    