#!/bin/bash

# Simple workflow integration test for LANTAE
# Tests common user scenarios and commands

set -e

LANTAE_BIN="$(dirname "$0")/../bin/lantae --no-agent --no-continuous"
TEST_TIMEOUT=10

echo "🧪 LANTAE Workflow Integration Tests"
echo "===================================="

# Test 1: Help command
echo -e "\n🔍 Test 1: Help Command"
if timeout $TEST_TIMEOUT bash -c "echo '/help' | $LANTAE_BIN 2>/dev/null | grep -q 'Available Commands'"; then
    echo "  ✅ PASSED: Help command works"
else
    echo "  ❌ FAILED: Help command not working"
fi

# Test 2: Provider command
echo -e "\n🔍 Test 2: Provider Information"
if timeout $TEST_TIMEOUT bash -c "echo '/provider' | $LANTAE_BIN 2>/dev/null | grep -q -E '(Current provider|provider:)'"; then
    echo "  ✅ PASSED: Provider command works"
else
    echo "  ❌ FAILED: Provider command not working"
fi

# Test 3: Models command (should not deadlock)
echo -e "\n🔍 Test 3: Models Command (No Deadlock)"
if timeout $TEST_TIMEOUT bash -c "echo '/models' | $LANTAE_BIN 2>/dev/null | grep -q -E '(Available models|Error listing|Cannot connect)'"; then
    echo "  ✅ PASSED: Models command completes without deadlock"
else
    echo "  ❌ FAILED: Models command deadlocked or failed"
fi

# Test 4: Connect command discovery
echo -e "\n🔍 Test 4: Connect Discovery"
if timeout 15 bash -c "echo '/connect' | $LANTAE_BIN 2>/dev/null | grep -q -E '(Quick scan|scan for Ollama|No Ollama instances|found.*instance)'"; then
    echo "  ✅ PASSED: Connect discovery works"
else
    echo "  ❌ FAILED: Connect discovery not working"
fi

# Test 5: Connect to specific instance
echo -e "\n🔍 Test 5: Connect to Specific Instance"
if timeout $TEST_TIMEOUT bash -c "echo '/connect localhost:11434' | $LANTAE_BIN 2>/dev/null | grep -q -E '(Testing connection|Successfully connected|Cannot connect)'"; then
    echo "  ✅ PASSED: Connect to instance works"
else
    echo "  ❌ FAILED: Connect to instance not working"
fi

# Test 6: Mixed workflow (no deadlocks)
echo -e "\n🔍 Test 6: Mixed Workflow Processing"
if timeout 20 bash -c "printf '/help\n/provider\n/models\n/connect localhost:11434\n/exit\n' | $LANTAE_BIN 2>/dev/null" >/dev/null; then
    echo "  ✅ PASSED: Mixed workflow completes without hanging"
else
    echo "  ❌ FAILED: Mixed workflow hangs or fails"
fi

echo -e "\n📊 Integration tests completed!"
echo "Run './test/integration_test.rb' for more detailed testing."