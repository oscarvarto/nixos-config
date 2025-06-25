#!/bin/bash

# Test script to verify Maven classpath generation works with bash-env
# This helps debug the Java testing environment setup

PROJECT_DIR="${1:-$(pwd)}"

echo "Testing Maven classpath generation in: $PROJECT_DIR"
echo "================================================"

# Check if pom.xml exists
if [[ ! -f "$PROJECT_DIR/pom.xml" ]]; then
    echo "❌ No pom.xml found in $PROJECT_DIR"
    exit 1
fi

echo "✓ Found pom.xml"

# Test with bash-env if available
if command -v bash-env >/dev/null 2>&1; then
    echo "🔧 Testing with bash-env..."
    CLASSPATH_OUTPUT=$(cd "$PROJECT_DIR" && bash-env mvn dependency:build-classpath -Dmdep.outputFile=/dev/stdout -q 2>/dev/null)
    if [[ -n "$CLASSPATH_OUTPUT" && ! "$CLASSPATH_OUTPUT" =~ (ERROR|WARN) ]]; then
        echo "✓ bash-env Maven classpath generation successful"
        echo "📋 Classpath length: $(echo "$CLASSPATH_OUTPUT" | wc -c) characters"
        echo "📁 First few entries:"
        echo "$CLASSPATH_OUTPUT" | tr ':' '\n' | head -5
    else
        echo "⚠️  bash-env Maven classpath generation failed or returned warnings"
        echo "Output: $CLASSPATH_OUTPUT"
    fi
else
    echo "⚠️  bash-env command not found"
fi

# Test with regular mvn
echo "🔧 Testing with regular mvn..."
REGULAR_CLASSPATH=$(cd "$PROJECT_DIR" && mvn dependency:build-classpath -Dmdep.outputFile=/dev/stdout -q 2>/dev/null)
if [[ -n "$REGULAR_CLASSPATH" && ! "$REGULAR_CLASSPATH" =~ (ERROR|WARN) ]]; then
    echo "✓ Regular Maven classpath generation successful"
    echo "📋 Classpath length: $(echo "$REGULAR_CLASSPATH" | wc -c) characters"
else
    echo "❌ Regular Maven classpath generation failed"
    echo "Output: $REGULAR_CLASSPATH"
fi

echo "================================================"
echo "✅ Test completed"
