#!/bin/bash
set -euo pipefail

# Documentation Update Script
# Automatically updates lantae.md by reading from provider files

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

show_progress() {
    echo -e "${BLUE}[DOCS] $1${NC}"
}

show_success() {
    echo -e "${GREEN}✓ [DOCS] $1${NC}"
}

show_warning() {
    echo -e "${YELLOW}⚠ [DOCS] $1${NC}"
}

error_exit() {
    echo -e "${RED}ERROR [DOCS]: $1${NC}" >&2
    exit 1
}

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Paths
DOCS_DIR="$PROJECT_ROOT/docs"
PROVIDERS_DIR="$DOCS_DIR/providers"
LANTAE_MD="$PROJECT_ROOT/lantae.md"
TEMPLATE_FILE="$PROJECT_ROOT/lantae.md.template"

# Check if we're in the right directory
verify_project_structure() {
    show_progress "Verifying project structure..."
    
    if [[ ! -d "$PROVIDERS_DIR" ]]; then
        error_exit "Providers directory not found: $PROVIDERS_DIR"
    fi
    
    if [[ ! -f "$LANTAE_MD" ]]; then
        error_exit "lantae.md not found: $LANTAE_MD"
    fi
    
    show_success "Project structure verified"
}

# Extract summary from provider file
extract_provider_summary() {
    local provider_file="$1"
    local provider_name="$2"
    
    if [[ ! -f "$provider_file" ]]; then
        show_warning "Provider file not found: $provider_file"
        return 1
    fi
    
    # Extract key information from the provider file
    local overview
    local key_benefits
    local setup
    local models
    
    overview=$(grep -A 3 "## Overview" "$provider_file" | tail -n +2 | head -n 1 || echo "")
    key_benefits=$(grep -A 10 "## Key Benefits" "$provider_file" | grep "^-" | head -n 3 || echo "")
    setup=$(grep -A 5 "### API Key Setup" "$provider_file" | grep "export\|\.env" | head -n 1 || echo "")
    
    # Generate provider section
    cat << EOF

### $provider_name
*${overview}*

**Setup**: \`${setup}\`

**Key Benefits**:
${key_benefits}

[📖 Full $provider_name Documentation](docs/providers/$(basename "$provider_file"))

---
EOF
}

# Read provider files and generate content
generate_provider_content() {
    show_progress "Generating provider content..."
    
    local provider_content=""
    
    # Process each provider file
    for provider_file in "$PROVIDERS_DIR"/*.md; do
        if [[ -f "$provider_file" ]]; then
            local basename_file
            basename_file=$(basename "$provider_file" .md)
            
            # Skip comparison file
            if [[ "$basename_file" == "provider-comparison" ]]; then
                continue
            fi
            
            # Capitalize first letter for display
            local provider_name
            provider_name=$(echo "$basename_file" | sed 's/^./\U&/')
            
            show_progress "Processing $provider_name provider..."
            provider_content+=$(extract_provider_summary "$provider_file" "$provider_name")
        fi
    done
    
    echo "$provider_content"
}

# Update lantae.md with new content
update_lantae_md() {
    local provider_content="$1"
    
    show_progress "Updating lantae.md..."
    
    # Create backup
    cp "$LANTAE_MD" "$LANTAE_MD.backup"
    
    # Read current file and replace content between markers
    awk -v content="$provider_content" '
        /<!-- PROVIDER_CONTENT_START -->/ {
            print
            print content
            skip = 1
            next
        }
        /<!-- PROVIDER_CONTENT_END -->/ {
            skip = 0
        }
        !skip
    ' "$LANTAE_MD.backup" > "$LANTAE_MD"
    
    # Update timestamp
    sed -i.tmp "s/Last generated: .*/Last generated: $(date '+%Y-%m-%d %H:%M:%S')/" "$LANTAE_MD"
    rm -f "$LANTAE_MD.tmp"
    
    show_success "lantae.md updated"
}

# Validate generated file
validate_output() {
    show_progress "Validating generated documentation..."
    
    # Check that file is not empty
    if [[ ! -s "$LANTAE_MD" ]]; then
        error_exit "Generated lantae.md is empty"
    fi
    
    # Check that markers are present
    if ! grep -q "<!-- PROVIDER_CONTENT_START -->" "$LANTAE_MD"; then
        error_exit "Start marker not found in lantae.md"
    fi
    
    if ! grep -q "<!-- PROVIDER_CONTENT_END -->" "$LANTAE_MD"; then
        error_exit "End marker not found in lantae.md"
    fi
    
    # Check that content was added
    local content_lines
    content_lines=$(awk '/<!-- PROVIDER_CONTENT_START -->/,/<!-- PROVIDER_CONTENT_END -->/' "$LANTAE_MD" | wc -l)
    
    if [[ $content_lines -lt 10 ]]; then
        error_exit "Generated content appears too short ($content_lines lines)"
    fi
    
    show_success "Documentation validation passed"
}

# Generate provider comparison table
update_comparison_table() {
    show_progress "Updating provider comparison table..."
    
    # This could be enhanced to read actual metrics from provider files
    # For now, we'll leave the existing static table
    
    show_success "Comparison table maintained"
}

# Main execution
main() {
    show_progress "Starting documentation update..."
    
    verify_project_structure
    
    # Generate new provider content
    local provider_content
    provider_content=$(generate_provider_content)
    
    if [[ -z "$provider_content" ]]; then
        error_exit "No provider content generated"
    fi
    
    # Update the main documentation file
    update_lantae_md "$provider_content"
    
    # Validate the output
    validate_output
    
    # Update comparison table
    update_comparison_table
    
    show_success "Documentation update complete!"
    
    # Show summary
    echo ""
    echo -e "${BLUE}Summary:${NC}"
    echo "  • Updated: lantae.md"
    echo "  • Providers processed: $(find "$PROVIDERS_DIR" -name "*.md" ! -name "provider-comparison.md" | wc -l | tr -d ' ')"
    echo "  • Backup created: lantae.md.backup"
    echo "  • Last updated: $(date '+%Y-%m-%d %H:%M:%S')"
    
    # Clean up old backup if successful
    if [[ -f "$LANTAE_MD.backup" ]]; then
        rm -f "$LANTAE_MD.backup"
    fi
}

# Usage information
if [[ "${1:-}" == "--help" || "${1:-}" == "-h" ]]; then
    echo "Documentation Update Script"
    echo ""
    echo "Usage: $0"
    echo ""
    echo "This script automatically updates lantae.md by reading from provider"
    echo "documentation files in docs/providers/."
    echo ""
    echo "The script will:"
    echo "  1. Read all provider .md files"
    echo "  2. Extract key information from each"
    echo "  3. Update the provider content section in lantae.md"
    echo "  4. Validate the generated output"
    echo ""
    exit 0
fi

main "$@"