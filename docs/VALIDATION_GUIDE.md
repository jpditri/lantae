# Lantae Validation & Tech Debt Management

This guide explains how to use Lantae's code validation and tech debt tracking system.

## Overview

Lantae includes a comprehensive validation system that performs:
- **Static Analysis**: Code style, security, complexity checks
- **QA Checks**: Test coverage, dead code detection, documentation analysis  
- **Dry-run Validation**: Syntax checking, dependency validation, smoke tests
- **Tech Debt Tracking**: Manual debt items with priority and effort tracking

## Quick Start

```bash
# Run all validations
rake validate

# Show tech debt summary
rake debt

# Run validations with auto-fix
rake validate:fix
```

## Validation Commands

### Static Analysis
```bash
rake validate:static    # RuboCop, security, complexity
```

### QA Checks  
```bash
rake validate:qa        # Coverage, dead code, documentation
```

### Dry-run Validation
```bash
rake validate:dry_run   # Syntax, dependencies, smoke test
```

### Auto-fix Issues
```bash
rake validate:fix       # Fix issues automatically where possible
```

## Tech Debt Management

### View Tech Debt
```bash
rake debt:summary       # Quick overview
rake debt:report        # Detailed report
```

### Add Manual Tech Debt
```bash
rake debt:add['Title','Description','priority']

# Example:
rake debt:add['Fix token counting','Replace char/4 with proper tokenizer','high']
```

### Resolve Tech Debt
```bash
rake debt:resolve[item_id]

# Example:  
rake debt:resolve[context-calculation-fix]
```

### Generate Reports
```bash
rake debt:html          # Generate HTML report
rake debt:csv           # Export to CSV
```

## Validation Categories

### Static Analysis

**RuboCop**: Style and convention violations
- Auto-fixable issues are corrected with `rake validate:fix`
- Manual review needed for complex violations

**Security**: Potential security issues
- Hardcoded secrets/API keys
- Unsafe code execution (eval, system calls)
- File permission issues

**Complexity**: Code complexity metrics  
- Large files (>500 lines)
- Long methods (>50 lines)
- High cyclomatic complexity

### QA Checks

**Test Coverage**: Code coverage analysis
- Requires SimpleCov gem for detailed metrics
- Flags coverage below 80%
- Identifies untested code paths

**Dead Code**: Unused code detection
- Finds potentially unused methods
- Basic heuristic analysis
- Manual verification recommended

**Documentation**: Missing documentation
- Classes without doc comments
- Public methods lacking descriptions
- README and guide completeness

### Dry-run Validation

**Syntax Check**: Ruby syntax validation
- Parses all Ruby files for syntax errors
- Critical priority for any failures
- Must pass before deployment

**Dependencies**: Dependency validation
- Checks Gemfile.lock is current
- Verifies all gems are installed
- Identifies version conflicts

**Integration Test**: Basic smoke testing
- Runs core functionality tests
- Validates API endpoints work
- Checks provider connections

## Tech Debt Tracking

### Priority Levels

- **Critical**: System-breaking issues, security vulnerabilities
- **High**: Major functionality issues, performance problems  
- **Medium**: Quality improvements, minor bugs
- **Low**: Style issues, documentation gaps

### Manual Debt Items

Add items for issues that automated tools can't detect:
- Architecture decisions needing revision
- Performance optimization opportunities  
- User experience improvements
- Technical documentation updates

### Effort Estimation

Use consistent effort estimates:
- **1-2 hours**: Quick fixes, style changes
- **2-4 hours**: Feature enhancements, bug fixes
- **4-8 hours**: Significant refactoring, new features
- **1-2 days**: Major architectural changes

## Best Practices

### Regular Validation
```bash
# Add to CI/CD pipeline
rake validate

# Weekly tech debt review
rake debt:report
```

### Prioritization
1. Fix critical issues immediately
2. Address high-priority items before new features
3. Allocate 20% of development time to tech debt
4. Review and update debt items regularly

### Team Workflow
1. Run validations before committing code
2. Add manual debt items during code reviews
3. Discuss tech debt in team meetings
4. Track resolution progress over time

## Integration with CI/CD

Add validation to your CI pipeline:

```yaml
# .github/workflows/validate.yml
name: Code Validation
on: [push, pull_request]
jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Ruby
        uses: ruby/setup-ruby@v1
        with:
          bundler-cache: true
      - name: Run validations
        run: rake validate
      - name: Upload tech debt report
        uses: actions/upload-artifact@v2
        with:
          name: tech-debt-report
          path: data/tech_debt_report.html
```

## Configuration

### Validation Settings

Customize validation behavior in `lib/ruby/validation_engine.rb`:

```ruby
# Adjust thresholds
LARGE_FILE_THRESHOLD = 500    # lines
LONG_METHOD_THRESHOLD = 50    # lines  
COVERAGE_THRESHOLD = 80       # percentage
```

### Tech Debt Categories

Add custom categories in `data/tech_debt.json`:

```json
{
  "categories": {
    "performance": { "description": "Performance optimizations" },
    "security": { "description": "Security improvements" },
    "ux": { "description": "User experience enhancements" }
  }
}
```

## Reports and Visualization

### HTML Report
- Visual dashboard with charts and metrics
- Filterable by priority and category
- Trend analysis over time
- Shareable with stakeholders

### CSV Export  
- Raw data for analysis
- Import into project management tools
- Historical tracking and reporting
- Custom metric calculations

## Troubleshooting

### Common Issues

**RuboCop errors**: Update `.rubocop.yml` configuration
**Missing gems**: Run `bundle install`  
**Test failures**: Check test dependencies and setup
**Permission errors**: Ensure write access to `data/` directory

### Debug Mode

Enable verbose output:
```bash
VERBOSE=1 rake validate
```

### Manual Validation

Run individual checks:
```bash
rubocop lib/ spec/ bin/
rspec --require simplecov
ruby -c lib/ruby/providers/base_provider.rb
```

## Contributing

When adding new validation checks:

1. Add the check to `ValidationEngine`
2. Update tech debt schema if needed
3. Add tests for the new validation
4. Update this documentation
5. Consider performance impact

For tech debt workflow improvements:
1. Discuss with team first
2. Update reporter templates
3. Test with existing debt data
4. Document new workflows

---

*For more information, see the main [README](../README.md) and [Architecture Guide](ARCHITECTURE.md).*