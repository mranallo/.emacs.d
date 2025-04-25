# Claude Code Instructions

This file contains specific instructions for Claude Code when working with this Emacs configuration.

## General Guidelines

1. Always maintain the existing code style and conventions
2. Document all changes clearly
3. Follow the project's existing structure and package management approach

## Commit Protocol

When creating a commit:

1. **Always update the README.md file** to reflect any changes made to:
   - Package additions/removals
   - Keybinding changes
   - Feature additions or major configuration changes
   - UI/UX modifications

2. Follow the commit message format:
   - Use semantic prefixes: `feat:`, `fix:`, `refactor:`, `docs:`, etc.
   - Write a clear, concise title
   - Include a detailed description of the changes
   - Explain the reasoning behind significant changes

3. Run these checks before committing:
   - Ensure code is byte-compilable (no obvious errors)
   - Maintain descriptive comments for complex code sections
   - Preserve existing keyboard shortcut conventions

## Documentation Standards

When updating the README.md:

1. Keep the sections organized as they currently are
2. Add new packages under the appropriate "Highlights" section
3. Document any new keybindings in the "Keybindings" section
4. Update installation instructions if new dependencies are added

## Linting and Testing

Before finalizing any changes:

1. Check for proper indentation and formatting
2. Ensure parentheses are balanced in all Lisp code
3. Verify package declarations have proper `:ensure` and `:config` sections where needed