# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Codebase Architecture

This is a performance-optimized Emacs 30.1 configuration with a three-file architecture:

- **`early-init.el`**: Startup optimizations, GC tuning, native compilation setup, UI element early disabling
- **`init.el`**: Main configuration with 74+ packages organized into 11 logical sections
- **`custom.el`**: User interface customizations and theme settings
- **`site-lisp/utilities/`**: Custom utility functions (14 functions for text manipulation, file operations, config management)

## Package Management Strategy

**Primary**: Built-in `use-package` with ELPA repositories (GNU > MELPA Stable > MELPA priority)
**Secondary**: `straight.el` for specific packages (Claude Code integration)

All packages use deferred loading patterns with `:defer`, `:hook`, and `:after` for performance.

## Configuration Structure

The `init.el` is organized into these sections:
1. Basic Setup and Package Management
2. General Emacs Settings
3. Terminal Configuration
4. UI/UX Enhancements
5. Navigation and Completion (Vertico ecosystem)
6. Text Editing and Formatting
7. Development Tools (Eglot, Tree-sitter)
8. Version Control (Magit)
9. Terminal and Shell (VTerm)
10. Window and Buffer Management
11. Language-specific Modes

## Development Commands

### Configuration Management
- **Reload config**: `M-x reload-init-file` (from utilities.el)
- **Edit config**: `M-x edit-init-file` 
- **Open config directory**: `M-x open-config-dir`
- **Byte compile on save**: Automatic for .el files

### Testing and Validation
- **Check package statistics**: `M-x use-package-report` (shows load times)
- **Native compilation status**: Check `*Warnings*` buffer for compilation errors
- **Startup profiling**: Built-in with `use-package-compute-statistics` enabled

### Language Server Management (Eglot)
- **Start LSP**: Automatic via hooks for supported languages
- **Code actions**: `C-c l a`
- **Rename**: `C-c l r`
- **Format**: `C-c l f`
- **Documentation**: `C-c l d`

### Tree-sitter Grammar Management
- **Install grammar**: `M-x treesit-install-language-grammar`
- **Check available**: `M-x treesit-language-available-p`
- Grammars auto-install on first use with error handling

## Key Technologies

### Emacs 30.1 Features
- **Native compilation**: JIT enabled with dedicated cache (`eln-cache/`)
- **Tree-sitter**: 15+ languages with auto-installation
- **Eglot**: Built-in LSP client (replaces external LSP packages)
- **Project.el**: Built-in project management (replaces Projectile)
- **Tab Bar**: Native tab support with custom keybindings
- **Pixel scrolling**: Advanced smooth scrolling with momentum

### Completion Stack
- **Vertico**: Minibuffer completion UI
- **Orderless**: Flexible completion matching
- **Marginalia**: Rich completion annotations
- **Corfu**: In-buffer completion popup
- **Cape**: Completion-at-point extensions
- **Embark**: Context-aware actions

### Development Tools
- **Magit**: Git interface with fullscreen mode
- **VTerm**: Full terminal emulator with toggle
- **Deadgrep**: ripgrep integration
- **Flycheck**: Syntax checking with custom CloudFormation linter
- **Treemacs**: File explorer with Nerd Icons theme

## Coding Patterns

### Package Declaration Pattern
```elisp
(use-package package-name
  :ensure t                    ; Auto-install from ELPA
  :defer t                     ; Lazy load for performance
  :init                        ; Code to run before loading
  :config                      ; Code to run after loading
  :bind                        ; Key bindings
  :hook                        ; Mode hooks
  :custom)                     ; Variable settings
```

### Performance Considerations
- Declare functions before use to silence compiler warnings
- Use `:defer` and hooks for lazy loading
- Prefer built-in packages over external ones
- Native compilation is enabled for all Elisp code

### Custom Function Location
- Add new utilities to `site-lisp/utilities/utilities.el`
- Follow existing docstring format with detailed descriptions
- Use `(provide 'utilities)` at the end

## Commit Protocol

1. **Always update README.md** for:
   - Package additions/removals
   - Keybinding changes
   - Feature additions or major configuration changes
   - UI/UX modifications

2. **Use semantic commit prefixes**: `feat:`, `fix:`, `refactor:`, `docs:`

3. **Validation checks**:
   - Ensure code is byte-compilable
   - Check native compilation doesn't produce errors
   - Verify package declarations have proper structure
   - Test keybindings don't conflict

## Environment Setup

- **API Keys**: Set environment variables (e.g., `OPENAI_API_KEY` for ChatGPT Shell)
- **External Tools**: Requires `git`, `ripgrep` for full functionality
- **Optional**: `gcc`, `libgccjit` for native compilation on macOS
- **Tree-sitter**: Grammars auto-install using git protocol URLs