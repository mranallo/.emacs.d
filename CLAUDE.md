# CLAUDE.md

This file provides comprehensive guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Codebase Architecture

This is a performance-optimized Emacs 30.1 configuration with the following structure:

### Core Configuration Files
- **`early-init.el`** (68 lines): Startup optimizations, GC tuning, native compilation setup, UI element early disabling
- **`init.el`** (993 lines): Main configuration with 80+ packages organized into 11 logical sections
- **`custom.el`**: User interface customizations, theme settings, and auto-generated custom variables
- **`GEMINI.md`**: Alternative AI assistant guidance for Gemini-based tools

### Directory Structure
```
~/.emacs.d/
├── early-init.el          # Startup optimizations
├── init.el                # Main configuration
├── custom.el              # UI customizations
├── compile-init.el        # Byte compilation helper
├── site-lisp/
│   ├── utilities/
│   │   └── utilities.el   # 14 custom utility functions
│   └── themes/
│       └── nordic-night-theme.el
├── straight/              # Straight.el package manager cache
├── eln-cache/            # Native compilation cache
├── elpa/                 # ELPA packages
├── transient/            # Transient state files
└── backups/              # Backup files directory
```

## Package Management Strategy

### Dual Package Management System
1. **Primary**: Built-in `use-package` with ELPA repositories
   - Repository priority: GNU ELPA (10) > MELPA Stable (5) > MELPA (0)
   - Auto-installation enabled via `:ensure t`
   - Statistics tracking enabled for debugging

2. **Secondary**: `straight.el` for development packages
   - Bootstrap version 6
   - Used specifically for packages not in ELPA or requiring latest development versions
   - Set `straight-use-package-by-default` to `nil` to prevent conflicts

### Performance Optimizations
- All packages use deferred loading patterns (`:defer t`, `:hook`, `:after`)
- Native compilation enabled with JIT for all packages
- Byte compilation on save for all `.el` files
- GCMH manages garbage collection with 64MB high threshold

## Configuration Structure

The `init.el` is organized into these sections with line numbers:
1. **Basic Setup and Package Management** (lines 1-85)
   - Straight.el bootstrap, ELPA setup, GCMH, server start
2. **General Emacs Settings** (lines 86-176)
   - Startup optimizations, backup strategy, scrolling, key bindings
3. **Terminal Configuration** (lines 177-201)
   - Terminal detection macros, xterm mouse support
4. **UI/UX Enhancements** (lines 202-307)
   - Doom themes/modeline, Solaire mode, Beacon, Nerd Icons, Tab Bar, Winum
5. **Navigation and Completion** (lines 308-452)
   - Vertico, Orderless, Marginalia, Corfu, Cape, Embark, Consult, Treemacs
6. **Text Editing and Formatting** (lines 564-652)
   - Comment-dwim-2, Expand-region, Whitespace cleanup, Undo-fu, Ligatures
7. **Development Tools** (lines 653-839)
   - Exec-path-from-shell, Flycheck, Eglot LSP, Tree-sitter, EditorConfig
8. **Version Control** (lines 840-850)
   - Magit with fullscreen mode
9. **Terminal and Shell** (lines 851-888)
   - VTerm with toggle functionality
10. **Window and Buffer Management** (lines 889-900)
    - Projectile, Deadgrep, Ace-window
11. **Language-specific Modes** (lines 901-993)
    - Go, YAML, Docker, Markdown, Web, JSON, TypeScript

## Complete Package Inventory

### Core Infrastructure
- **gcmh**: Garbage collection management (64MB high/16MB low thresholds)
- **use-package**: Package configuration framework
- **straight.el**: Alternative package manager for development packages
- **exec-path-from-shell**: Environment variable synchronization

### UI/Theme Packages
- **doom-themes**: Modern theme collection (using doom-one-light)
- **doom-modeline**: Enhanced modeline with icons
- **solaire-mode**: Visual distinction for file-visiting windows
- **beacon**: Cursor highlighting on scroll
- **nerd-icons**: Icon framework (replaces all-the-icons)
- **nerd-icons-completion**: Icons in completion UI
- **nerd-icons-dired**: Icons in dired
- **treemacs-nerd-icons**: Nerd icons for Treemacs
- **ligature**: Programming font ligatures support

### Completion System (Vertico Ecosystem)
- **vertico**: Vertical minibuffer completion UI
- **orderless**: Flexible completion matching
- **marginalia**: Rich annotations in minibuffer
- **corfu**: In-buffer completion popup
- **cape**: Completion-at-point extensions
- **embark**: Context-aware actions
- **consult**: Search and navigation commands
- **savehist**: Minibuffer history persistence
- **recentf**: Recently opened files tracking

### Development Tools
- **eglot**: Built-in LSP client (enhanced for Emacs 30.1)
- **treesit**: Built-in Tree-sitter support (15 languages configured)
- **flycheck**: Syntax checking framework
- **flycheck-pos-tip**: Tooltips for flycheck errors
- **editorconfig**: EditorConfig support
- **chatgpt-shell**: OpenAI API integration
- **copilot**: GitHub Copilot integration
- **claude-shell**: Claude API integration (via straight.el)

### Version Control
- **magit**: Advanced Git interface
- **git-gutter**: Show git diff in gutter
- **treemacs-magit**: Treemacs-Magit integration

### File/Project Management
- **treemacs**: File tree sidebar with full configuration suite
- **treemacs-projectile**: Project integration
- **treemacs-persp**: Perspective integration
- **treemacs-tab-bar**: Tab bar integration
- **projectile**: Project management framework
- **deadgrep**: ripgrep interface

### Text Editing
- **expand-region**: Semantic region expansion
- **comment-dwim-2**: Enhanced commenting
- **whitespace-cleanup-mode**: Automatic whitespace management
- **undo-fu**: Enhanced undo/redo
- **browse-kill-ring**: Kill ring browser
- **persistent-scratch**: Persistent scratch buffer
- **deft**: Note-taking interface (linked to NotePlan)

### Terminal/Shell
- **vterm**: Full terminal emulator
- **vterm-toggle**: Quick terminal toggle
- **eat**: Fast terminal emulator implemented in Emacs Lisp with Eshell integration

### Window Management
- **winum**: Number-based window navigation
- **ace-window**: Quick window switching
- **winner-mode**: Window configuration undo/redo
- **tab-bar**: Built-in tab management

### Language-specific
- **go-mode**: Go language support
- **yaml-mode**: YAML support
- **dockerfile-mode**: Dockerfile support
- **markdown-mode**: Markdown editing
- **web-mode**: Web template editing
- **json-mode**: JSON support
- **typescript-mode**: TypeScript support

## Development Commands

### Configuration Management
- **Reload config**: `M-x reload-init-file` (utilities.el:70)
- **Edit config**: `M-x edit-init-file` (utilities.el:76)
- **Open config directory**: `M-x open-config-dir` (utilities.el:81)
- **Byte compile on save**: Automatic for all .el files
- **Check startup time**: `M-x emacs-init-time`
- **Package report**: `M-x use-package-report` (shows load times)

### File Operations (Custom Utilities)
- **Copy file path**: `M-x copy-file-path` (utilities.el:137)
- **Copy file name**: `M-x copy-file-name` (utilities.el:144)
- **Rename current file**: `M-x rename-current-file` (utilities.el:151)
- **Delete current file**: `M-x delete-current-file` (utilities.el:164)
- **Sudo edit**: `M-x sudo-edit` (utilities.el:86)

### Testing and Validation
- **Check package statistics**: `M-x use-package-report`
- **Native compilation status**: Check `*Warnings*` buffer
- **Startup profiling**: `use-package-compute-statistics` enabled
- **List loaded packages**: `M-x package-activated-list`

### Language Server Management (Eglot)
- **Start LSP**: Automatic via hooks for ts-modes and regular modes
- **Code actions**: `C-c l a` (eglot-code-actions)
- **Rename**: `C-c l r` (eglot-rename)
- **Format**: `C-c l f` (eglot-format)
- **Documentation**: `C-c l d` (eldoc)
- **Shutdown server**: `M-x eglot-shutdown`

### Tree-sitter Management
- **Install grammar**: `M-x treesit-install-language-grammar`
- **Check available**: `M-x treesit-language-available-p`
- **List installed**: Check `~/.emacs.d/tree-sitter/` directory
- Auto-installation configured for: bash, cmake, css, elisp, go, html, javascript, json, make, markdown, python, toml, tsx, typescript, yaml, dockerfile, rust

## Essential Keybindings

### macOS Super (⌘) Bindings
- `s-c` / `s-x` / `s-v` / `s-a`: Copy/Cut/Paste/Select-all
- `s-z` / `s-Z`: Undo/Redo (via undo-fu)
- `s-{` / `s-}`: Previous/Next tab
- `s-t` / `s-w`: New tab / Close tab
- `s-/`: Comment (comment-dwim-2)
- `s-\\`: Treemacs toggle
- `s-0`: Treemacs select window

### Navigation
- `C-M-n` / `C-M-p`: Forward/Backward page (Tree-sitter aware)
- `C-M-d` / `C-M-u`: Down/Up list (Tree-sitter navigation)
- `C-x m` / `C-x C-m`: Execute extended command (M-x alternative)
- `C-a`: Smart beginning of line (toggles indentation/start)
- `C-@`: Expand region

### Text Editing
- `M-up` / `M-down`: Move line up/down
- `C-c d`: Duplicate line or region
- `C-c C-k`: Claude Code with context
- `C-x C-y`: Browse kill ring

### Development
- `C-c l a`: Eglot code actions
- `C-c l r`: Eglot rename
- `C-c l f`: Eglot format
- `C-c l d`: Eglot documentation
- `C-c p`: Projectile prefix
- `F5`: Magit status
- `F6`: Magit blame
- `C-\\` or ``` `` ```: VTerm toggle
- `C-c e`: Start Eat terminal
- `C-c C-e`: Start Eat in project root

### Window/Buffer Management
- `M-0`: Treemacs select window
- `C-x t t`: Treemacs toggle
- `C-x t d`: Treemacs select directory
- `C-x 1`: Ace-window (when >2 windows)
- `C-x o`: Other window

### AI Assistants
- `C-c g`: ChatGPT Shell prefix
- `C-c g s`: ChatGPT Shell
- `C-c g d`: ChatGPT describe code
- Claude Shell: Various `C-c c` prefixes

## Key Technologies

### Emacs 30.1 Specific Features
- **Native compilation**: JIT with dedicated cache, comp-speed 3, 8 async jobs
- **Tree-sitter**: 17 languages with auto-installation and mode remapping
- **Eglot**: Built-in LSP with Emacs 30.1 JSON optimizations
- **Project.el**: Built-in project management (used alongside Projectile)
- **Tab Bar**: Native tabs with custom keybindings
- **Pixel scrolling**: Momentum, interpolation, 0.75 factor
- **Enhanced completions**: Historical sorting, category overrides
- **Repeat mode**: Enabled for better command repetition
- **Process optimization**: 4MB read-process-output-max

### Completion Stack (Vertico Ecosystem)
- **Vertico**: Vertical UI with cycling (15 candidates)
- **Orderless**: Multiple matching styles
- **Marginalia**: Annotations for all completion categories
- **Corfu**: Auto-popup with 0.2s delay, 3 char minimum
- **Cape**: Dabbrev, file, keyword backends
- **Embark**: Actions and collect functionality
- **Consult**: ripgrep, buffer, imenu integration

### Development Tools
- **Magit**: Fullscreen mode, verbose refresh
- **VTerm**: Full emulation with smart toggle
- **Deadgrep**: ripgrep with project awareness
- **Flycheck**: Custom cfn-lint checker defined
- **Treemacs**: Nerd icons theme, Git integration, follow-mode

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

### Mode Remappings (Tree-sitter)
Automatic remapping configured via `major-mode-remap-alist`:
- `yaml-mode` → `yaml-ts-mode`
- `bash-mode`, `sh-mode` → `bash-ts-mode`
- `js-mode` → `js-ts-mode`
- `typescript-mode` → `typescript-ts-mode`
- `json-mode` → `json-ts-mode`
- `python-mode` → `python-ts-mode`
- `go-mode` → `go-ts-mode`
- `rust-mode` → `rust-ts-mode`
- `c-mode` → `c-ts-mode`
- `c++-mode` → `c++-ts-mode`
- `dockerfile-mode` → `dockerfile-ts-mode`
- Others: css, html, java, toml, xml

### Important Configuration Details
- **GC during startup**: `most-positive-fixnum` (restored after init)
- **File handler optimization**: Temporarily disabled during startup
- **Frame resizing**: Inhibited during initialization
- **Bidirectional text**: Optimization enabled (`bidi-inhibit-bpa`)
- **Font caching**: Compacting disabled during GC
- **Auto-save**: 20 second timeout, 200 keystroke interval
- **Backup strategy**: Version control enabled, 6 new/2 old versions kept
- **Lock files**: Disabled for performance
- **Completion sorting**: Historical with category overrides

## Commit Protocol

1. **Always update documentation** for:
   - Package additions/removals → Update both README.md and CLAUDE.md
   - Keybinding changes → Update keybindings section
   - Feature additions → Document in appropriate section
   - UI/UX modifications → Update UI packages list
   - Performance changes → Document in optimization sections

2. **Use semantic commit prefixes**:
   - `feat:` New features or packages
   - `fix:` Bug fixes
   - `refactor:` Code restructuring
   - `docs:` Documentation updates
   - `perf:` Performance improvements
   - `chore:` Maintenance tasks

3. **Validation checks**:
   - Run `M-x byte-compile-file` on modified .el files
   - Check `*Warnings*` buffer for native compilation errors
   - Verify package declarations have proper structure
   - Test keybindings with `C-h k` to check conflicts
   - Run `M-x use-package-report` to verify load times

## Environment Setup

### Required Environment Variables
- **`OPENAI_API_KEY`**: For ChatGPT Shell functionality
- **`PATH`**: Must include locations of external tools

### External Dependencies
- **Required**:
  - `git`: Version control and Tree-sitter grammar installation
  - `ripgrep`: For deadgrep and consult-ripgrep
- **Optional but Recommended**:
  - `gcc`, `libgccjit`: Native compilation on macOS
  - `cmake`: For vterm compilation
  - `cfn-lint`: CloudFormation linting
  - `gopls`: Go language server
  - `typescript-language-server`: TypeScript LSP

### Tree-sitter Setup
- Grammars auto-install on first use via git protocol
- Manual installation: `M-x treesit-install-language-grammar`
- Location: `~/.emacs.d/tree-sitter/`

## Troubleshooting Guide

### Startup Issues
- **Slow startup**: Run `M-x emacs-init-time`, check `M-x use-package-report`
- **Package errors**: Delete `~/.emacs.d/elpa/` and restart
- **Native comp warnings**: Check `*Warnings*` buffer, delete `eln-cache/`

### Package Management
- **Straight.el conflicts**: Ensure `straight-use-package-by-default` is `nil`
- **Package not found**: Check archive priorities, refresh with `M-x package-refresh-contents`
- **Version conflicts**: Use `straight-pull-package` for development versions

### LSP/Eglot Issues
- **Server not starting**: Check language server is installed and in PATH
- **Performance problems**: Adjust `eglot-events-buffer-size` to 0
- **Completion not working**: Verify Corfu is loaded, check `eglot-ignored-server-capabilities`

### Tree-sitter Problems
- **Grammar installation fails**: Check git is accessible, try https:// instead of git://
- **Mode not remapping**: Verify grammar installed with `treesit-language-available-p`
- **Syntax highlighting broken**: Reinstall grammar, check `*Messages*` buffer

### Common Fixes
- **Reset configuration**: `rm -rf ~/.emacs.d/elpa ~/.emacs.d/straight ~/.emacs.d/eln-cache`
- **Clear caches**: `M-x straight-normalize-all`
- **Recompile all**: `M-x byte-recompile-directory`
- **Debug loading**: Start with `emacs --debug-init`