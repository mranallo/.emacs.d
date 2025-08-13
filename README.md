# Emacs Configuration

> A high-performance Emacs 30.1 configuration with 80+ packages, native compilation, Tree-sitter support, and AI integration for modern software development.

## 🚀 Quick Start

```bash
# Clone configuration
git clone https://github.com/<your-username>/.emacs.d.git ~/.emacs.d

# Set required environment variables
export OPENAI_API_KEY="sk-..."  # For ChatGPT Shell

# Install required tools (macOS)
brew install ripgrep gcc libgccjit cmake

# Launch Emacs - packages auto-install on first run
emacs
```

## Overview

A meticulously optimized Emacs configuration leveraging Emacs 30.1's cutting-edge features:

### Configuration Architecture
- **`early-init.el`** (68 lines): Startup optimizations, GC tuning, native compilation setup
- **`init.el`** (1050+ lines): Main configuration with 85+ packages in 12 logical sections
- **`custom.el`**: Auto-generated customizations and theme settings
- **`site-lisp/utilities/`**: 14 custom utility functions for enhanced productivity

### Package Management
- **Dual system**: `use-package` with ELPA (primary) + `straight.el` for development packages
- **Smart loading**: All packages use deferred loading for optimal startup time
- **Auto-installation**: Missing packages install automatically on first launch

## Philosophy

- **Performance first**: Optimize GC with GCMH, disable unwanted defaults, leverage native compilation with dedicated cache.
- **Built‑in over external**: Prefer Emacs 30's built-in features (Tab Bar, Tree‑sitter, Eglot, Project.el, Pixel Scrolling).
- **Modularity**: Group related settings and packages logically; keep custom files separate (`custom.el`).
- **Environment‑driven secrets**: Read sensitive keys (e.g. `OPENAI_API_KEY`) from environment variables, not hard‑coded.
- **Minimal boilerplate**: Use sensible defaults and rely on community packages for specialized workflows.

## ✨ Key Features

### 🎯 Completion System (Vertico Ecosystem)
- **Vertico** + **Orderless** + **Marginalia**: Modern minibuffer completion
- **Corfu** + **Cape**: In-buffer auto-completion with multiple backends
- **Embark** + **Consult**: Context actions and advanced search
- **Savehist** + **Recentf**: Persistent history and recent files

### 🛠️ Development Tools
- **Eglot**: Built-in LSP client optimized for Emacs 30.1
- **Tree-sitter**: Native syntax highlighting for 17 languages
- **Magit**: Best-in-class Git interface with fullscreen mode
- **Flycheck**: Real-time syntax checking with custom linters
- **VTerm**: Full terminal emulator with smart toggle

### 🤖 AI Integration
- **ChatGPT Shell**: OpenAI GPT integration
- **GitHub Copilot**: AI pair programming
- **Claude Shell**: Claude API integration
- **Custom Context Function**: Smart project context for Claude Code

### 📁 Project & Navigation
- **Treemacs**: File explorer with Nerd Icons theme
- **Projectile** + **Project.el**: Dual project management
- **Deadgrep**: Lightning-fast ripgrep interface
- **Ace-window** + **Winum**: Efficient window navigation

### 🎨 UI & Aesthetics
- **Doom Themes** + **Doom Modeline**: Modern, beautiful interface
- **Solaire Mode**: Visual buffer distinction
- **Nerd Icons**: Comprehensive icon support
- **Ligatures**: Programming font ligatures
- **Pixel Scrolling**: Smooth scrolling with momentum

### ✏️ Text Editing Power
- **Expand-region**: Smart selection expansion
- **Multiple cursors**: Multi-cursor editing
- **Undo-fu**: Enhanced undo/redo
- **Comment-dwim-2**: Intelligent commenting
- **Rainbow-delimiters**: Colorized parentheses matching
- **Custom utilities**: Move lines, duplicate, smart navigation

### 🔍 Performance & Debugging
- **ESUP**: Startup profiler for optimization
- **Helpful**: Enhanced help buffers with better documentation
- **Lazy loading**: Deferred package loading for <1s startup

## Emacs 30.1 Optimizations

### Performance Enhancements
- **Native Compilation**: JIT with comp-speed 3, 8 async jobs, dedicated cache
- **Startup Time**: <1s with deferred loading and optimized GC
- **Tree-sitter**: 17 languages with auto-installation and retry logic
- **Eglot**: Optimized with ignored capabilities for better performance
- **Process Performance**: 4MB read buffers for LSP and external processes

### Modern Features
- **Long Line Handling**: Optimized for files with extremely long lines
- **Image Rendering**: Auto-scaling for better visual display
- **Lazy Highlighting**: Deferred completion highlighting
- **Pixel Scrolling**: Momentum with 0.75 interpolation factor
- **Enhanced Completions**: Historical sorting with category overrides
- **Tab Bar**: Native tabs with Command key bindings
- **Repeat Mode**: Streamlined command repetition

## Installation

1. **Clone** this repo into your Emacs configuration directory:

   ```bash
   git clone https://github.com/<your-username>/.emacs.d.git ~/.emacs.d
   ```

2. **Set up** environment variables (e.g. for ChatGPT Shell):

   ```bash
   export OPENAI_API_KEY="sk-..."
   ```

3. **Launch** Emacs. On first run, `use-package` will install missing packages automatically.

## 📋 Requirements

### Core Requirements
- **Emacs 30.1** or later (30.0.91+ for most features)
- **Git**: For version control and Tree-sitter grammar installation
- **ripgrep**: For deadgrep and consult-ripgrep functionality

### macOS Dependencies
```bash
# Required for native compilation
brew install gcc libgccjit

# Required for VTerm
brew install cmake

# Optional language servers
brew install gopls                    # Go
npm install -g typescript-language-server  # TypeScript/JavaScript
pip install cfn-lint                  # CloudFormation
```

### Supported Languages
Tree-sitter and LSP support for:
- **Go**, **Python**, **JavaScript/TypeScript**, **Rust**
- **C/C++**, **Java**, **YAML**, **JSON**
- **HTML/CSS**, **Markdown**, **Dockerfile**
- **Bash/Shell**, **TOML**, **XML**, **Make**

## Configuration

- All custom settings are in `init.el`. Separate `custom.el` holds UI customization.
- `early-init.el` optimizes startup and configures native compilation.
- To override or add settings:
  1. Edit `init.el` and add your `use-package` declarations.
  2. Restart Emacs or evaluate your buffer.

## ⌨️ Keybindings

### macOS Super (⌘) Keys
| Key | Function |
|-----|----------|
| `s-c` / `s-x` / `s-v` / `s-a` | Copy / Cut / Paste / Select All |
| `s-z` / `s-Z` | Undo / Redo |
| `s-{` / `s-}` | Previous / Next Tab |
| `s-t` / `s-w` | New Tab / Close Tab |
| `s-/` | Comment (smart) |
| `s-\\` | Toggle Treemacs |

### Development
| Key | Function |
|-----|----------|
| `C-c l a` | LSP Code Actions |
| `C-c l r` | LSP Rename |
| `C-c l f` | LSP Format |
| `C-c l d` | LSP Documentation |
| `F5` | Magit Status |
| `F6` | Magit Blame |
| `C-\\` or `C-`` ` | Toggle Terminal |
| `F7` | Deadgrep Search |

### Navigation & Editing
| Key | Function |
|-----|----------|
| `C-M-n` / `C-M-p` | Next / Previous Function |
| `C-a` | Smart Beginning of Line |
| `C-@` | Expand Region |
| `M-up` / `M-down` | Move Line Up / Down |
| `C-c d` | Duplicate Line/Region |
| `C-x C-y` | Browse Kill Ring |

### AI Integration
| Key | Function |
|-----|----------|
| `C-c C-k` | Claude Code with Context |
| `C-c g s` | ChatGPT Shell |
| `C-c g d` | ChatGPT Describe Code |

### Window Management
| Key | Function |
|-----|----------|
| `M-0` | Focus Treemacs |
| `C-x t t` | Toggle Treemacs |
| `C-x 1` | Ace Window (smart) |

## Customization Tips

- To change your theme, edit the `doom-themes` section in `init.el`.
- To add new packages, declare them with `use-package` and customize via `:config`, `:init`, `:bind`.
- For keybindings, use `bind-keys*` from the `use-package` suite.
- If you want a different Org or Python setup, add your hooks after the built-in declarations.

## 🔧 Troubleshooting

### Common Issues

| Problem | Solution |
|---------|----------|
| Slow startup | Run `M-x esup` for detailed profiling or `M-x emacs-init-time` |
| Package not installing | Delete `~/.emacs.d/elpa/` and restart |
| Native compilation warnings | Check `*Warnings*` buffer, delete `eln-cache/` |
| LSP not working | Ensure language server is installed and in PATH |
| Tree-sitter error | Auto-retry enabled, or run `M-x treesit-install-language-grammar` |
| Icons missing | Run `M-x nerd-icons-install-fonts` |
| Help not enhanced | Run `M-x package-install RET helpful RET` |

### Reset Configuration
```bash
# Complete reset (keeps your config files)
rm -rf ~/.emacs.d/elpa ~/.emacs.d/straight ~/.emacs.d/eln-cache

# Debug startup issues
emacs --debug-init
```

## 📚 Documentation

- **[CLAUDE.md](CLAUDE.md)**: Comprehensive guide for Claude Code AI assistant
- **[GEMINI.md](GEMINI.md)**: Guide for Gemini-based AI tools
- **Custom utilities**: See `site-lisp/utilities/utilities.el` for helper functions

## 🤝 Contributing

This is a personal configuration, but contributions are welcome:

- **Bug reports**: Open an issue with Emacs version and error messages
- **Feature requests**: Suggest improvements via issues
- **Pull requests**: Documentation improvements and bug fixes appreciated

### Development Workflow
1. Fork and clone the repository
2. Test changes with `emacs -Q -l init.el`
3. Update both README.md and CLAUDE.md for significant changes
4. Use semantic commit messages (`feat:`, `fix:`, `docs:`)

## 📈 Recent Improvements (December 2024)

- **Performance**: Startup time reduced from ~2s to <1s with lazy loading
- **Stability**: Removed conflicting packages (popwin, use-package-chords)
- **Enhanced packages**: Better Corfu, Eglot, and Magit configurations
- **New features**: Added Helpful, Rainbow-delimiters, and ESUP
- **Bug fixes**: Fixed vterm font issues and tree-sitter installation
- **Modern Emacs 30.1**: Added long-line optimizations and lazy highlighting

## 📄 License

MIT License - Feel free to use and modify for your own configuration.

---

Enjoy a blazing-fast, modern Emacs experience! 🚀

For questions or support, open an issue on GitHub.
