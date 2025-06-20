# Emacs Configuration

> A modular, performant, and modern Emacs setup designed to leverage Emacs 30 features, native compilation, and community-driven packages for an enhanced editing experience.

## Overview

This repository contains my personal Emacs configuration, split across three files:

- `early-init.el`: Startup optimizations (Garbage Collection tuning, UI element disabling, native compilation settings).
- `init.el`: Core configuration (package management, UI tweaks, keybindings, modes).
- `custom.el`: User interface customizations and theme settings.

Everything is managed with [`use-package`](https://github.com/jwiegley/use-package), ensuring clean, lazy-loaded declarations.

## Philosophy

- **Performance first**: Optimize GC with GCMH, disable unwanted defaults, leverage native compilation with dedicated cache.
- **Built‑in over external**: Prefer Emacs 30's built-in features (Tab Bar, Tree‑sitter, Eglot, Project.el, Pixel Scrolling).
- **Modularity**: Group related settings and packages logically; keep custom files separate (`custom.el`).
- **Environment‑driven secrets**: Read sensitive keys (e.g. `OPENAI_API_KEY`) from environment variables, not hard‑coded.
- **Minimal boilerplate**: Use sensible defaults and rely on community packages for specialized workflows.

## Highlights

- **Completion**: Vertico, Orderless, Marginalia, Embark, Corfu (in-buffer completion), Cape
- **Project & File Navigation**: Project.el, Treemacs with Nerd Icons, Winum
- **UI & Aesthetics**: Doom Themes, Doom Modeline, Solaire Mode, Ligatures, Nerd Icons, Pixel Scrolling
- **Language Support**: Eglot (built-in LSP client), Native Tree‑sitter (Emacs 30), YAML, Go, Docker, CloudFormation
- **Productivity Tools**: Magit, Flycheck, Deadgrep, VTerm + VTerm‑Toggle, ChatGPT Shell, Claude Code + Context Integration
- **Editing Enhancements**: Expand‑Region, Comment‑DWIM‑2, Duplicate Line, Whitespace Cleanup, Enhanced Undo, Simpleclip
- **Core Tweaks**: Better `yes-or-no` answers, robust backup strategy, server mode, macOS Super keybindings

## Emacs 30.1 Optimizations

- **Native Compilation**: Optimized for performance with higher compilation levels and JIT compilation
- **Tree-sitter**: Expanded language support with improved structural navigation and "thing-at-point" integration
- **Eglot**: Enhanced LSP client optimized for Emacs 30.1's native JSON improvements
- **Project.el**: Built-in project management replacing Projectile
- **Pixel Scrolling**: Advanced smooth scrolling with momentum, interpolation, and page continuity
- **Enhanced Completions UI**: Improved minibuffer completion with historical sorting and category-specific styles
- **Tab Bar**: Customized with intuitive keybindings and visual enhancements
- **Repeat Mode**: Better command repetition with streamlined interface
- **Robust Backup Strategy**: Performance-tuned backup with optimized auto-save settings
- **Memory Management**: GCMH with higher thresholds optimized for modern systems
- **Startup Optimization**: Reduced font cache rebuilding and improved initial rendering
- **Process Performance**: Larger read buffers for better LSP and external process communication

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

## Requirements

- Emacs 30 for all features (some may work with Emacs 29).
- Optional Homebrew packages on macOS for native compilation:
  - `gcc`, `libgccjit`
- `git`, `ripgrep` for searching and project management.

## Configuration

- All custom settings are in `init.el`. Separate `custom.el` holds UI customization.
- `early-init.el` optimizes startup and configures native compilation.
- To override or add settings:
  1. Edit `init.el` and add your `use-package` declarations.
  2. Restart Emacs or evaluate your buffer.

## Keybindings

- **Super (⌘)** on macOS is mapped to the `Super` modifier:
  - `s-c`, `s-x`, `s-v` for Copy/Cut/Paste
  - `s-z` / `s-Z` for Undo/Redo
  - `s-{`, `s-}` for tab navigation
  - `s-t`, `s-w` for new tab and close tab
- **Tree-sitter Navigation**:
  - `C-M-n`, `C-M-p` for function/defun navigation
  - `C-M-d`, `C-M-u` for thing-at-point navigation
- **Eglot**:
  - `C-c l a` for code actions
  - `C-c l r` for rename
  - `C-c l f` for format
  - `C-c l d` for documentation
- **Vertico/Consult/Embark** for enhanced minibuffer completion
- **VTerm** toggle with ```` (double backtick chord) or `C-\\`.
- **Claude Code** via `C-c c` prefix (e.g., `C-c c c` to start, `C-c c t` to toggle)
  - `C-c C-k` for Claude Code with automatic project context
- **Magit** on `<f5>` (status), `<f6>` (blame)
- **Treemacs** on `C-x t t`, `s-\\`, `M-0`, and other intuitive combos
- **Project.el** commands via `C-c p` prefix

## Customization Tips

- To change your theme, edit the `doom-themes` section in `init.el`.
- To add new packages, declare them with `use-package` and customize via `:config`, `:init`, `:bind`.
- For keybindings, use `bind-keys*` from the `use-package` suite.
- If you want a different Org or Python setup, add your hooks after the built-in declarations.

## Contributing

This is my personal configuration; pull requests are welcome but likely to be opinionated. Feel free to:

- Open an issue if you find a bug or have a feature request.
- Submit a PR for documentation fixes or enhancements.

Enjoy a faster, cleaner Emacs experience! 🚀
