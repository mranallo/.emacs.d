<!-- README for Emacs configuration repository -->

# Emacs Configuration

> A modular, performant, and modern Emacs setup designed to leverage Emacs 29/30 features, native compilation, and community-driven packages for an enhanced editing experience.

## Overview

This repository contains my personal Emacs configuration, split across two files:

- `early-init.el`: Startup optimizations (Garbage Collection tuning, package loading control).
- `init.el`: Core configuration (package management, UI tweaks, keybindings, modes).

Everything is managed with [`use-package`](https://github.com/jwiegley/use-package), ensuring clean, lazy-loaded declarations.

## Philosophy

- **Performance first**: Increase GC threshold during startup, disable unwanted defaults, enable native compilation on supported systems.
- **Built‑in over external**: Prefer Emacs’s shipped features (Tab Bar, Tree‑sitter, Undo/Redo, Ligatures) when possible.
- **Modularity**: Group related settings and packages logically; keep custom files separate (`custom.el`).
- **Environment‑driven secrets**: Read sensitive keys (e.g. `OPENAI_API_KEY`) from environment variables, not hard‑coded.
- **Minimal boilerplate**: Use sensible defaults and rely on community packages for specialized workflows.

## Highlights

- **Completion**: Ivy, Counsel (for M-x and file finding), Consult, Marginalia, Embark
- **Project & File Navigation**: Treemacs with Nerd Icons, Winum, Projectile integration
- **UI & Aesthetics**: Doom Themes, Doom Modeline, Solaire Mode, Ligatures, Nerd Icons
- **Language Support**: LSP Mode + UI + Treemacs, Tree‑sitter, YAML, Go, Docker, CloudFormation
- **Productivity Tools**: Magit, Flycheck, Deadgrep, VTerm + VTerm‑Toggle, ChatGPT Shell
- **Editing Enhancements**: Expand‑Region, Comment‑DWIM‑2, Duplicate Line, Whitespace Cleanup, Undo‑Fu, Simpleclip
- **Core Tweaks**: Better `yes-or-no` answers, no backup files, server mode, macOS Super keybindings

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

- Emacs 29 or newer (30 recommended for improved native compilation support).
- Optional Homebrew packages on macOS for native compilation:
  - `gcc`, `libgccjit`
- `git`, `ripgrep` for Consult/Deadgrep, and project searching.

## Configuration

- All custom settings are in `init.el`. Separate `custom.el` holds UI customization.
- `early-init.el` tweaks GC and disables auto package loading.
- To override or add settings:
  1. Edit `init.el` and add your `use-package` declarations.
  2. Restart Emacs or evaluate your buffer.

## Keybindings

- **Super (⌘)** on macOS is mapped to the `Super` modifier:
  - `s-c`, `s-x`, `s-v` for Copy/Cut/Paste
  - `s-z` / `s-Z` for Undo/Redo
- **Counsel** for `M-x` and `C-x C-f` (find-file)
- **Consult/Embark** via `C-.`, `C-;`, `C-s`, `C-x b`, etc.
- **VTerm** toggle with ```` (double backtick chord) or `C-\\`.
- **Magit** on `<f5>` (status), `<f6>` (blame)
- **Treemacs** on `C-x t t`, `s-\\`, `M-0`, and other intuitive combos

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