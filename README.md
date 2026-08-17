# Emacs Configuration

A personal Emacs 30 configuration with optional Omarchy desktop integration,
built-in project and language tooling, and a modern completion UI.

## Highlights

- Vertico, Orderless, Marginalia, Consult, Corfu, and Cape completion stack
- Built-in Project, Eglot, Flymake, Tree-sitter, Savehist, and Recentf
- Magit, Treemacs, VTerm, Org-roam, and Claude Code IDE
- Doom themes/modeline, Nerd Icons, ligatures, and pixel scrolling
- package.el for archive packages and `package-vc` through use-package `:vc`

## Requirements

- Emacs 30 or later
- Git
- ripgrep for `consult-ripgrep`
- CMake and libvterm for VTerm
- A Nerd Font for icons
- Language servers for languages where automatic Eglot startup is desired

On macOS, the core command-line dependencies can be installed with:

```sh
brew install ripgrep cmake
```

Common language servers are installed separately, for example `gopls`,
`typescript-language-server`, `yaml-language-server`, or `cfn-lsp-extra`.

## Installation

### Omarchy

Omarchy users should install its Emacs package first, then clone this repository
to the XDG Emacs configuration directory. The tracked `omarchy.el` shim loads the
package-managed integration, so theme and font changes continue to sync live.

```bash
git clone https://github.com/mranallo/.emacs.d.git ~/.config/emacs
```

The active `~/.config/emacs` directory does not need to be replaced until a
checkout has been tested. Omarchy-specific integration is skipped automatically
on systems where `/usr/share/omarchy-emacs/config/omarchy.el` is absent.

Install `cmake` before first using VTerm so Emacs can compile its native module:

```bash
sudo pacman -S cmake
```

### Other Systems

Clone the repository as `~/.emacs.d` and start Emacs. The Omarchy integration
is not loaded unless `/usr/share/omarchy-emacs/config/omarchy.el` is installed.
Missing archive packages are installed by use-package. Git packages declared
with `:vc`, currently Claude Code IDE, are managed by package-vc.

Package installation is the only expected first-launch network activity. Font
and Tree-sitter grammar installation are explicit operations:

- Run `M-x nerd-icons-install-fonts` if icon glyphs are unavailable.
- Run `M-x mr/treesit-install-grammars` to install missing grammars.
- Restart Emacs after grammar installation so available modes are remapped.

Tree-sitter modes are only enabled when `treesit-ready-p` confirms that the
corresponding grammar can be loaded. Conventional major modes remain the
fallback.

## Package Management

All packages have one owner:

- `use-package` with package.el installs packages from GNU ELPA and MELPA.
- `use-package :vc` installs packages directly from source control.
- straight.el is not used.

Package archives are not refreshed during normal startup or compilation.

## Development Tooling

Eglot is the LSP client and Flymake is the diagnostic frontend. Eglot starts
from configured language hooks. YAML files use `yaml-language-server` when it
is installed; files below an `infrastructure/` directory prefer
`cfn-lsp-extra` when available.

Future backups and auto-save files are kept below
`$XDG_STATE_HOME/emacs/`, or `~/.local/state/emacs/` when `XDG_STATE_HOME` is
unset. Lock files remain enabled to protect against concurrent edits.

## Key Bindings

| Key | Command |
| --- | --- |
| `C-s` | `consult-line` |
| `C-c r` | `consult-ripgrep` |
| `C-x b` | `consult-buffer` |
| `C-c p` | Project command map |
| `s-t` | `project-find-file` |
| `C-c l a/r/f/d` | Eglot actions, rename, format, documentation |
| `F5` / `F6` | Magit status / blame |
| `C-\`` | Toggle VTerm |
| `C-c w` | Window-management command map |
| `C-c z` | Text-scale command map |
| `C-c c` | Claude Code IDE menu |
| `C-c C-k` | Build Claude project context |
| `C-c n ...` | Org-roam commands |

## Validation

Check the configuration without refreshing package archives:

```sh
emacs --batch -Q -l compile-init.el
emacs --debug-init
```

`compile-init.el` derives the repository location dynamically and does not
perform network operations.

## Files

- `early-init.el`: safe startup and native-compilation settings
- `init.el`: package declarations, behavior, and key bindings
- `custom.el`: settings written by Customize
- `compile-init.el`: noninteractive byte-compilation entry point
- `site-lisp/utilities/utilities.el`: personal commands
- `site-lisp/themes/nordic-night-theme.el`: optional custom theme
