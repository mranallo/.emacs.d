;;; package --- Summary
;;; Commentary:
;;; Code:

;;; =====================================================================
;;; Basic Setup and Package Management
;;; =====================================================================

;; Native compilation support - Emacs 30 has improved native compilation on macOS
;; This workaround is likely not needed for Emacs 30 but keeping modified version
;; to avoid potential issues. Remove if native compilation works without it.
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setenv "LIBRARY_PATH"
	  (string-join
	   '("/opt/homebrew/opt/gcc/lib/gcc/current"
	     "/opt/homebrew/opt/libgccjit/lib/gcc/current"
	     "/opt/homebrew/opt/gcc/lib/gcc/current/gcc/aarch64-apple-darwin24/14")
	   ":")))

;; Package management setup
;; Prevent package.el from auto-initializing; we handle it manually
(setq package-enable-at-startup nil)

(require 'package)
;; Setup package archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
;; Prioritize archives: GNU > MELPA Stable > MELPA
(setq package-archive-priorities
      '(("gnu" . 10)
        ("melpa-stable" . 5)
        ("melpa" . 0)))
(package-initialize)

;; Use built-in use-package for cleaner package configurations
(require 'use-package)
(eval-and-compile
 ;; Silence native-compiler warnings for external doom-themes functions
 (declare-function doom-themes-treemacs-config "doom-themes" nil)
 (declare-function doom-themes-org-config     "doom-themes" nil))
(setq use-package-always-ensure t
      use-package-verbose t
      use-package-compute-statistics t
      use-package-expand-minimally t
      use-package-minimum-reported-time 0.2)

;; Byte compile lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'emacs-lisp-byte-compile nil 'local)))

;; Enable server for opening file/folder from emacsclient
(server-start)

;;; =====================================================================
;;; General Emacs Settings
;;; =====================================================================

;; don't display startup message
(setq inhibit-startup-message t)

;; remove text from titlebar
(setq frame-title-format nil)

;; no toolbar
(tool-bar-mode -1)

;; no menu-bar-mode
;; (menu-bar-mode -1)

;; disable backup files (foo~)
(setq backup-inhibited t)

;; delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)

;; use default Mac browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; use line numbers in programming modes
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; highlight current line
(global-hl-line-mode t)

;; pick up changes to files on disk automatically (ie, after git pull)
(global-auto-revert-mode 1)

;; Make yes-or-no questions answerable with 'y' or 'n'
(setq use-short-answers t)  ;; Preferred in Emacs 28+ over fset yes-or-no-p

;; macOS specific key bindings
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; key bindings
(bind-keys*
 ("C-M-n" . forward-page)
 ("C-M-p" . backward-page)
 ;; macOS-style: Command for copy/cut/paste/select-all
 ("s-c" . kill-ring-save)
 ("s-x" . kill-region)
 ("s-v" . yank)
 ("s-a" . mark-whole-buffer))

;;; =====================================================================
;;; Terminal Configuration
;;; =====================================================================

(defun is-in-terminal()
  "Will let you know if you are in a terminal session."
    (not (display-graphic-p)))

(defmacro when-term (&rest body)
  "Works just like `progn' but will only evaluate expressions in VAR when Emacs is running in a terminal else just nil."
  `(when (is-in-terminal) ,@body))

;; ITERM2 MOUSE SUPPORT
(when-term
 (require 'mouse)
 (xterm-mouse-mode t)
 (defun track-mouse (e))
 (global-set-key [mouse-4] 'scroll-down-line)
 (global-set-key [mouse-5] 'scroll-up-line))

;; turn off scroll bar
(if (display-graphic-p) (scroll-bar-mode -1))

;;; =====================================================================
;;; UI/UX Enhancements
;;; =====================================================================

;; Beacon - Highlight cursor position when scrolling
(use-package beacon
  :init
  (beacon-mode +1))

;; Doom themes - A collection of modern themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)

  (setq doom-themes-treemacs-theme "nerd-icons")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)

  ;; Set a dark titlebar
  (set-frame-parameter nil 'ns-appearance 'dark)
  (set-frame-parameter nil 'ns-transparent-titlebar nil))

;; Doom modeline - A fancy and fast mode-line
(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-icon-scale-factor 1.0)
  (doom-modeline-minor-modes nil))

;; Solaire mode - Visually distinguish file-visiting windows from other types of windows
(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; Which-key - Display available keybindings in popup
(use-package which-key
  :config
  (which-key-mode +1))

;; Winner mode - Navigate window configurations with undo/redo
(use-package winner
  :ensure nil  ;; built-in
  :init (winner-mode))

;; Nerd Icons - Vector icons for Emacs
(use-package nerd-icons)

;; Nerd Icons Completion - Show icons in completion UI
(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

;; Nerd Icons Dired - Show icons in dired mode
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Tab Bar - Built-in tab bar in Emacs 27+
(use-package tab-bar
  :ensure nil  ;; built-in
  :config
  (tab-bar-mode 1)
  (setq tab-bar-show 1))

;; Winum - Navigate windows using numbers
(use-package winum)

;;; =====================================================================
;;; Navigation and Completion
;;; =====================================================================

;; Vertico - Vertical completion UI
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

;; Orderless - Flexible completion style
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia - Annotations for minibuffer completions
(use-package marginalia
  :init
  (marginalia-mode))

;; Consult - Search and navigation commands
(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-s" . consult-ripgrep)
   ("M-p" . consult-git-grep)))

;; Embark - Context-aware actions
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

;; Embark Consult - Integration between Embark and Consult
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Avy - Jump to visible text using a char-based decision tree
(use-package avy
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g g" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("C-c C-j" . avy-resume))
  :config
  (setq avy-background t)
  (setq avy-style 'at-full))

;; Treemacs - Tree layout file explorer
(use-package treemacs
  :defer t
  :bind (("s-\\" . treemacs))  ;; Command-\\ to toggle file tree
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "s-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay        0.5
	  treemacs-directory-name-transformer      #'identity
	  treemacs-display-in-side-window          t
	  treemacs-eldoc-display                   'simple
	  treemacs-file-event-delay                2000
	  treemacs-file-extension-regex            treemacs-last-period-regex-value
	  treemacs-file-follow-delay               0.2
	  treemacs-file-name-transformer           #'identity
	  treemacs-follow-after-init               t
	  treemacs-expand-after-init               t
	  treemacs-find-workspace-method           'find-for-file-or-pick-first
	  treemacs-git-command-pipe                ""
	  treemacs-goto-tag-strategy               'refetch-index
	  treemacs-header-scroll-indicators        '(nil . "^^^^^^")
	  treemacs-hide-dot-git-directory          t
	  treemacs-indentation                     2
	  treemacs-indentation-string              " "
	  treemacs-is-never-other-window           nil
	  treemacs-max-git-entries                 5000
	  treemacs-missing-project-action          'ask
	  treemacs-move-forward-on-expand          nil
	  treemacs-no-png-images                   nil
	  treemacs-no-delete-other-windows         t
	  treemacs-project-follow-cleanup          t
	  treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position                        'left
	  treemacs-read-string-input               'from-child-frame
	  treemacs-recenter-distance               0.1
	  treemacs-recenter-after-file-follow      nil
	  treemacs-recenter-after-tag-follow       nil
	  treemacs-recenter-after-project-jump     'always
	  treemacs-recenter-after-project-expand   'on-distance
	  treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
	  treemacs-show-cursor                     nil
	  treemacs-show-hidden-files               t
	  treemacs-silent-filewatch                nil
	  treemacs-silent-refresh                  nil
	  treemacs-sorting                         'alphabetic-asc
	  treemacs-select-when-already-in-treemacs 'move-back
	  treemacs-space-between-root-nodes        t
	  treemacs-tag-follow-cleanup              t
	  treemacs-tag-follow-delay                1.5
	  treemacs-text-scale                      nil
	  treemacs-user-mode-line-format           nil
	  treemacs-user-header-line-format         nil
	  treemacs-wide-toggle-width               70
	  treemacs-width                           35
	  treemacs-width-increment                 1
	  treemacs-width-is-initially-locked       t
	  treemacs-workspace-switch-cleanup        nil)

    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs-toggle)
	("C-\\"      . treemacs)
	("C-x t d"   . treemacs-select-directory)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

;; Treemacs Projectile - Integration between Treemacs and Projectile
(use-package treemacs-projectile
  :after (treemacs projectile))

;; Treemacs Icons Dired - Show treemacs icons in dired
(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

;; Treemacs Magit - Integration between Treemacs and Magit
(use-package treemacs-magit
  :after (treemacs magit))

;; Treemacs Persp - Integration between Treemacs and Perspective
(use-package treemacs-persp
  :after (treemacs persp-mode)
  :config (treemacs-set-scope-type 'Perspectives))

;; Treemacs Tab Bar - Integration between Treemacs and Tab Bar
(use-package treemacs-tab-bar
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

;; Treemacs Nerd Icons - Use nerd icons in Treemacs
(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config
  ;; Load the nerd-icons theme for Treemacs after Treemacs and nerd-icons are available
  (treemacs-load-theme "nerd-icons"))

;;; =====================================================================
;;; Text Editing and Formatting
;;; =====================================================================

;; Comment-dwim-2 - Enhanced commenting commands
(use-package comment-dwim-2
  :bind
  ("s-/" . comment-dwim-2))

;; Expand-region - Increase selected region by semantic units
(use-package expand-region
  :bind
  ("C-@" . er/expand-region))

;; Utilities - Custom utility functions
(use-package utilities
  :load-path "site-lisp/utilities"
  :bind
  ("<M-down>" . move-line-down)
  ("<M-up>" . move-line-up)
  ("C-a" . smarter-move-beginning-of-line)
  ("C-c d" . duplicate-line-or-region))

;; Whitespace-cleanup-mode - Automatically clean whitespace
(use-package whitespace-cleanup-mode
  :init
  (progn
    (global-whitespace-cleanup-mode t)))

;; Undo-fu - Enhanced undo/redo functionality
(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

;; Simpleclip - Simplified clipboard handling
(use-package simpleclip
  :bind
  ("s-c" . simpleclip-copy)
  ("s-x" . simpleclip-cut)
  ("s-v" . simpleclip-paste))

;; Browse-kill-ring - Browse and insert items from kill ring
(use-package browse-kill-ring
  :bind
  ("C-x C-y" . browse-kill-ring))

;; Flyspell - Spell checking
(use-package flyspell
  :bind
  ("<mouse-3>" . flyspell-correct-word)
  :config
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode)))

;; Persistent-scratch - Save scratch buffer between sessions
(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;; Deft - Quick note taking and searching
(use-package deft
  :bind
  ("C-c n" . deft)
  :config
  (setq deft-extensions '("txt"))
  (setq deft-directory "/Users/mranallo/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Notes/")
  (setq deft-auto-save-interval 0.0))

;; Ligature - Support for programming ligatures
(when (fboundp 'global-ligature-mode)
  (use-package ligature
    :config
    ;; Enable the "www" ligature in every possible major mode
    (ligature-set-ligatures 't '("www"))
    ;; Enable traditional ligature support in eww-mode, if the
    ;; `variable-pitch' face supports it
    (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
    ;; Enable all Cascadia Code ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                         ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                         "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                         "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                         "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                         "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                         "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                         "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                         ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                         "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                         "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                         "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\\\" "://"))
    ;; Enables ligature checks globally in all buffers
    (global-ligature-mode t)))

;;; =====================================================================
;;; Development Tools
;;; =====================================================================

;; Exec-path-from-shell - Ensure environment variables in Emacs match the shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

;; Flycheck - Syntax checking
(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

;; Flycheck-pos-tip - Show flycheck errors in tooltip
(use-package flycheck-pos-tip
  :defer t
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

;; Define custom CloudFormation linter once Flycheck is loaded
(with-eval-after-load 'flycheck
  (flycheck-define-checker cfn-lint
    "A CloudFormation linter using cfn-python-lint.
See URL 'https://github.com/aws-cloudformation/cfn-lint'."
    :command ("cfn-lint" "-f" "parseable" source)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column
              ":" (one-or-more digit) ":" (one-or-more digit) ":"
              (id "W" (one-or-more digit)) ":" (message) line-end)
     (error line-start (file-name) ":" line ":" column
            ":" (one-or-more digit) ":" (one-or-more digit) ":"
            (id "E" (one-or-more digit)) ":" (message) line-end))
    :modes cfn-mode)
  (add-to-list 'flycheck-checkers 'cfn-lint))

;; LSP Mode - Language Server Protocol support
(use-package lsp-mode
  :hook (
	 (go-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

;; LSP UI - UI enhancements for LSP Mode
(use-package lsp-ui
  :commands lsp-ui-mode)

;; LSP Treemacs - Integration between LSP and Treemacs
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; Tree-sitter - Incremental parsing system
(when (fboundp 'global-tree-sitter-mode)
  (use-package tree-sitter
    :ensure nil  ;; built-in
    :hook
    ((go-mode . tree-sitter-mode)
     (js-mode . tree-sitter-mode)
     (typescript-mode . tree-sitter-mode)
     (python-mode . tree-sitter-mode)
     (ruby-mode . tree-sitter-mode)
     (rust-mode . tree-sitter-mode))
    :config
    (global-tree-sitter-mode)))

;; Hydra - Make Emacs bindings that stick around
(use-package hydra)

;; Deadgrep - Fast, modern text search using ripgrep
(use-package deadgrep
  :bind ("<f7>" . deadgrep))

;; ChatGPT Shell - Interface to ChatGPT
(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")))

;;; =====================================================================
;;; Version Control
;;; =====================================================================

;; Magit - Git interface
(use-package magit
  :bind
  ("<f5>" . magit-status)
  ("<f6>" . magit-blame-addition)
  :config
  (progn
 (defadvice magit-status (around magit-fullscreen activate)
  "Set Magit to run fullscreen."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows)))
  
(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen)))

;;; =====================================================================
;;; Terminal and Shell
;;; =====================================================================

;; VTerm - Fully-featured terminal emulator
(use-package vterm
  :init
  (progn
    (add-to-list 'display-buffer-alist
		 '((lambda(bufname _) (with-current-buffer bufname (equal major-mode 'vterm-mode)))
		   (display-buffer-reuse-window display-buffer-at-bottom)
		   ;;(display-buffer-reuse-window display-buffer-in-direction)
		   ;;display-buffer-in-direction/direction/dedicated is added in emacs27
		   ;;(direction . bottom)
		   ;;(dedicated . t) ;dedicated is supported in emacs27
		   (reusable-frames . visible)
		   (window-height . 0.3)))))

;; VTerm Toggle - Quickly toggle terminal window
(use-package vterm-toggle
  :config
  (setq vterm-toggle-fullscreen-p nil))

;; Use-package-chords - Key-chord integration for use-package
(use-package use-package-chords
  :config (key-chord-mode 1)
  (key-chord-define-global "``" 'vterm-toggle))

;;; =====================================================================
;;; Window and Buffer Management
;;; =====================================================================

;; Popwin - Popup window manager
(use-package popwin
  :config
  (progn
    (push "*Compile-Log*" popwin:special-display-config)
    (push "*compilation*" popwin:special-display-config)
    (popwin-mode 1))
  :bind-keymap
  ("C-z" . popwin:keymap))

;;; =====================================================================
;;; Language-specific Modes
;;; =====================================================================

;; Go Mode - Major mode for Go programming language
(use-package go-mode)

;; YAML Mode - Major mode for YAML files
(use-package yaml-mode
  :hook (yaml-mode . display-line-numbers-mode))

;; YAML Pro - Enhanced YAML editing
(use-package yaml-pro)

;; Dockerfile Mode - Major mode for Docker files
(use-package dockerfile-mode)

;; Docker Compose Mode - Major mode for docker-compose files
(use-package docker-compose-mode)

;; cfn-mode - CloudFormation template mode
(define-derived-mode cfn-mode yaml-mode
  "Cloudformation"
  "Cloudformation template mode.")
(add-to-list 'auto-mode-alist '("infrastructure/.*\\.yml$" . cfn-mode))

;;; =====================================================================
;;; Custom Settings
;;; =====================================================================

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
