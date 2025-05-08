;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; =====================================================================
;;; Basic Setup and Package Management
;;; =====================================================================

;; Set a dedicated native-comp cache directory
(when (featurep 'native-compile)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Declare external function to silence compiler warning
(declare-function straight-use-package "straight" (package &optional no-clone no-build))

;; Define variables before use to avoid free variable warnings
(defvar straight-use-package-by-default nil
  "When non-nil, make `use-package' use straight.el by default.")

;; straight.el package manager bootstrap
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el to use use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default nil)  ;; Don't use straight for all packages by default

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

;; Use GCMH for better GC management optimized for Emacs 30.1
(use-package gcmh
  :ensure t
  :init (gcmh-mode 1)
  :config
  (setq gcmh-idle-delay 1                       ;; Run GC sooner when idle
        gcmh-high-cons-threshold (* 64 1024 1024)  ;; 64MB - increased for modern systems
        gcmh-low-cons-threshold (* 16 1024 1024)   ;; 16MB minimum
        gcmh-verbose nil))

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

;; Enhanced backup strategy optimized for performance
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      remote-file-name-inhibit-locks t  ; Faster remote file operations
      create-lockfiles nil              ; Disable lock files for better performance
      auto-save-default t               ; Keep auto-save enabled
      auto-save-timeout 20              ; Seconds idle before auto-save (increased)
      auto-save-interval 200)           ; Keystrokes before auto-save (increased)

;; delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)

;; Optimized pixel-based scrolling for Emacs 30.1
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-use-momentum t
      pixel-scroll-precision-interpolate-page t  ; Smooth page scrolls
      pixel-scroll-precision-interpolation-factor 0.75
      pixel-scroll-precision-large-scroll-height 40.0
      pixel-scroll-precision-initial-velocity-factor 9.0)

;; Enhanced completions UI optimized for Emacs 30.1
(setq completions-format 'one-column
      completions-detailed t
      completions-max-height 20
      completions-highlight-face 'completions-highlight
      completions-sort 'historical
      completion-category-overrides '((file (styles partial-completion))
                                      (buffer (styles substring)))
      completion-cycle-threshold 3)

;; Enable repeat-mode for better command repetition
(repeat-mode 1)

;; Use the built-in undo system with better defaults for Emacs 30.1
(setq undo-limit 134217728) ; 128mb
(setq undo-strong-limit 201326592) ; 192mb
(setq undo-outer-limit 1610612736) ; 1.5gb

;; Additional performance optimizations for Emacs 30.1
(setq read-process-output-max (* 4 1024 1024)) ; 4mb - Increase read chunk size for process output
(setq auto-mode-case-fold nil)                 ; Speed up file opening by disabling case folding
(setq frame-resize-pixelwise t)                ; Smoother frame resizing

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
 ("C-x m" . execute-extended-command)  ;; Altern to M-x
 ("C-x C-m" . execute-extended-command)  ;; Altern to M-x
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
  "Works just like `progn' but will only evaluate BODY when in terminal.
Otherwise returns nil."
  `(when (is-in-terminal) ,@body))

;; ITERM2 MOUSE SUPPORT
(when-term
 (require 'mouse)
 (xterm-mouse-mode t)
 (defun track-mouse (_e))
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
  ;; Declare variables before use
  (defvar doom-themes-enable-bold t
    "If nil, bold is universally disabled.")
  (defvar doom-themes-enable-italic t
    "If nil, italics is universally disabled.")
  
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)

  ;; Declare variable before use
  (defvar doom-themes-treemacs-theme "nerd-icons"
    "The treemacs theme to use with doom-themes.")
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

;; Standardize on nerd-icons
(use-package nerd-icons
  :config
  ;; If you're missing icons, uncomment this to install them
  ;; (unless (file-exists-p (expand-file-name "icons" nerd-icons-data-dir))
  ;;   (nerd-icons-install-fonts t))
  )

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
  :custom
  (tab-bar-show 1)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-tab-hints t)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  :config
  (tab-bar-mode 1)
  :bind
  (("s-{" . tab-bar-switch-to-prev-tab)
   ("s-}" . tab-bar-switch-to-next-tab)
   ("s-t" . tab-bar-new-tab)
   ("s-w" . tab-bar-close-tab)))

;; Winum - Navigate windows using numbers
(use-package winum)

;;; =====================================================================
;;; Navigation and Completion
;;; =====================================================================

;; Fully commit to Vertico ecosystem (recommended for Emacs 30)
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  (vertico-resize t))

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

;; Save minibuffer history
(use-package savehist
  :ensure nil  ;; built-in
  :init
  (savehist-mode))

;; Track recently opened files
(use-package recentf
  :ensure nil  ;; built-in
  :init
  (recentf-mode)
  :config
  (setq recentf-max-saved-items 50)
  (setq recentf-max-menu-items 15))

;; Corfu - In-buffer completion UI
(use-package corfu
  :ensure t
  :init
  ;; Declare Corfu functions to silence compiler warnings
  (declare-function corfu-next "corfu")
  (declare-function corfu-previous "corfu")
  
  (global-corfu-mode)
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous`
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 2)          ;; Complete with minimum 2 characters
  (corfu-auto-delay 0.0)         ;; No delay for completion
  (corfu-quit-at-boundary 'separator) ;; Automatically quit at word boundary
  (corfu-echo-documentation 0.25)     ;; Show documentation quickly
  (corfu-preview-current 'insert)     ;; Preview current candidate
  (corfu-preselect-first t)           ;; Preselect first candidate
  :config
  ;; TAB-and-Go customizations
  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "TAB") 'corfu-next)
    (define-key corfu-map (kbd "S-TAB") 'corfu-previous)))

;; Cape - Completion At Point Extensions
(use-package cape
  :ensure t
  :init
  ;; Declare Cape functions to silence compiler warnings
  (declare-function cape-file "cape")
  (declare-function cape-dabbrev "cape")
  (declare-function cape-keyword "cape")
  (declare-function cape-wrap-silent "cape")
  (declare-function cape-wrap-noninteractive "cape")
  
  ;; Add useful completion sources
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  :config
  (with-eval-after-load 'cape
    ;; Add cape-keyword after cape is loaded
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    
    ;; Silence the pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    
    ;; Ensure case-sensitivity for file completion
    (advice-add 'comint-completion-at-point :around #'cape-wrap-noninteractive)))


;; Consult - Additional search and navigation commands
(use-package consult
  :init
  ;; Use consult-fd if fd is available, otherwise use consult-find
  (defvar consult-find-command)
  (defvar consult-fd-command)
  (when (executable-find "fd")
    (setq consult-fd-args '("--color=never" "--full-path")))
  
  ;; Configure ripgrep command for consult-ripgrep
  (defvar consult-ripgrep-command)
  (when (executable-find "rg")
    (setq consult-ripgrep-command 
          "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number . -e %s"))
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-x" . consult-mode-command)
   ("C-x C-f" . find-file)  ;; Use standard find-file or consider consult-find
   ("M-y" . consult-yank-pop)
   ("M-s f" . consult-find)  ;; Alternative file finding command
   ("M-s r" . consult-ripgrep)
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
(declare-function treemacs-filewatch-mode "treemacs")
(declare-function treemacs-fringe-indicator-mode "treemacs")
(declare-function treemacs-git-mode "treemacs")
(declare-function treemacs-hide-gitignored-files-mode "treemacs")
(declare-function treemacs-set-scope-type "treemacs-scope")
(declare-function treemacs-load-theme "treemacs-themes")

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

;; Treemacs Icons Dired - DISABLED to prevent double icons with nerd-icons-dired
;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once))

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
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "MANPATH"))
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
    :modes 'yaml-mode)
  (add-to-list 'flycheck-checkers 'cfn-lint))

;; Enhanced Eglot configuration for Emacs 30.1
(use-package eglot
  :ensure nil  ;; built-in
  :hook
  ;; Hook into all tree-sitter modes
  ((go-mode go-ts-mode) . eglot-ensure)
  ((yaml-mode yaml-ts-mode) . eglot-ensure)
  ((dockerfile-mode dockerfile-ts-mode) . eglot-ensure)
  ((js-mode js-ts-mode) . eglot-ensure)
  ((typescript-mode typescript-ts-mode tsx-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  ((c-mode c-ts-mode) . eglot-ensure)
  ((c++-mode c++-ts-mode) . eglot-ensure)
  ((rust-mode rust-ts-mode) . eglot-ensure)
  :config
  ;; Performance optimizations for Emacs 30.1
  (setq eglot-autoshutdown t)
  (setq eglot-sync-connect 1)  ; Improved in Emacs 30.1 with native JSON
  (setq eglot-events-buffer-size 0)
  (setq eglot-extend-to-xref t)
  
  ;; Reduce network traffic and improve performance
  (setq eglot-connect-timeout 30)
  (setq eglot-send-changes-idle-time 0.5)
  
  ;; Disable automatic highlighting to improve performance
  (setq eglot-highlight-symbol-face nil)
  
  ;; Improve code completion
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  
  ;; Language servers configuration
  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode) . ("cfn-lsp-extra")))
  
  ;; Enable snippet expansion with corfu
  (with-eval-after-load 'corfu
    (setq eglot-workspace-configuration 
          `((:yaml . (:format . t))
            (:go . (:usePlaceholders . t))
            (:json . (:format . t))))))
  
  ;; Keybindings for Eglot features
  (bind-keys :map eglot-mode-map
             ("C-c l a" . eglot-code-actions)
             ("C-c l r" . eglot-rename)
             ("C-c l f" . eglot-format)
             ("C-c l d" . eldoc))

;; Enhanced tree-sitter configuration
(use-package treesit
  :ensure nil  ;; built-in
  :config
  ;; Define language sources for auto-installation
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (xml "https://github.com/tree-sitter/tree-sitter-xml")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")))
  
  ;; Auto-install missing tree-sitter grammars when needed
  (dolist (grammar treesit-language-source-alist)
    (unless (treesit-language-available-p (car grammar))
      (message "Installing tree-sitter grammar for %s" (car grammar))
      (treesit-install-language-grammar (car grammar))))
  
  ;; Expanded tree-sitter modes for common languages (Emacs 30.1 optimized)
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
          (js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)
          (go-mode . go-ts-mode)
          (rust-mode . rust-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (java-mode . java-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (html-mode . html-ts-mode)
          (toml-mode . toml-ts-mode)
          (xml-mode . xml-ts-mode)))
  
  ;; Emacs 30.1 tree-sitter navigation enhancements
  (defun ts-setup-navigation ()
    "Set up enhanced tree-sitter navigation."
    (setq-local forward-sexp-function nil)  ; Use improved built-in sexp navigation
    (setq-local treesit-thing-settings 
                '((defun "function_definition" "method_definition" "class_declaration")
                  (sexp "expression" "statement" "declaration")
                  (comment "comment")
                  (string "string" "string_literal" "raw_string_literal"))))
  
  ;; Apply navigation enhancements to all tree-sitter modes
  (dolist (mode '(yaml-ts-mode-hook bash-ts-mode-hook js-ts-mode-hook 
                  typescript-ts-mode-hook json-ts-mode-hook css-ts-mode-hook
                  python-ts-mode-hook go-ts-mode-hook rust-ts-mode-hook
                  c-ts-mode-hook c++-ts-mode-hook java-ts-mode-hook
                  dockerfile-ts-mode-hook))
    (add-hook mode #'ts-setup-navigation))
  
  ;; Configure tree-sitter font-lock and indentation
  (setq treesit-font-lock-level 4)
  
  ;; Mode-specific configurations
  (add-hook 'yaml-ts-mode-hook
            (lambda ()
              (setq-local indent-tabs-mode nil
                          tab-width 2))))
  
  ;; Enable structural navigation with tree-sitter
  (bind-keys*
   ("C-M-n" . treesit-end-of-defun)
   ("C-M-p" . treesit-beginning-of-defun)
   ("C-M-d" . treesit-beginning-of-thing)
   ("C-M-u" . treesit-end-of-thing))

;; Use Project.el instead of Projectile
(use-package project
  :ensure nil  ;; built-in
  :config
  (setq project-switch-commands
        '((project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-dired "Dired")
          (project-eshell "Eshell")
          (project-shell "Shell")
          (project-vterm "VTerm")))
  :bind-keymap
  ("C-c p" . project-prefix-map)
  :bind
  (:map project-prefix-map
        ("v" . project-vterm)))

;; Add VTerm to project.el
(defun project-vterm ()
  "Start vterm in the current project's root directory."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
    (vterm)))

;; Deadgrep - Fast, modern text search using ripgrep
(use-package deadgrep
  :bind ("<f7>" . deadgrep))

;; ChatGPT Shell - Interface to ChatGPT
(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")))

;; Claude Code - Emacs integration for Claude Code CLI
(use-package claude-code
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main"
                   :files ("*.el" (:exclude "demo.gif")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :config
  (claude-code-mode))

;;; =====================================================================
;;; Version Control
;;; =====================================================================

;; Magit - Git interface
(defun magit-status-fullscreen (orig-fun &rest args)
  "Advice to make magit-status run fullscreen."
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(use-package magit
  :bind
  ("<f5>" . magit-status)
  ("<f6>" . magit-blame-addition)
  :config
  (progn
   ;; Make magit status run fullscreen
   (advice-add 'magit-status :around #'magit-status-fullscreen)
  
   (defun magit-quit-session ()
     "Restore the previous window configuration and kill the magit buffer."
     (interactive)
     (kill-buffer)
     (jump-to-register :magit-fullscreen))))

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
		   (window-height . 0.3))))
  :config
  (setq vterm-buffer-name-string "vterm %s")
  ;; Set terminal environment variable to improve compatibility
  ;; Use a more modern terminal type that better supports Unicode and colors
  (setq vterm-term-environment-variable "xterm-direct")
  
  ;; Fix font configuration for vterm
  (custom-set-faces
   '(vterm-color-default ((t (:inherit default :family "DejaVu Sans Mono")))))
  
  ;; Comprehensive vterm setup for proper Unicode and box drawing
  (add-hook 'vterm-mode-hook
            (lambda ()
              ;; Better box drawing with specific font and no line spacing
              (setq-local line-spacing 0)
              )))

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
    (setq popwin:close-popup-window-timer-interval 0.5)
    (setq popwin:popup-window-position 'bottom)
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

;; CloudFormation files - Using yaml-mode for CloudFormation templates
(add-to-list 'auto-mode-alist '("infrastructure/.*\\.yml$" . yaml-mode))

;;; =====================================================================
;;; Custom Settings
;;; =====================================================================

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
