;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; Load desktop integration only when the Omarchy package is installed.
(when (file-readable-p "/usr/share/omarchy-emacs/config/omarchy.el")
  (load (expand-file-name "omarchy.el" user-emacs-directory) nil 'nomessage))

;;; =====================================================================
;;; Function Definitions (must come first)
;;; =====================================================================

(eval-and-compile
  (defvar dashboard-vertically-center-content)
  (defvar eglot-events-buffer-config)
  (defvar org-roam-dailies-capture-templates)
  (defvar org-roam-dailies-directory)
  (defvar pixel-scroll-precision-initial-velocity-factor)
  (defvar pixel-scroll-precision-interpolate-page)
  (defvar pixel-scroll-precision-interpolation-factor)
  (defvar pixel-scroll-precision-large-scroll-height)
  (defvar pixel-scroll-precision-use-momentum)
  (defvar treesit-language-source-alist))

(declare-function treesit-ready-p "treesit")
(declare-function dashboard-modify-heading-icons "dashboard-widgets")

(defun is-in-terminal()
  "Will let you know if you are in a terminal session."
    (not (display-graphic-p)))

(defmacro when-term (&rest body)
  "Works just like `progn' but will only evaluate BODY when in terminal.
Otherwise returns nil."
  `(when (is-in-terminal) ,@body))

(defun magit-status-fullscreen (orig-fun &rest args)
  "Advice to make magit-status run fullscreen."
  (window-configuration-to-register :magit-fullscreen)
  (apply orig-fun args)
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun project-vterm ()
  "Start vterm in the current project's root directory."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
    (vterm)))

(defun mr/text-scale-reset ()
  "Restore the current buffer's default text scale."
  (interactive)
  (text-scale-set 0))

(defun mr/yaml-eglot-ensure ()
  "Start the appropriate YAML language server when it is installed."
  (cond
   ((and buffer-file-name
         (string-match-p "/infrastructure/.*\\.ya?ml\\'" buffer-file-name)
         (executable-find "cfn-lsp-extra"))
    (setq-local eglot-server-programs
                (cons `((,major-mode) . ("cfn-lsp-extra"))
                      eglot-server-programs))
    (eglot-ensure))
   ((executable-find "yaml-language-server")
    (eglot-ensure))))

(defun mr/treesit-install-grammars ()
  "Install each missing grammar in `treesit-language-source-alist'."
  (interactive)
  (require 'treesit)
  (dolist (source treesit-language-source-alist)
    (let ((language (car source)))
      (unless (treesit-ready-p language t)
        (condition-case err
            (treesit-install-language-grammar language)
          (error
           (message "Could not install %s grammar: %s"
                    language (error-message-string err)))))))
  (message "Tree-sitter grammar installation finished; restart Emacs to update mode remapping"))

;;; =====================================================================
;;; Basic Setup and Package Management
;;; =====================================================================

;; Use package.el for archives and package-vc through use-package's `:vc'.
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
;; nerd-icons-completion tracks MELPA and requires the matching icon API.
(setq package-pinned-packages '((nerd-icons . "melpa")))
(package-initialize)

;; Assign this before packages have a chance to write Custom settings.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; use-package is built into Emacs 29 and later.
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

;; Enable server for opening file/folder from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Enable a nice launch Dashboard for Emacs
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)

  ;; Set the banner to your custom logo
  (setq dashboard-startup-banner
        (expand-file-name "logo/Nuvola_apps_emacs_vector2.png"
                          user-emacs-directory))


  ;; Content is centered
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)

  ;; Configure dashboard items
  (setq dashboard-items '((recents  . 5)
                          (projects . 8)
                          (bookmarks . 5)))

  ;; Configure dashboard to use project.el instead of projectile
  (setq dashboard-projects-backend 'project-el)

  ;; Display icons
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  ;; Set the icons for dashboard items using the correct function
  (dashboard-modify-heading-icons '((recents   . "nf-oct-history")
                                    (bookmarks . "nf-oct-book")
                                    (projects  . "nf-oct-rocket"))))


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

;; Keep potentially sensitive backups out of the configuration repository.
(let* ((state-home (or (getenv "XDG_STATE_HOME")
                       (expand-file-name "~/.local/state/")))
       (backup-dir (expand-file-name "emacs/backups/" state-home))
       (auto-save-dir (expand-file-name "emacs/auto-save/" state-home)))
  (make-directory backup-dir t)
  (make-directory auto-save-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))))
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

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

;; Emacs 30 display and completion behavior.
(setq image-scaling-factor 'auto
      completion-lazy-hilit t)

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
 ("s-a" . mark-whole-buffer)
 ;; Disable C-mouse-wheel font size changes
 ("C-<wheel-up>" . ignore)
 ("C-<wheel-down>" . ignore)
 ("C-<mouse-4>" . ignore)
 ("C-<mouse-5>" . ignore))

;;; =====================================================================
;;; Terminal Configuration
;;; =====================================================================

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

;; Set initial frame size and position
(when window-system
  (set-frame-size (selected-frame) 200 100))    ; Size: 200 columns, 100 rows

;; Font rendering optimizations for macOS
(when (eq system-type 'darwin)
  (setq ns-use-thin-smoothing t)
  (setq ns-antialias-text t)
  (setq mac-allow-anti-aliasing t)

  ;; Set Window transparency
  (set-frame-parameter nil 'alpha 98)
  (add-to-list 'default-frame-alist '(alpha . 98)))

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
  :defer t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-icon-scale-factor 1.0)
  (doom-modeline-minor-modes nil))

;; Solaire mode - Visually distinguish file-visiting windows from other types of windows
(use-package solaire-mode
  :defer t
  :hook (after-init . solaire-global-mode))

;; Which-key - Display available keybindings in popup
(use-package which-key
  :defer t
  :hook (after-init . which-key-mode))

;; Winner mode - Navigate window configurations with undo/redo
(use-package winner
  :ensure nil  ;; built-in
  :init (winner-mode))

;; Standardize on nerd-icons
(use-package nerd-icons
  :config
  ;; Font installation is intentionally explicit rather than a startup side effect.
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (message "Nerd Icons font missing; run M-x nerd-icons-install-fonts")))

;; Nerd Icons Completion - Show icons in completion UI
(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

;; Nerd Icons Dired - Show icons in dired mode
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Winum - Navigate windows using numbers
(use-package winum
  :defer t
  :hook (after-init . winum-mode))

;;; =====================================================================
;;; Navigation and Completion
;;; =====================================================================

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
  (corfu-auto-prefix 3)          ;; Complete with minimum 3 characters
  (corfu-auto-delay 0.2)         ;; Small delay for completion
  (corfu-separator ?\s)          ;; Use space as separator
  (corfu-quit-at-boundary nil)   ;; Don't quit at boundary
  (corfu-quit-no-match t)        ;; Quit when no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect prompt
  (corfu-popupinfo-delay '(1.0 . 0.5))
  :config
  (corfu-popupinfo-mode 1)
  ;; TAB-and-Go customizations
  (define-key corfu-map (kbd "TAB") #'corfu-next)
  (define-key corfu-map (kbd "S-TAB") #'corfu-previous))

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
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-x" . consult-mode-command)
   ("C-x C-f" . find-file)  ;; Use standard find-file or consider consult-find
   ("M-y" . consult-yank-pop)
   ("M-s f" . consult-find)  ;; Alternative file finding command
   ("C-c r" . consult-ripgrep)
   ("M-p" . consult-git-grep)))

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
(declare-function my/treemacs-setup-font "init")
(declare-function treemacs-filewatch-mode "treemacs")
(declare-function treemacs-follow-mode "treemacs")
(declare-function treemacs-fringe-indicator-mode "treemacs")
(declare-function treemacs-git-mode "treemacs")
(declare-function treemacs-hide-gitignored-files-mode "treemacs")
(declare-function treemacs-set-scope-type "treemacs-scope")
(declare-function treemacs-load-theme "treemacs-themes")

(use-package treemacs
  :ensure t
  :defer t
  :commands (treemacs treemacs-add-and-display-current-project-exclusively)
  :bind (("s-\\" . treemacs))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "s-0") #'treemacs-select-window))
  :config
  (progn
    ;; Treemacs font configuration
    ;; You can customize the font family and size here
    (defun my/treemacs-setup-font ()
      "Configure Treemacs font."
      (setq-local buffer-face-mode-face '(:family "PT Mono" :height 110))
      (buffer-face-mode 1))

    (add-hook 'treemacs-mode-hook #'my/treemacs-setup-font)

    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-follow-mode t)  ;; Follow current file
    (treemacs-project-follow-mode t)  ;; Follow current project

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

;; Treemacs Magit - Integration between Treemacs and Magit
(use-package treemacs-magit
  :after (treemacs magit))

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
  ("C-c d" . duplicate-line-or-region)
  ("C-c C-k" . claude-code-with-context))

;; Whitespace-cleanup-mode - Automatically clean whitespace
(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode))

;; Undo-fu - Enhanced undo/redo functionality
(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

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

;; ;; Deft - Quick note taking and searching
;; (use-package deft
;;   :bind
;;   ("C-c n" . deft)
;;   :config
;;   (setq deft-extensions '("txt"))
;;   (setq deft-directory "/Users/mranallo/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Notes/")
;;   (setq deft-auto-save-interval 0.0))

;; Ligature - Support for programming ligatures
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
  (global-ligature-mode t))

;;; =====================================================================
;;; Development Tools
;;; =====================================================================

;; Exec-path-from-shell - Ensure environment variables in Emacs match the shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "MANPATH"))
  (exec-path-from-shell-initialize))

;; Eglot uses the built-in Flymake diagnostic frontend.
(use-package eglot
  :ensure nil  ;; built-in
  :hook
  ;; Hook into all tree-sitter modes
  ((go-mode go-ts-mode) . eglot-ensure)
  ((yaml-mode yaml-ts-mode) . mr/yaml-eglot-ensure)
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
  (setq eglot-events-buffer-config '(:size 0 :format full))
  (setq eglot-extend-to-xref t)

  ;; Reduce network traffic and improve performance
  (setq eglot-connect-timeout 30)
  (setq eglot-send-changes-idle-time 0.5)

  ;; Improve code completion and performance
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider
          :documentOnTypeFormattingProvider
          :inlayHintProvider))  ; Disable inlay hints for performance

  (setq eglot-workspace-configuration
        '((:yaml . (:format . t))
          (:go . (:usePlaceholders . t))
          (:json . (:format . t))))

  ;; Keybindings for Eglot features
  :bind (:map eglot-mode-map
         ("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)
         ("C-c l d" . eldoc)))

;; Enhanced tree-sitter configuration
(use-package treesit
  :ensure nil  ;; built-in
  :config
  ;; Grammars are installed explicitly with `mr/treesit-install-grammars'.
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (dolist (mapping '((yaml yaml-mode yaml-ts-mode)
                     (bash bash-mode bash-ts-mode)
                     (bash sh-mode bash-ts-mode)
                     (javascript js-mode js-ts-mode)
                     (json js-json-mode json-ts-mode)
                     (typescript typescript-mode typescript-ts-mode)
                     (json json-mode json-ts-mode)
                     (css css-mode css-ts-mode)
                     (python python-mode python-ts-mode)
                     (go go-mode go-ts-mode)
                     (rust rust-mode rust-ts-mode)
                     (c c-mode c-ts-mode)
                     (cpp c++-mode c++-ts-mode)
                     (java java-mode java-ts-mode)
                     (dockerfile dockerfile-mode dockerfile-ts-mode)
                     (html html-mode html-ts-mode)
                     (toml toml-mode toml-ts-mode)))
    (when (treesit-ready-p (nth 0 mapping) t)
      (add-to-list 'major-mode-remap-alist
                   (cons (nth 1 mapping) (nth 2 mapping)))))

  ;; Configure tree-sitter font-lock and indentation
  (setq treesit-font-lock-level 4)

  ;; Mode-specific configurations
  (add-hook 'yaml-ts-mode-hook
            (lambda ()
              (setq-local indent-tabs-mode nil
                          tab-width 2))))

;; Tree-sitter navigation keybindings defined later to avoid conflicts

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
        ("v" . project-vterm))
  ("s-t" . project-find-file))

;; Window and text-scale command maps use Emacs's native repeat support.
(use-package ace-window
  :commands ace-window)

(defvar-keymap mr/window-map
  :doc "Window management commands."
  :repeat (:exit (consult-buffer find-file ace-window))
  "v" #'split-window-right
  "s" #'split-window-below
  "d" #'delete-window
  "o" #'delete-other-windows
  "b" #'consult-buffer
  "f" #'find-file
  "a" #'ace-window
  "h" #'shrink-window-horizontally
  "j" #'enlarge-window
  "k" #'shrink-window
  "l" #'enlarge-window-horizontally)

(defvar-keymap mr/text-scale-map
  :doc "Text scaling commands."
  :repeat t
  "+" #'text-scale-increase
  "-" #'text-scale-decrease
  "0" #'mr/text-scale-reset)

(keymap-global-set "C-c w" mr/window-map)
(keymap-global-set "C-c z" mr/text-scale-map)

;;; =====================================================================
;;; AI Coding
;;; =====================================================================
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c c" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup) ; Optionally enable Emacs MCP tools
  (setq claude-code-ide-show-claude-window-in-ediff t)
  (setq claude-code-ide-focus-claude-after-ediff t)

  ;; Use eat instead of vterm
  ;; (setq claude-code-ide-terminal-backend 'eat)

  (setq claude-code-ide-vterm-anti-flicker t)
  (setq claude-code-ide-vterm-render-delay 0.05))  ; Increase for smoother but less responsive


;;; =====================================================================
;;; Version Control
;;; =====================================================================

(use-package magit
  :bind
  ("<f5>" . magit-status)
  ("<f6>" . magit-blame-addition)
  :custom
  (magit-diff-refine-hunk t)
  :config
  ;; Make magit status run fullscreen
  (advice-add 'magit-status :around #'magit-status-fullscreen))

;;; =====================================================================
;;; Terminal and Shell
;;; =====================================================================

;; VTerm - Fully-featured terminal emulator
(use-package vterm
  :commands vterm
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
  (setq vterm-buffer-name-string "vterm %s"))


;; VTerm Toggle - Quickly toggle terminal window
(use-package vterm-toggle
  :after vterm
  :bind (("C-`" . vterm-toggle))
  :config
  (setq vterm-toggle-fullscreen-p nil))

;;; =====================================================================
;;; Language-specific Modes
;;; =====================================================================

;; Go Mode - Major mode for Go programming language
(use-package go-mode
  :defer t)

;; YAML Mode - Major mode for YAML files
(use-package yaml-mode
  :defer t
  :hook (yaml-mode . display-line-numbers-mode))

;; YAML Pro - Enhanced YAML editing
(use-package yaml-pro
  :defer t)

;; Dockerfile Mode - Major mode for Docker files
(use-package dockerfile-mode
  :defer t)

;; Docker Compose Mode - Major mode for docker-compose files
(use-package docker-compose-mode
  :defer t)

;; CloudFormation files - Using yaml-mode for CloudFormation templates
(add-to-list 'auto-mode-alist '("infrastructure/.*\\.yml$" . yaml-mode))

;;; =====================================================================
;;; Additional Helpful Packages
;;; =====================================================================

;; Helpful - Better help buffers
(use-package helpful
  :defer t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; Rainbow delimiters - Colorize matching parentheses
(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ESUP - Emacs Start Up Profiler
(use-package esup
  :defer t
  :commands esup)

;;; =====================================================================
;;; Org-roam - Networked Note Taking
;;; =====================================================================

;; Org-roam - Build a personal knowledge management system
(use-package org-roam
  :ensure t
  :defer t
  :custom
  ;; Set your notes directory - change this to your preferred location
  (org-roam-directory "~/Documents/org-roam/")

  ;; Database location
  (org-roam-db-location (concat org-roam-directory "org-roam.db"))

  ;; Completion system (uses your existing Vertico setup)
  (org-roam-completion-everywhere t)

  ;; Node display template - shows title and tags
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  :bind
  ;; Essential org-roam keybindings with "C-c n" prefix
  ("C-c n f" . org-roam-node-find)        ; Find or create node
  ("C-c n i" . org-roam-node-insert)      ; Insert link to node
  ("C-c n c" . org-roam-capture)          ; Quick capture
  ("C-c n l" . org-roam-buffer-toggle)    ; Show backlinks buffer
  ("C-c n g" . org-roam-graph)            ; Visualize graph (requires graphviz)

  ;; Daily notes
  ("C-c n j" . org-roam-dailies-capture-today)
  ("C-c n t" . org-roam-dailies-goto-today)
  ("C-c n y" . org-roam-dailies-goto-yesterday)

  :config
  ;; Create org-roam directory if it doesn't exist
  (unless (file-exists-p org-roam-directory)
    (make-directory org-roam-directory t))

  ;; Initialize database
  (org-roam-db-autosync-enable)

  ;; Simple capture templates for beginners
  ;; (setq org-roam-capture-templates
  ;;       '(("d" "default" plain "%?"
  ;;          :if-new (file+head "${slug}.org"
  ;;                             "#+title: ${title}\n#+date: %U\n\n")
  ;;          :unnarrowed t)
  ;;         ("n" "note" plain "%?"
  ;;          :if-new (file+head "notes/${slug}.org"
  ;;                             "#+title: ${title}\n#+filetags: :note:\n#+date: %U\n\n")
  ;;          :unnarrowed t)))

  ;; Daily notes configuration
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n")))))

;;; =====================================================================
;;; Keybindings (Consolidated)
;;; =====================================================================

;; Structural navigation works in both conventional and Tree-sitter modes.
(bind-keys*
 ("C-M-n" . end-of-defun)
 ("C-M-p" . beginning-of-defun)
 ("C-M-d" . down-list)
 ("C-M-u" . backward-up-list))

;;; =====================================================================
;;; Custom Settings
;;; =====================================================================

(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
