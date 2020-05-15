(package-initialize)

;; MELPA config
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-hook 'emacs-lisp-mode-hook '(lambda ()
  (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
)

;; don't display startup message
(setq inhibit-startup-message t)

;; no toolbar
(tool-bar-mode -1)

;; no menu-bar-mode
(menu-bar-mode -1)

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
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;;;;;;;;;;; Terminal Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;


(defun is-in-terminal()
    (not (display-graphic-p)))

(defmacro when-term (&rest body)
  "Works just like `progn' but will only evaluate expressions in VAR when Emacs is running in a terminal else just nil."
  `(when (is-in-terminal) ,@body))

;; ITERM2 MOUSE SUPPORT
(when-term
 (require 'mouse)
 (xterm-mouse-mode t)
 (defun track-mouse (e))
 (setq mouse-sel-mode t))

;; turn off scroll bar
(if (display-graphic-p) (scroll-bar-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(use-package expand-region
  :ensure t
  :bind
  ("C-@" . er/expand-region))

(use-package utilities
  :load-path "site-lisp/utilities"
  :bind
  ("<M-down>" . move-line-down)
  ("<M-up>" . move-line-up)
  ("C-a" . smarter-move-beginning-of-line)
  ("C-c d" . duplicate-line-or-region))

(use-package vterm
  :ensure t
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

(use-package vterm-toggle
  :ensure t
  :chords
  ("``" . vterm-toggle)
  :config
  (setq vterm-toggle-fullscreen-p nil))

(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

(use-package ido
  :init (progn
          (ido-mode)
          (ido-everywhere))
  :config
  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-default-file-method 'selected-window
        ido-default-buffer-method 'selected-window
        ido-use-faces nil))

(use-package beacon
  :ensure t
  :init
  (beacon-mode +1))

(use-package browse-kill-ring
  :ensure t
  :bind
  ("C-x C-y" . browse-kill-ring))

(use-package deft
  :ensure t
  :bind
  ("C-c n" . deft)
  :config
  (setq deft-extensions '("txt"))
  (setq deft-directory "/Users/mranallo/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Notes/"))

(use-package flyspell
  :ensure t
  :bind
  ("<mouse-3>" . flyspell-correct-word)
  :config
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode)))
  
(use-package magit
  :ensure t
  :bind
  ("<f5>" . magit-status)
  ("<f6>" . magit-blame-addition)
  :config
  (progn
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))))

; Popwin
(use-package popwin
  :ensure t
  :config
  (progn
    (setq popwin:special-display-config nil)
    (push '("*Backtrace*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '("*compilation*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("*Compile-Log*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("*Flycheck errors*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("^\\*docker-build-output:.*\\*$"
            :regexp t :dedicated t :position bottom :stick t :noselect t   :height 0.2  :tail t)
          popwin:special-display-config)
    (push '("*Help*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '("*Shell Command Output*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '("*Warnings*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.2)
          popwin:special-display-config)
    (push '("^\\*Man .*\\*$"
            :regexp t    :position bottom :stick t :noselect nil :height 0.2)
	  popwin:special-display-config)
    (popwin-mode 1))
  :bind-keymap
  ("C-z" . popwin:keymap))

(use-package hydra
  :ensure t)

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)
  (setq company-tooltip-align-annotations 't)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map))
  :bind
  (:map company-active-map
   ("RET" . nil)
   ("<ret>" . nil)
   ("<right>" . company-complete-common)
   ("C-p" . company-select-previous-or-abort)
   ("C-n" . company-select-next-or-abort)
   ("<tab>" . company-complete-selection)
   ("TAB" . company-complete-selection))
  :diminish company-mode)

(use-package company-tabnine
  :ensure t
  :config
  (progn
    (add-to-list 'company-backends #'company-tabnine)))

(use-package deadgrep
  :ensure t
  :bind ("<f7>" . deadgrep ))

(use-package flycheck
  :ensure t
  :defer t
  :init (global-flycheck-mode)
  :config
  (progn
    (flycheck-define-checker cfn-lint
      "A Cloudformation linter using cfn-python-lint.
       See URL 'https://github.com/awslabs/cfn-python-lint'."
      :command ("cfn-lint" "-f" "parseable" source)
      :error-patterns (
		       (warning line-start (file-name) ":" line ":" column
				":" (one-or-more digit) ":" (one-or-more digit) ":"
				(id "W" (one-or-more digit)) ":" (message) line-end)
		       (error line-start (file-name) ":" line ":" column
			      ":" (one-or-more digit) ":" (one-or-more digit) ":"
			      (id "E" (one-or-more digit)) ":" (message) line-end)
		       )
      :modes (cfn-mode)
      )
    (add-to-list 'flycheck-checkers 'cfn-lint)))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

(use-package smex
  :ensure t
  :bind
  (([remap execute-extended-command] . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-x C-m" . smex)))

(use-package whitespace-cleanup-mode
  :ensure t
  :init
  (progn
    (global-whitespace-cleanup-mode t)))

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :bind ("<f8>" . 'neotree-toggle)
  :init
  ;; slow rendering
  (setq inhibit-compacting-font-caches t)

  ;; Every time when the neotree window is opened, let it find current file and jump to node
  (setq neo-smart-open t)

  ;; When running ‘projectile-switch-project’ (C-c p p), ‘neotree’ will change root automatically
  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
  
  ;; show hidden files
  (setq-default neo-show-hidden-files t))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  ;; (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode))

(use-package solaire-mode
  :ensure t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package undo-fu
  :ensure t
  :bind
  ("M-z" . undo-fu-only-undo)
  ("M-Z" . undo-fu-only-redo))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package winner
  :init (winner-mode))

(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . display-line-numbers-mode))

;; cfn-mode
(define-derived-mode cfn-mode yaml-mode
  "Cloudformation"
  "Cloudformation template mode.")
(add-to-list 'auto-mode-alist '("infrastructure/.*\\.yml$" . cfn-mode))

(setq custom-file "~/.emacs.d/custom.elc")
(load custom-file)
