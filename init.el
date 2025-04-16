;;; package --- Summary
;;; Commentary:
;;; Code:

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


;;;;;;;;;;;;;;;;;;;;;;;;;; Terminal Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package comment-dwim-2
  :bind
  ("s-/" . comment-dwim-2))

(use-package expand-region
  :bind
  ("C-@" . er/expand-region))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))

(use-package utilities
  :load-path "site-lisp/utilities"
  :bind
  ("<M-down>" . move-line-down)
  ("<M-up>" . move-line-up)
  ("C-a" . smarter-move-beginning-of-line)
  ("C-c d" . duplicate-line-or-region))

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

(use-package vterm-toggle
  :config
  (setq vterm-toggle-fullscreen-p nil))

(use-package use-package-chords
  :config (key-chord-mode 1)
  (key-chord-define-global "``" 'vterm-toggle))

;; Use built-in tab-bar in Emacs 27+
(use-package tab-bar
  :ensure nil  ;; built-in
  :config
  (tab-bar-mode 1)
  (setq tab-bar-show 1))

;; Replace ido with Vertico/Consult for better completion in Emacs 30
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :bind
  (("C-s" . consult-line)
   ("C-x b" . consult-buffer)
   ("M-y" . consult-yank-pop)
   ("M-s" . consult-ripgrep)
   ("M-p" . consult-git-grep)))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package beacon
  :init
  (beacon-mode +1))

(use-package browse-kill-ring
  :bind
  ("C-x C-y" . browse-kill-ring))

(use-package deft
  :bind
  ("C-c n" . deft)
  :config
  (setq deft-extensions '("txt"))
  (setq deft-directory "/Users/mranallo/Library/Mobile Documents/iCloud~co~noteplan~NotePlan/Documents/Notes/")
  (setq deft-auto-save-interval 0.0))

(use-package flyspell
  :bind
  ("<mouse-3>" . flyspell-correct-word)
  :config
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode)))

(use-package go-mode)

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

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

;; Popwin
(use-package popwin
  :config
  (progn
    (push "*Compile-Log*" popwin:special-display-config)
    (push "*compilation*" popwin:special-display-config)
    (popwin-mode 1))
  :bind-keymap
  ("C-z" . popwin:keymap))

(use-package hydra)

;; Removed ivy/swiper/counsel in favor of Vertico/Consult above

(use-package deadgrep
  :bind ("<f7>" . deadgrep))

(use-package flycheck
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
  :defer t
  :config
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

;; Using built-in completion with Vertico instead of smex
(use-package whitespace-cleanup-mode
  :init
  (progn
    (global-whitespace-cleanup-mode t)))

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

(use-package yaml-pro)

(use-package winum)

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
	("C-x t t"   . treemacs)
	("C-\\"      . treemacs)
	("C-x t d"   . treemacs-select-directory)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar
  :after (treemacs)
  :config (treemacs-set-scope-type 'Tabs))

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

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-icon-scale-factor 1.0)
  (doom-modeline-minor-modes nil))

(use-package solaire-mode
  :config
  (solaire-global-mode +1))

;; Use built-in undo-redo functionality from Emacs 28+
(use-package undo-fu
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

(use-package simpleclip
  :bind
  ("s-c" . simpleclip-copy)
  ("s-x" . simpleclip-cut)
  ("s-v" . simpleclip-paste))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package winner
  :ensure nil  ;; built-in
  :init (winner-mode))

(use-package yaml-mode
  :hook (yaml-mode . display-line-numbers-mode))

;; Using built-in ligature support in Emacs 29+
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

;; Use the new built-in tree-sitter support in Emacs 29+
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

(use-package lsp-mode
  :hook (
	 (go-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred)

(use-package lsp-ui
  :commands lsp-ui-mode)

;; Use consult instead of lsp-ivy
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

;; Removed company-box since we're using completion-preview-mode

;;;;;;;;;;;;;;;;;;;;;;;;;; Mode Configuration ;;;;;;;;;;;;;;;;;;;;;;;;;

;; cfn-mode
(define-derived-mode cfn-mode yaml-mode
  "Cloudformation"
  "Cloudformation template mode.")
(add-to-list 'auto-mode-alist '("infrastructure/.*\\.yml$" . cfn-mode))


(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key (getenv "OPENAI_API_KEY")))

;; Microsoft copilot integration
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))

  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(js-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))


(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs-magit
  :after (treemacs magit))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(provide 'init)
;;; init.el ends here
