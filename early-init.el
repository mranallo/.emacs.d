;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Reduce startup GC without disabling it entirely.
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Prevent package.el from automatic package loading; we do it manually in init.el
(setq package-enable-at-startup nil)

;; Disable UI elements early to prevent momentary display
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(undecorated-round . nil) default-frame-alist)

;; Frame parameters optimized for Emacs 30.1
(push '(inhibit-double-buffering . t) default-frame-alist)
(push '(scroll-bar-adjust-thumb-portion . nil) default-frame-alist)

;; Prevent frame resizing during initialization
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Faster rendering
(setq bidi-inhibit-bpa t  ; Bidirectional text optimization
      fast-but-imprecise-scrolling t) ; Speed up scrolling operations

;; Keep native-compiled files with the rest of this configuration's cache.
(when (featurep 'native-compile)
  (startup-redirect-eln-cache
   (expand-file-name "eln-cache/" user-emacs-directory))
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t))

;; Additional optimizations for Emacs 30.1
(setq read-process-output-max (* 4 1024 1024) ; 4MB
      auto-mode-case-fold nil              ; Don't ignore case when matching auto-mode-alist
      inhibit-compacting-font-caches t)    ; Don't compact font caches during GC

(provide 'early-init)
;;; early-init.el ends here
