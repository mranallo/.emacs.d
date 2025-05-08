;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Increase garbage collection thresholds during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8)

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

;; Prevent font cache resizing during startup (Emacs 30.1 specific)
(setq inhibit-redisplay t
      after-init-time t)
(add-hook 'after-init-hook
          (lambda ()
            (setq inhibit-redisplay nil
                  after-init-time nil)))

;; Faster rendering
(setq bidi-inhibit-bpa t  ; Bidirectional text optimization
      fast-but-imprecise-scrolling t) ; Speed up scrolling operations

;; Native compilation settings optimized for Emacs 30.1
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil ; Don't report errors during async compilation
        native-comp-deferred-compilation t  ; Compile packages in the background
        native-comp-async-jobs-number 8     ; Increased for modern processors
        native-comp-jit-compilation t       ; Enable JIT compilation
        comp-speed 3                        ; Increased optimization level for Emacs 30
        comp-native-driver-options '("-O2") ; C compiler optimization level
        native-comp-driver-options '("-O2" "-mtune=native"))) ; Optimize for your CPU

(provide 'early-init)
;;; early-init.el ends here
