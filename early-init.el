;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Increase garbage collection thresholds during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Prevent package.el from automatic package loading; we do it manually in init.el
(setq package-enable-at-startup nil)

;; Disable UI elements early to prevent momentary display
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)

;; Prevent frame resizing during initialization
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Native compilation settings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-async-jobs-number 4 ; Adjust based on your CPU cores
        comp-speed 2)) ; Balance between compile speed and optimization

(provide 'early-init)
;;; early-init.el ends here
