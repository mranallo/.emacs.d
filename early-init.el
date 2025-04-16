;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;; Increase garbage collection thresholds during startup for faster loading
(setq gc-cons-threshold (* 50 1000 1000)
      gc-cons-percentage 0.6)

;; Prevent package.el from automatic package loading; we do it manually in init.el
(setq package-enable-at-startup nil)

;;; Code:

;; (add-to-list 'default-frame-alist '(undecorated-round . t))

(provide 'early-init)
;;; early-init.el ends here
