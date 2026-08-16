;;; compile-init.el --- Compile this Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --batch -Q -l compile-init.el

;;; Code:

(setq user-emacs-directory
      (file-name-directory (or load-file-name buffer-file-name)))

;; Set up package management to find installed packages
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; Now compile the init file
(byte-compile-file (expand-file-name "init.el" user-emacs-directory))

;;; compile-init.el ends here
