;;; omarchy.el --- Omarchy Emacs integration shim -*- lexical-binding: t -*-

(let* ((omarchy--system-dir "/usr/share/omarchy-emacs/config")
       (omarchy--system-file (expand-file-name "omarchy.el" omarchy--system-dir)))
  (when (file-exists-p omarchy--system-file)
    ;; Keep isolated checkouts usable before Omarchy copies its managed theme.
    (add-to-list 'custom-theme-load-path
                 (expand-file-name "themes" omarchy--system-dir))
    (load omarchy--system-file nil 'nomessage)))

;;; omarchy.el ends here
