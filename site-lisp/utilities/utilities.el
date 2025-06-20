;;; package --- Summary
;;; Commentary:
;;; This is a handfull of my utilities
;;; Code:
(defun move-line-up ()
  "Move the current line up by one line.
This function transposes the current line with the line above it.
It maintains the indentation level of the moved line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down by one line.
This function transposes the current line with the line below it.
It maintains the indentation level of the moved line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun smarter-move-beginning-of-line (arg)
  "Move point to the first non-whitespace character on the line.
If point is already at that position, move to the beginning of the line.
This function toggles between the first non-whitespace character
and the beginning of the line.

With prefix argument ARG, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun duplicate-line-or-region (&optional n)
  "Duplicate the current line or active region.
If a region is active, duplicate the region. Otherwise, duplicate the current line.
With prefix argument N, make N copies of the line or region.
If N is negative, comment out the original line and use the absolute value of N."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
		      (buffer-substring (region-beginning) (region-end))
		    (prog1 (thing-at-point 'line)
		      (end-of-line)
		      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
			  (newline))))))
	(dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
	  (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
	(if (> 0 n)                             ;Comment out original with negative arg
	    (comment-region (line-beginning-position) (line-end-position)))
	(forward-line 1)
	(forward-char pos)))))

;; Additional utility functions

(defun reload-init-file ()
  "Reload the Emacs configuration file."
  (interactive)
  (load-file user-init-file)
  (message "Reloaded init file"))

(defun edit-init-file ()
  "Open the Emacs configuration file for editing."
  (interactive)
  (find-file user-init-file))

(defun open-config-dir ()
  "Open the Emacs configuration directory in dired."
  (interactive)
  (dired user-emacs-directory))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
With prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                        (read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text.
With prefix argument REGION, process the entire region instead of just the paragraph."
  (interactive "P")
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: lowercase -> UPPERCASE -> Title Case -> lowercase..."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq pos1 (car bds) pos2 (cdr bds))))
    
    (when (not (eq pos1 pos2))
      (let ((meat (buffer-substring-no-properties pos1 pos2)))
        (cond ((string-equal meat (downcase meat))
               (progn (delete-region pos1 pos2)
                      (insert (upcase meat))))
              ((string-equal meat (upcase meat))
               (progn (delete-region pos1 pos2)
                      (insert (capitalize meat))))
              (t (progn (delete-region pos1 pos2)
                        (insert (downcase meat)))))))))

(defun insert-date ()
  "Insert current date at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun insert-datetime ()
  "Insert current date and time at point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M:%S")))

(defun copy-file-path ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (when buffer-file-name
    (kill-new buffer-file-name)
    (message "Copied path: %s" buffer-file-name)))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (when buffer-file-name
    (kill-new (file-name-nondirectory buffer-file-name))
    (message "Copied filename: %s" (file-name-nondirectory buffer-file-name))))

(defun rename-current-file (new-name)
  "Rename the current file and update the buffer name.
NEW-NAME is the new filename."
  (interactive "sNew name: ")
  (let ((old-name (buffer-file-name)))
    (if old-name
        (progn
          (rename-file old-name new-name 1)
          (set-visited-file-name new-name)
          (rename-buffer new-name)
          (message "File renamed to: %s" new-name))
      (message "Buffer is not visiting a file"))))

(defun delete-current-file ()
  "Delete the current file and kill the buffer."
  (interactive)
  (when buffer-file-name
    (when (y-or-n-p (format "Really delete %s? " buffer-file-name))
      (delete-file buffer-file-name)
      (kill-buffer)
      (message "File deleted"))))

(defun claude-code-with-context ()
  "Start Claude Code with automatic project context.
Generates a comprehensive context message including:
- Current project structure
- Recent git changes
- Open buffers and their purposes
- Current configuration state"
  (interactive)
  (let* ((project-root (or (and (fboundp 'project-current)
                                (project-current)
                                (project-root (project-current)))
                           default-directory))
         (context-buffer "*Claude Context*"))
    
    ;; Generate context in a temporary buffer
    (with-current-buffer (get-buffer-create context-buffer)
      (erase-buffer)
      (insert "# Project Context for Claude Code\n\n")
      
      ;; Current project info
      (insert "## Current Project\n")
      (insert (format "- Root: %s\n" project-root))
      (insert (format "- Current file: %s\n" (or buffer-file-name "No file")))
      (insert (format "- Major mode: %s\n\n" major-mode))
      
      ;; Git status
      (when (file-exists-p (expand-file-name ".git" project-root))
        (insert "## Git Status\n")
        (insert "```\n")
        (let ((default-directory project-root))
          (call-process "git" nil t nil "status" "--porcelain"))
        (insert "```\n\n")
        
        ;; Recent commits
        (insert "## Recent Commits\n")
        (insert "```\n")
        (let ((default-directory project-root))
          (call-process "git" nil t nil "log" "--oneline" "-5"))
        (insert "```\n\n"))
      
      ;; Open buffers in this project
      (insert "## Open Project Files\n")
      (let ((project-files '()))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (and buffer-file-name
                       (string-prefix-p project-root buffer-file-name))
              (push (format "- %s (%s)\n" 
                           (file-name-nondirectory buffer-file-name)
                           major-mode) project-files))))
        (if project-files
            (dolist (file (reverse project-files))
              (insert file))
          (insert "- No project files currently open\n")))
      (insert "\n")
      
      ;; Current Emacs state
      (insert "## Current Emacs State\n")
      (insert (format "- Emacs version: %s\n" emacs-version))
      (insert (format "- Native compilation: %s\n" 
                     (if (featurep 'native-compile) "enabled" "disabled")))
      (insert (format "- Active packages: %d loaded\n" (length package-activated-list)))
      
      ;; Copy context to clipboard and show in popup
      (let ((context-text (buffer-string)))
        (kill-new context-text)
        (pop-to-buffer context-buffer)
        (goto-char (point-min))
        (message "Project context copied to clipboard! Paste it when starting Claude Code.")))
    
    ;; Start Claude Code if available
    (when (fboundp 'claude-code-start)
      (claude-code-start))))

(provide 'utilities)
;;; utilities.el ends here
