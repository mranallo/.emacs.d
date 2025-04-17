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
