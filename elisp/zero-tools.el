(defun backward-kill-line (arg)
  "Kill chars backward until encountering the end of a line."
  (interactive "p")
  (kill-line 0))

(define-globalized-minor-mode real-global-highlight-parentheses-mode
 highlight-parentheses-mode (lambda ()
                      (if (not (minibufferp (current-buffer)))
			   (highlight-parentheses-mode 1))
                      ))
(real-global-highlight-parentheses-mode 1)

(defun force-backup-of-buffer ()
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)
