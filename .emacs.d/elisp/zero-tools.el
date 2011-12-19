(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

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

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (progn
    (recentf-mode t)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting"))))

(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(set-frame-parameter (selected-frame) 'alpha '(80 80))
(add-to-list 'default-frame-alist '(alpha 80 80))

;; Search At Point
(require 'etags) ;; provides `find-tag-default' in Emacs 21.
  
(defun isearch-yank-regexp (regexp)
  "Pull REGEXP into search regexp." 
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol (&optional partialp)
  "Put symbol at current point into search string.

If PARTIALP is non-nil, find all partial matches."
  (interactive "P")
  (let* ((sym (find-tag-default))
      ;; Use call of `re-search-forward' by `find-tag-default' to
      ;; retrieve the end point of the symbol.
      (sym-end (match-end 0))
      (sym-start (- sym-end (length sym))))
    (if (null sym)
     (message "No symbol at point")
   (goto-char sym-start)
   ;; For consistent behavior, restart Isearch from starting point
   ;; (or end point if using `isearch-backward') of symbol.
   (isearch-search)
   (if partialp
       (isearch-yank-string sym)
     (isearch-yank-regexp
      (concat "\\_<" (regexp-quote sym) "\\_>"))))))

(defun isearch-current-symbol (&optional partialp)
  "Incremental search forward with symbol under point.

Prefixed with \\[universal-argument] will find all partial
matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-forward-regexp nil 1)
    (isearch-yank-symbol partialp)))

(defun isearch-backward-current-symbol (&optional partialp)
  "Incremental search backward with symbol under point.

Prefixed with \\[universal-argument] will find all partial
matches."
  (interactive "P")
  (let ((start (point)))
    (isearch-backward-regexp nil 1)
    (isearch-yank-symbol partialp)))

(global-set-key (kbd "C-é") 'isearch-current-symbol)
(global-set-key (kbd "C-è") 'isearch-backward-current-symbol)

;; Subsequent hitting of the keys will increment to the next
;; match--duplicating `C-s' and `C-r', respectively.
(define-key isearch-mode-map (kbd "C-é") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-è") 'isearch-repeat-backward)
