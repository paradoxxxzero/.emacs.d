
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

(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

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

(defun fix-window-horizontal-size (width)
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (enlarge-window (- (or width 80) (window-width)) 'horizontal))
(global-set-key (kbd "<f8>") 'fix-window-horizontal-size)

(defun cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."

  (cond
    (buffer-read-only
      (set-cursor-color "orange")
      (setq cursor-type 'hbar))
    (overwrite-mode
      (set-cursor-color "red")
      (setq cursor-type 'box))
    (t 
      (set-cursor-color "yellow")
      (setq cursor-type '(bar . 3)))))

(add-hook 'post-command-hook 'cursor-according-to-mode)

(defvar blink-cursor-colors hl-paren-colors)
(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Rainbow cursor !!!"
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )

(defun add-after-save-hook (file_re cmd)
  "Run a command on file save if filename match the regexp"
  (add-hook
   'after-save-hook
   `(lambda ()
     (if (and buffer-file-name (numberp (string-match ,file_re buffer-file-name)))
         (call-process-shell-command ,cmd nil 0)))))


(defun urlget (url)
  (let ((buff (url-retrieve-synchronously url)))
    (with-current-buffer buff
      (end-of-buffer)
      (move-beginning-of-line nil)
      (buffer-substring-no-properties (point) (point-max)))))

(defun parseresults (response)
  (cdr
   (split-string (replace-regexp-in-string "\\([\]\"\[]\\)" "" response) ",")))


(defun try-expand-google (old)
  (let ((expansion ()))
    (if (not old)
        (progn
          (he-init-string (he-dabbrev-beg) (point))
          (setq he-expand-list
                (if (not (equal he-search-string ""))
                    (parseresults 
                     (urlget 
                      (concat "http://suggestqueries.google.com/complete/search?client=firefox&hl=fr&q="
                              he-search-string)))))
          (setq he-search-loc2 0)))
    (if (not (equal he-search-string ""))
        (setq expansion (he-dabbrev-kill-search he-search-string)))
    (if (not expansion)
        (progn
          (if old (he-reset-string))
          ())
      (progn
        (he-substitute-string expansion t)
        t))))

(defun swap-buffers () 
  (interactive)
  (let ((w1 (selected-window)) (w2 (next-window)))
    (let ((b1 (window-buffer w1)) (b2 (window-buffer w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (select-window w2))))

(defun clone-buffer ()
  (interactive)
  (set-window-buffer (next-window) (window-buffer (selected-window))))


(defun wrap-region (char1 &optional char2)
  `(lambda (start end)
    (interactive "r")
    (if (use-region-p)
        (progn
          (goto-char start)
          (insert ,char1)
          (goto-char (+ end 1))
          (insert ,(or char2 char1)))
      (insert ,char1))))

(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))
;; (mapc
;;  (lambda (l) (global-set-key (read-kbd-macro (concat "C-" (car l))) (apply 'wrap-region l)))
;;  '(("'") ("\"") ("`") ("(" ")") ("[" "]") ("{" "}") ("<" ">")))

