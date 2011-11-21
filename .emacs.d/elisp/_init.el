(load  "~/.emacs.d/elisp/basic-edit-toolkit.el")
(load  "~/.emacs.d/elisp/highlight-parentheses/highlight-parentheses.el")
(load  "~/.emacs.d/elisp/zero-tools.el")

(require 'jabber-autoloads)
(setq jabber-account-list
      '(
        ("fmounier@jabber.kozea.fr" (:network-server . "jabber.keleos.fr"))
        ("paradoxxx.zero@gmail.com" (:network-server . "talk.google.com") (:connection-type . ssl))
        ))

;; Mode config
(setq jinja2-user-keywords
  '(
    "auth" "showonmatch" "errorproof"))

(setq jinja2-user-functions
  '(
    "date_format" "money_format"
    "money_format_no_currency" "sublength"
    "json" "percent_format" "person_title"
    "mail_format" "sort_by" "split"))

;; Autoloads
(add-to-list 'load-path "~/.emacs.d/elisp/lua-mode/")
(autoload 'lua-mode "lua-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/js2-mode/")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/coffee-mode/")
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/jinja2-mode/")
(autoload 'jinja2-mode "jinja2" nil t)
(add-to-list 'auto-mode-alist '("\\.jinja2$" . jinja2-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/yaml-mode/")
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(add-to-list 'auto-mode-alist '("\\.scss$" . css-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/minimap/")
(require 'minimap)

(add-to-list 'load-path "~/.emacs.d/elisp/mingus/")
(require 'mingus)

(add-to-list 'load-path "~/.emacs.d/elisp/emacs-for-python/extensions/")
(autoload 'python-mode "python" nil t)
(autoload 'cython-mode "cython-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.python$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; Hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Ido
(autoload 'ido-everywhere "ido")
(ido-everywhere 1)
(ido-mode 1)

(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; Flymake
(when (load-file "~/.emacs.d/elisp/emacs-for-python/extensions/flymake-patch.el")
  (setq flymake-info-line-regex
        (append flymake-info-line-regex '("unused$" "^redefinition" "used$")))
  (defun flymake-create-temp-intemp (file-name prefix)
    "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
    (unless (stringp file-name)
      (error "Invalid file-name"))
    (or prefix
        (setq prefix "flymake"))
    (let* ((name (concat
                  (file-name-nondirectory
                   (file-name-sans-extension file-name))
                  "_" prefix))
           (ext  (concat "." (file-name-extension file-name)))
           (temp-name (make-temp-file name nil ext))
           )
      (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name))
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-intemp)))
      (list "~/.emacs.d/pycheckers" (list temp-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init))
  (load-library "flymake-cursor"))

(eval-after-load 'python
  (add-hook 'python-mode-hook 'flymake-find-file-hook))


(standard-display-ascii ?\t "↹    ")


;; Keys
(global-set-key (kbd "M-DEL") 'kill-word)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "s-SPC") 'dabbrev-expand)
(global-set-key (kbd "M-SPC") 'hippie-expand)
(global-set-key (kbd "M-RET") 'flymake-display-err-menu-for-current-line)
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

(global-set-key (kbd "S-M-SPC") 'set-mark-command)
(global-set-key (kbd "C-$") 'comment-or-uncomment-region+)
(global-set-key (kbd "C-.") 'backward-kill-line)

(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab]
                (lambda ()
                  (interactive)
                  (other-window -1)))

(global-set-key [C-up] 'duplicate-line-or-region-above)
(global-set-key [C-down] 'duplicate-line-or-region-below)
(global-set-key [s-up] 'backward-to-indentation)
(global-set-key [s-down] 'forward-to-indentation)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key [M-left] 'backward-word)
(global-set-key [M-right] 'forward-word)
(global-set-key [C-S-up] 'backward-paragraph)
(global-set-key [C-S-down] 'forward-paragraph)
(global-set-key [M-S-up] 'backward-paragraph)
(global-set-key [M-S-down] 'forward-paragraph)

(windmove-default-keybindings '(meta))
(global-set-key [(meta super left)] 'windmove-left)
(global-set-key [(meta super right)] 'windmove-right)
(global-set-key [(meta super up)] 'windmove-up)
(global-set-key [(meta super down)] 'windmove-down)
