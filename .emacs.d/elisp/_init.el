(load  "~/.emacs.d/elisp/basic-edit-toolkit.el")
(load  "~/.emacs.d/elisp/highlight-parentheses/highlight-parentheses.el")
 (load  "~/.emacs.d/elisp/zero-tools.el")
;; (load  "~/.emacs.d/elisp/mode-line.e;; l")
;; (load  "~/.__jabber.el")
;; (require 'jabber-autoloads)
;; (setq jabber-account-list
      ;; `(
        ;; ("fmounier@jabber.kozea.fr" (:network-server . "jabber.keleos.fr") (:password . ,kjabber))
        ;; ("paradoxxx.zero@gmail.com" (:network-server . "talk.google.com") (:password . ,gtalkjabber) (:connection-type . ssl))
        ;; ))

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
(autoload 'jinja2-mode "jinja2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jinja2$" . jinja2-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/yaml-mode/")
(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/python-mode/")
(autoload 'python-mode "python" nil t)
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/haml-mode/")
(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/scss-mode/")
(autoload 'scss-mode "scss-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/sass-mode/")
(autoload 'sass-mode "sass-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(add-to-list 'load-path "~/.emacs.d/elisp/less-css-mode/")
(autoload 'less-css-mode "less-css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

(autoload 'cython-mode "cython-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxd\\'" . cython-mode))
(add-to-list 'auto-mode-alist '("\\.pxi\\'" . cython-mode))

;; (add-to-list 'load-path "~/.emacs.d/elisp/undo-tree")
;; (require 'undo-tree)

(add-to-list 'load-path "~/.emacs.d/elisp/powerline")
(require 'powerline)

;; (add-to-list 'load-path "~/.emacs.d/elisp/anything-config")
;; (require 'anything-config)

(add-to-list 'load-path "~/.emacs.d/elisp/flymake")
(require 'flymake)
(defun flymake-python-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-copy))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/pycheckers" (list local-file))))
(add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-python-init))
(add-hook 'python-mode-hook 'flymake-find-file-hook)
(add-hook 'css-mode-hook 'flymake-find-file-hook)
(add-to-list 'load-path "~/.emacs.d/elisp/flymake-cursor")
(require 'flymake-cursor)


(add-to-list 'load-path "~/.emacs.d/elisp/full-ack")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(add-to-list 'load-path "~/.emacs.d/elisp/pretty-mode")
(require 'pretty-mode)
(global-pretty-mode)



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
        try-expand-google
        try-expand-google-spelling))

;; Ido
(autoload 'ido-everywhere "ido")
(ido-everywhere 1)
(ido-mode 1)

(setq ido-enable-flex-matching t) ;; enable fuzzy matching

;; (add-to-list 'load-path "~/.emacs.d/elisp/pylookup")
;; (setq pylookup-dir "~/.emacs.d/elisp/pylookup")
;; (add-to-list 'load-path pylookup-dir)

;; (setq-default frame-title-format (list "%b - Emacs"))
;; load pylookup when compile time
;; (eval-when-compile (require 'pylookup))

;; set executable file and db file
;; (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
;; (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
;; (autoload 'pylookup-lookup "pylookup"
  ;; "Lookup SEARCH-TERM in the Python HTML indexes." t)

;; (autoload 'pylookup-update "pylookup" 
  ;; "Run pylookup-update and create the database at `pylookup-db-file'." t)

(standard-display-ascii ?\t "↹    ")

;; Tramp remote sudo: /sudo:root@host[#port]:/path/to/file
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))



(add-after-save-hook "kozea/pygal/.*\.py$" "cd ~/kozea/pygal/; ~/.envs/pygal/bin/python ./demo/simple_test.py")
(add-after-save-hook "kozea/sitenco/projects/pygal/" "wsreload --url 'http://pygal.l/*'")
(add-after-save-hook "kozea/hydra" "wsreload --url 'http://*.pharminfo.fr.zero:5000/*'")
(add-after-save-hook "kozea/hydra" "wsreload --url 'http://*.groupinfo.fr.zero:5001/*'")
(add-after-save-hook "kozea/labocube" "wsreload --url 'http://localhost:3795/*'")
(add-after-save-hook "kozea/elearning" "wsreload --url 'http://manager.l:5999/*'")
(add-after-save-hook "kozea/elearning" "wsreload --url 'http://student.l:5111/*'")
(add-after-save-hook "kozea/pygal/demo/moulinrouge" "wsreload --url 'http://moulinrouge.l:21112/*'")
(add-after-save-hook "kozea/pygal/demo/cabaret" "wsreload --url 'http://cabaret.l:12221/*'")
(add-after-save-hook "_/umlaut" "wsreload --url 'file:///home/zero/_/umlaut/index.html'")
(add-after-save-hook "_/phy" "wsreload --url 'file:///home/zero/_/phy/index.html'")
(add-after-save-hook "_/gol" "wsreload --url 'file:///home/zero/_/gol/index.html'")
(add-after-save-hook "_/coffee2d" "wsreload --url 'file:///home/zero/_/coffee2d/index.html'")

(add-to-list 'load-path "~/.emacs.d/elisp/pyregexp") ;; if the files are not already in the load path
(require 'pyregexp)
;;(global-set-key (kbd "M-à") 'pyregexp-replace)


(add-to-list 'load-path "~/.emacs.d/elisp/multiple-cursors")
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-M-m") 'mc/mark-more-like-this-extended) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "<f6>") 'mark-previous-like-this)
(global-set-key (kbd "<f7>") 'mark-next-like-this)
(global-set-key (kbd "<f8>") 'mc/edit-lines)

(add-to-list 'load-path "~/.emacs.d/elisp/expand-region")
(require 'expand-region)
(global-set-key (kbd "C- ") 'er/expand-region)

;; Keys
(global-set-key (kbd "M-DEL") 'kill-word)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "s-SPC") 'python-completion-complete-at-point)
(global-set-key (kbd "M-SPC") 'hippie-expand)
(global-set-key (kbd "M-RET") 'flymake-display-err-menu-for-current-line)
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f11] 'flymake-goto-next-error)

(global-set-key (kbd "S-M-SPC") 'set-mark-command)
(global-set-key (kbd "C-$") 'comment-or-uncomment-region+)
(global-set-key (kbd "C-.") 'backward-kill-line)
(global-set-key (kbd "C-à") 'ack-same)
(global-set-key (kbd "s-<tab>") 'swap-buffers)
(global-set-key (kbd "C-s-<tab>") 'clone-buffer)

(global-set-key [mouse-7] 'next-buffer)
(global-set-key [mouse-6] 'previous-buffer)
(global-set-key [S-mouse-8] 'other-window)
(global-set-key [S-mouse-9]
                (lambda ()
                  (interactive)
                  (other-window -1)))
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab]
                (lambda ()
                  (interactive)
                  (other-window -1)))

(global-set-key [C-S-t]
                (lambda ()
                  (interactive)
                  (save-excursion
                    (left-char)
                    (transpose-key))))

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

(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<up>") 'shrink-window)
(global-set-key (kbd "s-C-<down>") 'enlarge-window)

(global-set-key (kbd "<XF86Calculator>") 'flymake-start-syntax-check)
(global-set-key (kbd "<XF86Mail>") 'pylookup-lookup)

(global-set-key (kbd "C-%") 'goto-match-paren)
(global-set-key (kbd "<f12>") 'shell-command-on-region)
(global-set-key [S-mouse-1] 'shift-mouse-select)
(global-set-key [S-down-mouse-1] 'ignore)

;; (global-set-key (kbd "C-x u") 'undo-tree-undo)
;; (global-set-key (kbd "C-x y") 'undo-tree-redo)
;; (global-set-key (kbd "C-x C-u") 'undo-tree-visualize)
;; (global-set-key (kbd "<f6>") 'undo-tree-save-state-to-register)
;; (global-set-key (kbd "<f9>") 'undo-tree-restore-state-from-register)

(server-start)
;; (jabber-connect-all)
