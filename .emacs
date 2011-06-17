(load  "~/.emacs.d/elisp/basic-edit-toolkit.el")
(load  "~/.emacs.d/elisp/coffee-mode/coffee-mode.el")
(load  "~/.emacs.d/elisp/highlight-parentheses/highlight-parentheses.el")
(load  "~/.emacs.d/elisp/emacs-for-python/epy-init.el")
(load  "~/.emacs.d/elisp/new-python-mode/python.el")
(load  "~/.emacs.d/elisp/ipython.el")
(load  "~/.emacs.d/elisp/rfringe/rfringe.el")

(standard-display-ascii ?\t "↹    ")

(epy-setup-checker "pycheckers %f")

(setq jinja2-user-keywords
  '(
    "auth" "showonmatch" "errorproof"))

(setq jinja2-user-functions
  '(
    "date_format" "money_format"
    "money_format_no_currency" "sublength"
    "json" "percent_format" "person_title"
    "mail_format" "sort_by" "split"))

(load  "~/.emacs.d/elisp/zero-tools.el")
(load  "~/.emacs.d/elisp/jinja2-mode/jinja2.el")
(load  "~/.emacs.d/elisp/multi-web-mode/multi-web-mode.el")

(setq mweb-default-major-mode 'jinja2-mode)
(setq mweb-tags '(
                  ;; (php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js2-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions ' ("html" "htm" "ctp" "phtml" "php" "php4" "php5", "jinja2"))
(multi-web-global-mode 1)

;; Keys
(global-set-key (kbd "M-SPC") 'dabbrev-expand)
(global-set-key (kbd "M-RET") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "S-M-SPC") 'set-mark-command)
(global-set-key (kbd "C-$") 'comment-or-uncomment-region+)
(global-set-key (kbd "C-.") 'backward-kill-line)
;; (global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab]
                (lambda ()
                  (interactive)
                  (other-window -1)))

(define-key ac-mode-map (kbd "C-SPC") 'auto-complete)

(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key [C-up] 'duplicate-line-or-region-above)
(global-set-key [C-down] 'duplicate-line-or-region-below)
(global-set-key [M-S-up] 'backward-paragraph)
(global-set-key [M-S-down] 'forward-paragraph)

(set-frame-parameter (selected-frame) 'alpha '(85 70))
(add-to-list 'default-frame-alist '(alpha 85 70))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosave/" t))))
 '(backup-by-copying t)
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))))
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(delete-old-versions t)
 '(hl-paren-colors (quote ("orange1" "yellow1" "greenyellow" "green1" "springgreen1" "cyan1" "slateblue1" "magenta1" "purple" "orange1" "yellow1" "greenyellow" "green1" "springgreen1" "cyan1" "slateblue1" "magenta1" "purple")))
 '(home-end-enable t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(menu-bar-mode nil)
 '(org-log-done (quote time))
 '(remote-shell-program "zsh")
 '(require-final-newline t)
 '(rst-level-face-base-color "black")
 '(safe-local-variable-values (quote ((py-indent-offset . 4) (Mode . Python) (js2-basic-offset . 4))))
 '(scroll-bar-mode nil)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(vc-follow-symlinks t)
 '(vc-make-backup-files t)
 '(version-control t)
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#eeeeee" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "monofur"))))
 '(ac-candidate-face ((t (:background "black" :foreground "dark orange"))))
 '(ac-completion-face ((t (:foreground "yellow"))))
 '(ac-selection-face ((t (:background "black" :foreground "red"))))
 '(button ((t (:box (:line-width 1 :color "violet red" :style released-button)))))
 '(custom-button-pressed-unraised ((t (:foreground "violet" :box (:line-width 1 :color "DodgerBlue1" :style pressed-button)))))
 '(custom-button-unraised ((t (:box (:line-width 1 :color "DodgerBlue1" :style released-button)))))
 '(diff-file-header ((t (:foreground "DarkSlateGray3" :weight bold))))
 '(diff-header ((t (:foreground "DarkSlateGray1"))))
 '(flymake-errline ((t (:underline "red" :weight bold))))
 '(flymake-infoline ((t (:underline "forest green"))))
 '(flymake-warnline ((t (:underline "goldenrod"))))
 '(font-lock-builtin-face ((t (:foreground "SpringGreen2"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "magenta"))))
 '(font-lock-comment-face ((t (:italic t :foreground "#9933cc"))))
 '(font-lock-constant-face ((t (:foreground "SeaGreen3"))))
 '(font-lock-doc-face ((t (:foreground "LightSalmon"))))
 '(font-lock-function-name-face ((t (:foreground "#ffcc00"))))
 '(font-lock-keyword-face ((t (:foreground "#ff6600"))))
 '(font-lock-preprocessor-face ((t (:foreground "#aaffff"))))
 '(font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
 '(font-lock-string-face ((t (:foreground "#66FF00"))))
 '(font-lock-type-face ((t (:foreground "DodgerBlue2"))))
 '(font-lock-variable-name-face ((t (:foreground "deep pink"))))
 '(font-lock-warning-face ((t (:bold t :foreground "Pink"))))
 '(fringe ((t (:background "#222222"))))
 '(highlight ((t (:background "#101010"))))
 '(minibuffer-prompt ((t (:bold t :foreground "#ff6600"))))
 '(mode-line ((t (:foreground "#cccccc" :background "#222222" :box nil))))
 '(mode-line-buffer-id ((t (:foreground "#eeeeee" :background "#191919" :box nil))))
 '(mode-line-inactive ((t (:foreground "#a4a4a4" :background "#222222" :box nil))))
 '(modeline-mousable ((t (:background "#444444" :foreground "black"))))
 '(modeline-mousable-minor-mode ((t (:background "#444444" :foreground "black"))))
 '(mumamo-border-face-in ((t (:inherit font-lock-preprocessor-face :foreground "orange red" :weight bold))))
 '(mumamo-border-face-out ((t (:inherit font-lock-preprocessor-face :foreground "dark orange" :weight bold))))
 '(primary-selection ((t (:background "#101010"))))
 '(region ((t (:background "#191919"))))
 '(rst-level-1-face ((t (:foreground "DeepPink2"))) t)
 '(rst-level-2-face ((t (:foreground "PaleVioletRed3"))) t)
 '(rst-level-3-face ((t (:foreground "DarkOrchid2"))) t)
 '(secondary-selection ((t (:background "#090909"))))
 '(zmacs-region ((t (:background "#161616")))))

(server-start)
