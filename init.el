(require 'package)
(package-initialize)

;;;; Theme
;; Noctilux Theme
(load-theme 'noctilux t)


;;;; Tools
;; Ack And A Half
(global-set-key (kbd "M-à") 'ack-and-a-half)
(global-set-key (kbd "C-à") 'ack-and-a-half-same)

;; Move text
(move-text-default-bindings)

;; Multiple Cursors
;;(require 'multiple-cursors)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "s-i") 'mc/mark-all-like-this)
(global-set-key (kbd "s-I") 'mc/mark-next-like-this)
(global-set-key (kbd "C-s-I") 'mc/mark-previous-like-this)
(global-set-key (kbd "s-TAB") 'mc/edit-lines)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

;; Py Regexp
;;(require 'pyregexp)

;; Zencoding Mode
;;(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(global-set-key (kbd "s-SPC") 'zencoding-expand-line)


;;;; UI
;; Git Gutter
;;(require 'git-gutter)
(global-git-gutter-mode t)

;; Raibow Delimiters
;;(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; Ido Ubiquitous
;;(require 'ido-ubiquitous)
(setq ido-everywhere t)

;; Smex
;;(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Powerline
;;(require 'powerline)
(powerline-default-theme)

;; Pretty-mode
;;(require 'pretty-mode)
(global-pretty-mode 1)


;;;; Modes
;; Coffee Mode

;; Sass Mode

;; Jinja2 Mode

;; Js3 Mode

;;;; Check
;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)


;;;; Built in
;; IDO
(ido-mode 1)
(setq ido-enable-flex-matching t)

;; Tramp
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; Hippie Expand
(global-set-key (kbd "S-M-SPC") 'set-mark-command)
(global-set-key (kbd "M-SPC") 'hippie-expand)

;; Misc
(global-set-key (kbd "C-$") 'comment-or-uncomment-region)
(global-set-key (kbd "<M-dead-circumflex>") 'delete-indentation)
(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab]
                (lambda ()
                  (interactive)
                  (other-window -1)))

(standard-display-ascii ?\t "↹ ")

(global-set-key [C-up] (lambda (arg)
                         (interactive "p")
                         (let ((col (current-column)))
                           (save-excursion
                             (kill-whole-line arg))
                           (previous-line)
                           (move-to-column col))))

(global-set-key [C-down] (lambda (arg)
                           "Duplicate arg lines"
                           (interactive "p")
                           (let (beg end (origin (point)))
                             (if (and mark-active (> (point) (mark)))
                                 (exchange-point-and-mark))
                             (setq beg (line-beginning-position))
                             (if mark-active
                                 (exchange-point-and-mark))
                             (setq end (line-end-position))
                             (let ((region (buffer-substring-no-properties beg end)))
                               (dotimes (i arg)
                                 (goto-char end)
                                 (newline)
                                 (insert region)
                                 (setq end (point)))
                               (goto-char (+ origin (* (length region) arg) arg))))))

(global-set-key (kbd "C-.") (lambda (arg)
                              "Kill arg lines backward."
                              (interactive "p")
                              (kill-line (- 1 arg))))

;; (global-set-key (kbd "<XF86Calculator>") 'psql-on-region-elearning)
;; (global-set-key (kbd "<S-XF86Calculator>") 'psql-on-region-hydra)
;; (global-set-key (kbd "<M-XF86Calculator>") 'psql-on-region-pystil)

(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-by-copying-when-linked t)
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(recentf-max-menu-items 255)
 '(recentf-max-saved-items 255)
 '(recentf-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
