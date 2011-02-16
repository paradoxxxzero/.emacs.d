;;; jinja.el ---
;;
;; Author: Mounier Florian based on the work of:
;;         Lennart Borgman (lennart O borgman A gmail O com)
;;         Georg Brandl
;; Created: 2010-02-15
;; Version: 0.1
;; Last-Updated: 2010-02-15
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Simple highlighting and indenting for Jinja for use with mumamo.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(defconst jinja-indenting-keywords
  '("block" "comment" "else" 
    "filter" "for" "if" "spaceless" "with"
    "raw" "macro" "autoescape" "trans" "call"
    ))

(defconst jinja-font-lock-keywords
  (list
   (cons (rx-to-string
          `(and
            word-start
            (or "as" "autoescape" "debug" "extends"
                "firstof" "in" "include" "load" "now" "regroup" "ssi"
                "templatetag" "url" "widthratio"

		"elif" "true" "false" "none" "False" "True" "None" "loop" "super" "caller" "varargs" "kwargs"
		"break" "continue" "is" "do" "pluralize" "set" "from" "import" "context" "with" "without" "ignore" "missing"  
		"scoped" 
                (seq
                 (opt "end")
                 ,(append '(or) jinja-indenting-keywords)
                 ))
          word-end))
         font-lock-keyword-face)
   )
   "Minimal highlighting expressions for Jinja mode")

(defcustom jinja-indent-width 2
  "Indentation width for Jinja."
  :type 'integer
  :group 'jinja)

(defun jinja-indent-line ()
  "Indent current line as Jinja code."
  (save-match-data
    (let* ((indent-re (rx-to-string `(and (* (any " \t"))
                                          ,(append '(or "else") jinja-indenting-keywords))))
           (deindent-re (rx-to-string `(and (* (any " \t"))
                                            (or "else"
                                                (seq
                                                 "end"
                                                 ,(append '(or) jinja-indenting-keywords))))))
           (here (point-marker))
           (this-indentation (current-indentation))
           (this-line-start (progn (beginning-of-line) (point)))
           (prev-line-start (progn (skip-chars-backward " \t\n\r\f")
                                   (beginning-of-line)
                                   (when (< (point) this-line-start)
                                     (point))))
           (prev-indentation (if prev-line-start (current-indentation) 0))
           (shift-in (if (and prev-line-start
                              ;;(re-search-forward indent-re (point-at-eol) t))
                              (looking-at indent-re))
                         jinja-indent-width
                       0))
           (shift-out (progn
                        (goto-char this-line-start)
                        ;;(if (re-search-forward deindent-re (point-at-eol) t)
                        (if (looking-at deindent-re)
                            (- jinja-indent-width) 0)))
           (new-indentation (max 0 (+ prev-indentation shift-in shift-out)))
           )
      (goto-char this-line-start)
      (cond
       ((> new-indentation this-indentation)
        (skip-chars-forward " \t")
        (indent-to new-indentation))
       ((< new-indentation this-indentation)
        (back-to-indentation)
        (delete-region this-line-start (point))
        (indent-to new-indentation)))
      (goto-char here))))

;;;###autoload
(define-derived-mode jinja-mode nil "Jinja"
  "Simple Jinja mode for use with mumamo.
This mode only provides syntax highlighting."
  (modify-syntax-entry ?_ "w")
  (set (make-local-variable 'indent-line-function) 'jinja-indent-line)
  (setq font-lock-defaults '(jinja-font-lock-keywords)))

;;; Comments mode
;; (defconst jinja-comment-font-lock-keywords
;;   (list
;;    (cons "\\(.*\\)" (list 1 font-lock-comment-face))
;;    ))

;; (defvar jinja-comment-font-lock-defaults
;;   '(jinja-comment-font-lock-keywords t t))

;; (define-derived-mode jinja-comment-mode nil "Jinja comment"
;;   "For jinja comment blocks."
;;   (set (make-local-variable 'font-lock-defaults) jinja-comment-font-lock-defaults))

;;; Variables mode

(defconst jinja-variable-font-lock-keywords
  (list
   ;; Built in filters:
   (cons (rx
          "|"
          (submatch
           (or "abs" "attr" "batch" "capitalize" "center" "default" "dictsort" 
	       "escape" "filesizeformat" "first" "float" "forceescape" "format" 
	       "groupby" "indent" "int" "join" "last" "length" "list" "lower" 
	       "pprint" "random" "replace" "reverse" "round" "safe" "slice" 
	       "sort" "string" "striptags" "sum" "title" "trim" "truncate" 
	       "upper" "urlize" "wordcount" "wordwrap" "xmlattr"
	       )))
         (list 1 font-lock-builtin-face))
   (cons (rx
          "|"
          (submatch
           (0+ (any "a-z"))))
         (list 1 font-lock-function-name-face))
   (cons "\\([^|]*\\)" (list 1 font-lock-variable-name-face))
   ))

(defvar jinja-variable-font-lock-defaults
  '(jinja-variable-font-lock-keywords
    t t
    ;; This still gives teh syntax symbol to |, why?
    ((?| . ". "))
    ))

;;;###autoload
(define-derived-mode jinja-variable-mode nil "Jinja variable"
  "For jinja comment blocks."
  ;;(modify-syntax-entry ?| ?.)
  (set (make-local-variable 'font-lock-defaults) jinja-variable-font-lock-defaults))

(provide 'jinja)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jinja.el ends here
