(require 'sgml-mode)

(defvar jinja2-mode-hook nil)

(defvar jinja2-mode-map
  (let ((jinja2-mode-map (make-keymap)))
    (define-key jinja2-mode-map "\C-j" 'newline-and-indent)
    jinja2-mode-map)
  "Keymap for Jinja2 major mode")

(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . jinja2-mode))

(defvar jinja2-font-lock-keywords nil)
(setq jinja2-font-lock-keywords
   `(
     (,(rx "{{"
	  (* whitespace)
	  (group
	   (*? anything)
	   )
	  (*
	   "|" (* whitespace) (*? anything))
	  (* whitespace)
	  "}}") (1 font-lock-variable-name-face t))
     (,(rx  (group "|" (* whitespace))
	    (group (+ word))
	    )
      (1 font-lock-keyword-face t)
      (2 font-lock-warning-face t))
     (,(rx  (group "|" (* whitespace))
	    (group (or
	     "abs" "attr" "batch" "capitalize"
	     "center" "default" "dictsort"
	     "escape" "filesizeformat" "first"
	     "float" "forceescape" "format"
	     "groupby" "indent" "int" "join"
	     "last" "length" "list" "lower"
	     "pprint" "random" "replace"
	     "reverse" "round" "safe" "slice"
	     "sort" "string" "striptags" "sum"
	     "title" "trim" "truncate" "upper"
	     "urlize" "wordcount" "wordwrap" "xmlattr"
	     ))
	    )
      (1 font-lock-keyword-face t)
      (2 font-lock-function-name-face t)
      )
     (,(rx word-start
	   (? "end")
	   (or
	    "if" "else" "for" "block" "filter" "with"
	    "raw" "macro" "autoescape" "trans" "call"
	    ;; Hydra specific
	    "auth" "showonmatch" "errorproof"
	    )
	   word-end) (0 font-lock-keyword-face))
     (,(rx word-start
	   (or
	    "as" "autoescape" "debug" "extends"
	    "firstof" "in" "include" "load"
	    "now" "regroup" "ssi" "templatetag"
	    "url" "widthratio" "elif" "true"
	    "false" "none" "False" "True" "None"
	    "loop" "super" "caller" "varargs"
	    "kwargs" "break" "continue" "is"
	    "do" "pluralize" "set" "from" "import"
	    "context" "with" "without" "ignore"
	    "missing" "scoped"
	    )
	   word-end) (0 font-lock-builtin-face))
     (,(rx (or "{%" "%}")) (0 font-lock-function-name-face t))
     (,(rx (or "{{" "}}")) (0 font-lock-type-face t))
     (,(rx "{#"
	   (* whitespace)
	   (group
	    (*? anything)
	    )
	   (* whitespace)
	   "#}") (1 font-lock-comment-face t))
     (,(rx (or "{#" "#}")) (0 font-lock-comment-delimiter-face t))
    ))

(defvar jinja2-mode-syntax-table
  (let ((jinja2-mode-syntax-table (make-syntax-table)))
    jinja2-mode-syntax-table)
  "Syntax table for jinja2-mode")

(defun jinja2-mode ()
  "Major mode for editing Jinja2 templates"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table jinja2-mode-syntax-table)
  (use-local-map jinja2-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(jinja2-font-lock-keywords))
  (set (make-local-variable 'comment-start) "{#")
  (set (make-local-variable 'comment-end) "#}")
  (set (make-local-variable 'font-lock-multiline) t)
  (setq major-mode 'jinja2-mode)
  (setq mode-name "Jinja2")
  (run-hooks 'jinja2-mode-hook))

(provide 'jinja2-mode)
