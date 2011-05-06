;; (defvar jinja2-keywords
;;   '(
;;     "if" "else" "for"
;;     "block" "filter" "with"
;;     "raw" "macro" "autoescape" "trans" "call"
;;     ;; Hydra specific
;;     "auth" "showonmatch" "errorproof"
;;     )
;;   "Jinja2 keywords.")

;; (defvar jinja2-builtins
;;   '(
;;     "as" "autoescape" "debug" "extends"
;;     "firstof" "in" "include" "load" "now" "regroup" "ssi"
;;     "templatetag" "url" "widthratio"
;;     "elif" "true" "false" "none" "False" "True" "None" "loop" "super" "caller" "varargs" "kwargs"
;;     "break" "continue" "is" "do" "pluralize" "set"
;;     "from" "import" "context" "with" "without" "ignore" "missing"
;;     "scoped"
;;     )
;;   "Jinja2 builtins.")

;; (defvar jinja2-functions
;;   '(
;;     "abs" "attr" "batch" "capitalize" "center" "default" "dictsort"
;;     "escape" "filesizeformat" "first" "float" "forceescape" "format"
;;     "groupby" "indent" "int" "join" "last" "length" "list" "lower"
;;     "pprint" "random" "replace" "reverse" "round" "safe" "slice"
;;     "sort" "string" "striptags" "sum" "title" "trim" "truncate"
;;     "upper" "urlize" "wordcount" "wordwrap" "xmlattr"
;;     )
;;   "Jinja2 functions.")


(defvar jinja2-mode-hook nil)

(defvar jinja2-mode-map
  (let ((jinja2-mode-map (make-keymap)))
    (define-key jinja2-mode-map "\C-j" 'newline-and-indent)
    jinja2-mode-map)
  "Keymap for Jinja2 major mode")

(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . jinja2-mode))

(defvar jinja2-font-lock-keywords nil)
(setq jinja2-font-lock-keywords (list
   '("{# *\\(\n?.*?\\)+ *#}" . font-lock-comment-face)
   '("{{ *\\(.*?\\)\\(| ?.*?\\)* *}}" . (1 font-lock-variable-name-face))

   '("{{ *.*?\\(\\(| ?\\(a\\(?:bs\\|ttr\\)\\|batch\\|c\\(?:apitalize\\|enter\\)\\|d\\(?:\\(?:efaul\\|ictsor\\)t\\)\\|escape\\|f\\(?:i\\(?:\\(?:lesizeforma\\|rs\\)t\\)\\|loat\\|or\\(?:ceescape\\|mat\\)\\)\\|groupby\\|in\\(?:\\(?:den\\)?t\\)\\|join\\|l\\(?:ast\\|ength\\|ist\\|ower\\)\\|pprint\\|r\\(?:andom\\|e\\(?:\\(?:plac\\|vers\\)e\\)\\|ound\\)\\|s\\(?:afe\\|lice\\|ort\\|tri\\(?:ng\\|ptags\\)\\|um\\)\\|t\\(?:itle\\|r\\(?:im\\|uncate\\)\\)\\|u\\(?:pper\\|rlize\\)\\|word\\(?:count\\|wrap\\)\\|xmlattr\\) *\\)*?\\) *}}" . (1 font-lock-function-name-face))

   '("\\<\\(False\\|None\\|True\\|a\\(?:s\\|utoescape\\)\\|break\\|c\\(?:aller\\|ont\\(?:ext\\|inue\\)\\)\\|d\\(?:ebug\\|o\\)\\|e\\(?:lif\\|xtends\\)\\|f\\(?:alse\\|irstof\\|rom\\)\\|i\\(?:gnore\\|mport\\|nclude\\|[ns]\\)\\|kwargs\\|lo\\(?:ad\\|op\\)\\|missing\\|no\\(?:ne\\|w\\)\\|pluralize\\|regroup\\|s\\(?:coped\\|et\\|si\\|uper\\)\\|t\\(?:emplatetag\\|rue\\)\\|url\\|varargs\\|wi\\(?:dthratio\\|th\\(?:out\\)?\\)\\)\\>" . font-lock-builtin-face)
   '("\\<\\(end\\)?\\(aut\\(?:h\\|oescape\\)\\|block\\|call\\|e\\(?:lse\\|rrorproof\\)\\|f\\(?:\\(?:ilte\\|o\\)r\\)\\|if\\|macro\\|raw\\|showonmatch\\|trans\\|with\\)\\>" . font-lock-keyword-face)
   '("{%\\|\\%}\\|{{\\|}}" . font-lock-builtin-face)))

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
  (setq major-mode 'jinja2-mode)
  (setq mode-name "Jinja2")
  (run-hooks 'jinja2-mode-hook))

(provide 'jinja2-mode)
