; $Id: flowcaml.el,v 1.3 2003/06/27 15:13:39 simonet Exp $

(defvar flowcaml-font-lock t
  "*Enable highlighting capabilities of Flow Caml mode.")



(defun flowcaml-font-lock ()
  (cond
   ((fboundp 'global-font-lock-mode)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults
	  '(flowcaml-font-lock-keywords nil nil ((?' . "w") (?_ . "w")))))
   (t
    (setq font-lock-keywords flowcaml-font-lock-keywords)))
  (make-local-variable 'font-lock-keywords-only)
  (setq font-lock-keywords-only t)
  (font-lock-mode 1)
)



(defconst flowcaml-font-lock-keywords
  (list
;stop special comments
   '("\\(^\\|[^\"]\\)\\((\\*\\*/\\*\\*)\\)"
     2 font-lock-stop-face)
;doccomments
   '("\\(^\\|[^\"]\\)\\((\\*\\*[^*]*\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-doccomment-face)
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 font-lock-comment-face)
;character literals
   (cons (concat caml-quote-char "\\(\\\\\\([ntbr" caml-quote-char "\\]\\|"
                 "[0-9][0-9][0-9]\\)\\|.\\)" caml-quote-char
                 "\\|\"[^\"\\]*\\(\\\\\\(.\\|\n\\)[^\"\\]*\\)*\""
                 "\\|`[^\\]*\\(\\\\\\(.\\|\n\\)[^\\]*\\)*`")
         'font-lock-string-face)
;modules and constructors
   '("\\(`?\\<[A-Z][A-Za-z0-9_']*\\>\\)\\|\\(!\\<[a-z][a-z0-9_']*\\>\\)"
     . font-lock-function-name-face)
;definition
   (cons (concat
          "\\<\\(a\\(nd\\|s\\)\\|c\\(onstraint\\|lass\\)"
          "\\|ex\\(ception\\|ternal\\)\\|fun\\(ct\\(ion\\|or\\)\\)?"
          "\\|in\\(herit\\|itializer\\)?\\|let"
          "\\|m\\(ethod\\|utable\\|odule\\)"
          "\\|of\\|p\\(arser\\|rivate\\)\\|rec\\|type"
          "\\|v\\(al\\(ue\\)?\\|irtual\\)"
	  "\\|level\\|content\\|flow\\|affects\\|raises\\|row"
	  "\\|less\\|greater\\|than\\|noneq"
	  "\\)\\>")
         'font-lock-type-face)
;blocking
   '("\\<\\(begin\\|end\\|object\\|s\\(ig\\|truct\\)\\)\\>"
     . font-lock-keyword-face)
;control
   (cons (concat
          "\\<\\(do\\(ne\\|wnto\\)?\\|else\\|for\\|i\\(f\\|gnore\\)"
          "\\|lazy\\|match\\|new\\|or\\|t\\(hen\\|o\\|ry\\)"
	  "\\|finally\\|from"
          "\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>"
          "\\|\|\\|->\\|&\\|#")
         'font-lock-reference-face)
   '("\\<raise\\|propagate\\>" . font-lock-comment-face)
;labels (and open)
   '("\\(\\([~?]\\|\\<\\)[a-z][a-zA-Z0-9_']*:\\)[^:=]" 1
     font-lock-variable-name-face)
   '("\\<\\(assert\\|open\\|include\\)\\>\\|[~?][ (]*[a-z][a-zA-Z0-9_']*"
     . font-lock-variable-name-face)))



(defun flowcaml-mode ()
  "Major mode for editing Flow Caml code."

  (interactive)

  (require 'caml)
  (kill-all-local-variables)
  (setq major-mode 'flowcaml-mode)
  (setq mode-name "flowcaml")
  (use-local-map caml-mode-map)
  (set-syntax-table caml-mode-syntax-table)
  (setq local-abbrev-table caml-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(*")
  (make-local-variable 'comment-end)
  (setq comment-end "*)")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'caml-indent-command)
  ;itz Fri Sep 25 13:23:49 PDT 1998
  (make-local-variable 'add-log-current-defun-function)
  (setq add-log-current-defun-function 'caml-current-defun)
  ;itz 03-25-96
  (setq before-change-function 'caml-before-change-function)
  (setq caml-last-noncomment-pos nil)
  (setq caml-last-comment-start (make-marker))
  (setq caml-last-comment-end (make-marker))
  ;garrigue 27-11-96
  (setq case-fold-search nil)
  ;garrigue july 97
  (if running-xemacs ; from Xemacs lisp mode
      (if (and (featurep 'menubar)
               current-menubar)
          (progn
            ;; make a local copy of the menubar, so our modes don't
            ;; change the global menubar
            (set-buffer-menubar current-menubar)
            (add-submenu nil caml-mode-xemacs-menu)))
    ;imenu support (not for Xemacs)
    (make-local-variable 'imenu-create-index-function)
    (setq imenu-create-index-function 'caml-create-index-function)
    (make-local-variable 'imenu-generic-expression)
    (setq imenu-generic-expression caml-imenu-search-regexp)
    (if (and caml-imenu-enable (< (buffer-size) 10000))
        (caml-show-imenu)))

  (make-local-variable 'inferior-caml-program)
  (setq inferior-caml-program "flowcaml")

  (if flowcaml-font-lock (flowcaml-font-lock))

)

(provide 'flowcaml)
