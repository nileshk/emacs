;;; jpl-mode.el

(require 'cc-mode)

(eval-when-compile
  (let ((load-path
	 (if (and (boundp 'byte-compile-dest-file)
		  (stringp byte-compile-dest-file))
	     (cons (file-name-directory byte-compile-dest-file) load-path)
	   load-path)))
    (load "cc-mode" nil t)
    (load "cc-fonts" nil t)
    (load "cc-langs" nil t)
    (load "cc-bytecomp" nil t)
))

(eval-and-compile
  (c-add-language 'jpl-mode 'c-mode))

(c-lang-defconst c-identifier-ops
                 jpl '((left-assoc "." "?.")))

(c-lang-defconst c-after-id-concat-ops
  jpl '( "*" "&" "@" ))

(c-lang-defconst c-operators
  jpl `(
           ;; Primary.
           ,@(c-lang-const c-identifier-ops)
             
             (postfix-if-paren "<" ">")
             
             ;; Postfix.
             (left-assoc "." "*." "?." ".&" ".@")
             
             (postfix "++" "--" "[" "]" "(" ")" "<:" ":>")
             
             ;; Unary.
             (prefix "++" "--" "+" "-" "!" "~" "(" ")")
             
             ;; Multiplicative.
             (left-assoc "*" "/" "%")
             
             ;; Additive.
             (left-assoc "+" "-")
             
             ;; Shift.
             (left-assoc "<<" ">>" ">>>")
             
             ;; Relational.
             (left-assoc "<" ">" "<=" ">=" "<=>")
             
             ;; Matching.
             (left-assoc "=~" "==~" )

             ;; Equality.
             (left-assoc "==" "!=" )
             
             ;; Bitwise and.
             (left-assoc "&")
             
             ;; Bitwise exclusive or.
             (left-assoc "^")
             
             ;; Bitwise or.
             (left-assoc "|")
             
             ;; Logical and.
             (left-assoc "&&")
             
             ;; Logical or.
             (left-assoc "||")
             
             ;; Conditional.
             (right-assoc-sequence "?" ":")
             
             ;; Assignment.
             (right-assoc ,@(c-lang-const c-assignment-operators))
             
             ;; Sequence.
             (left-assoc ",")

             ;; Separator for parameter list and code in a closure.
             (left-assoc "->")
             ))

;; Virtual semicolon

(c-lang-defconst c-stmt-delim-chars
                 jpl "^;{}\n\r?:")

(c-lang-defconst c-stmt-delim-chars-with-comma
                 jpl "^;,{}\n\r?:")

(defun jpl-at-vsemi-p ( &optional pos )
  (save-excursion
    (let ((pos-or-point (if pos (goto-char pos) (point))))
      (if (eq pos-or-point (point-min))
          nil
        (and
         (not (char-equal (char-before) ?\;))
         (jpl-ws-or-comment-to-eol-p pos-or-point)
         (jpl-not-in-statement-p pos-or-point))))))

(c-lang-defconst c-at-vsemi-p-fn
                 jpl 'jpl-at-vsemi-p)

(defun jpl-ws-or-comment-to-eol-p ( pos )
  (save-excursion
    (goto-char pos)
    (skip-chars-forward " \t")
    (char-equal (char-after) ?\n)))

(defun jpl-not-in-statement-p ( pos )
  (save-excursion
    (goto-char pos)
    (if (equal (point) (point-min))
        nil
      (backward-char 1)
      (or
       (not (looking-at "[=+*/%<]"))
       (if (char-equal (char-after) ?>)
           (if (equal (point) (point-min))
               nil
             (char-equal (char-before) ?-)))))))

(defun jpl-vsemi-status-unknown-p () nil)

(c-lang-defconst c-vsemi-status-unknown-p-fn
                 jpl 'c-jpl-vsemi-status-unknown-p)

(c-lang-defconst c-typeless-decl-kwds
                 jpl (append (c-lang-const c-class-decl-kwds)
                                (c-lang-const c-brace-list-decl-kwds)
                                '("proc")))

(c-lang-defconst c-modifier-kwds
                 jpl '( "string" ))

(c-lang-defconst c-constant-kwds
  jpl '("SELECT" "FROM" "WHERE" "ORDER BY" "UPDATE" "DELETE" "INSERT" "COMMIT"
        "ROLLBACK" "GROUP BY" "AND" "SET" "INTO" "VALUES" "LIKE"
        "TO_DATE" "NVL" "LPAD" "RPAD" "ABS" "TO_NUMBER" "TO_CHAR" "REPLACE" "LTRIM"
        "RTRIM" "TRANSLATE" "SYSDATE" "CHR"))

(c-lang-defconst c-primary-expr-kwds
  jpl '("DBMS CLOSE CURSOR" "DBMS DECLARE CURSOR" "DBMS DECLARE" "DBMS WITH CURSOR"
        "DBMS SQL" "DBMS RUN" "DBMS CLOSE CONNECTION" "STORE FILE" "CONTINUE_TOP"
        "CONTINUE_BOTTOM" "CONTINUE_UP" "CONTINUE_DOWN" "STORED_SUB"
        "CONTINUE" "CURSOR" "EXECUTE" "FOR" "USING" "ALIAS"))

(c-lang-defconst c-inexpr-class-kwds
                 jpl nil)

(c-lang-defconst c-inexpr-brace-list-kwds
                 jpl nil)

(c-lang-defconst c-other-kwds
                 jpl '( "vars" "parms" "if" "else" "call" "send" "return" "global" 
                        "break" "while" "include" "public" "unload" "flush" "msg" "receive"
                        "send" "dbms" "RETURN" ))

(c-lang-defconst c-multiline-string-start-char
  jpl '( "'" ))

(defconst jpl-font-lock-keywords-1 (c-lang-const c-matchers-1 jpl)
  "Minimal highlighting for Jpl mode.
Fontifies nothing except the syntactic fontification of strings and
comments.")

(defconst jpl-font-lock-keywords-2 (c-lang-const c-matchers-2 jpl)
  "Fast normal highlighting for Jpl mode.
In addition to `java-font-lock-keywords-1', this adds fontification of
keywords, simple types, declarations that are easy to recognize, the
user defined types on `java-font-lock-extra-types', and the doc
comment styles specified by `c-doc-comment-style'.")

(defconst jpl-font-lock-keywords-3 (c-lang-const c-matchers-3 jpl)
  "Accurate normal highlighting for Jpl mode.
Like `java-font-lock-keywords-2' but detects declarations in a more
accurate way that works in most cases for arbitrary types without the
need for `java-font-lock-extra-types'.")

(defvar jpl-font-lock-keywords jpl-font-lock-keywords-3
  "Default expressions to highlight in Jpl mode.")

(defun jpl-font-lock-keywords-2 ()
  (c-compose-keywords-list jpl-font-lock-keywords-2))
(defun jpl-font-lock-keywords-3 ()
  (c-compose-keywords-list jpl-font-lock-keywords-3))
(defun jpl-font-lock-keywords ()
  (c-compose-keywords-list jpl-font-lock-keywords))

(defvar jpl-mode-syntax-table nil
  "Syntax table used in Jpl mode buffers.")
(or jpl-mode-syntax-table
    (setq jpl-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table jpl))))

(defvar jpl-mode-abbrev-table nil
  "Abbreviation table used in jpl-mode buffers.")
(c-define-abbrev-table 'jpl-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the syntactic context, and which
  ;; therefore should trigger reindentation when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar jpl-mode-map ()
  "Keymap used in jpl-mode buffers.")
(if jpl-mode-map
    nil
  (setq jpl-mode-map (c-make-inherited-keymap))
  )

;; Custom variables
(defcustom jpl-mode-hook nil
  "*Hook called by `jpl-mode'."
  :type 'hook
  :group 'c)

;;; The entry point into the mode
(defun jpl-mode ()
  "Major mode for editing Jpl code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `jpl-mode-hook'.

Key bindings:
\\{jpl-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table jpl-mode-syntax-table)
  (setq major-mode 'jpl-mode
	mode-name "JPL"
	local-abbrev-table jpl-mode-abbrev-table
	abbrev-mode t)
  (use-local-map jpl-mode-map)
  (c-init-language-vars jpl-mode)
  (c-common-init 'jpl-mode)
  (c-run-mode-hooks 'c-mode-common-hook 'jpl-mode-hook)
  (c-update-modeline))

(provide 'jpl-mode)
