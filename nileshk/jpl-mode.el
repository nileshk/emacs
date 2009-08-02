;; jpl-mode.el - Major Mode for editing Prolifics Panther JPL
;;
;; Author: Nilesh Kapadia - http://www.nileshk.com
;; Version: 0.1
;;
;; This mode is derived from c-mode and at present only provides
;; syntax highlighting for JPL code.
;;
;; Installation:
;; 
;; 1) Ensure the jpl-mode.el is in your load path
;; 2) Add the following to your .emacs:
;;
;;   (autoload 'jpl-mode "jpl-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.jpl$" . jpl-mode))
;;

(require 'font-lock)

(defvar jpl-types
  '("string")
  "JPL types.")

(defvar jpl-keywords
  '("vars" "proc" "parms" "if" "else" "call" "send" "return" "global" "RETURN")
  "JPL keywords.")

(defvar jpl-constants
  '("SM_BUSY_CURSOR" "SM_TM_COMMAND" "SM_N_GOFIELD" "SM_DELAY_CURSOR"
    "SM_AUTO_BUSY_CURSOR" "SM_MB_OK" "SM_MB_ICONINFORMATION" "SM_JWINDOW"
    "SM_SELECT" "SM_DESELECT" "SM_TM_INQUIRE" "SM_MB_ICONSTOP")
  "JPL constants.")

(defvar jpl-database-commands
  '("DBMS CLOSE CURSOR" "DBMS DECLARE CURSOR" "DBMS DECLARE" "DBMS WITH CURSOR"
    "DBMS SQL" "DBMS RUN" "DBMS CLOSE CONNECTION" "STORE FILE" "CONTINUE_TOP"
    "CONTINUE_BOTTOM" "CONTINUE_UP" "CONTINUE_DOWN" "STORED_SUB"
    "CONTINUE" "CURSOR" "EXECUTE" "FOR" "USING" "ALIAS")
  "JPL Database Commands.")

(defvar jpl-sql
  '("SELECT" "FROM" "WHERE" "ORDER BY" "UPDATE" "DELETE" "INSERT" "COMMIT"
    "ROLLBACK" "GROUP BY" "AND" "SET" "INTO" "VALUES" "LIKE"
    "TO_DATE" "NVL" "LPAD" "RPAD" "ABS" "TO_NUMBER" "TO_CHAR" "REPLACE" "LTRIM"
    "RTRIM" "TRANSLATE" "SYSDATE" "CHR")
  "JPL SQL Commands.")

(defvar jpl-types-regexp (regexp-opt jpl-types 'words))
(defvar jpl-keywords-regexp (regexp-opt jpl-keywords 'words))
(defvar jpl-database-commands-regexp (regexp-opt jpl-database-commands 'words))
(defvar jpl-constants-regexp (regexp-opt jpl-constants 'words))
(defvar jpl-sql-regexp (regexp-opt jpl-sql 'words))
(defvar jpl-tm-regexp "\\<\\([A-Z][A-Z]\\|[A-Z]\\)_[_A-Z0-9]+\\>")
;;(defvar jpl-tm-regexp "\\<\\(TM_\\|SM_\\)[_A-Z0-9]+\\>")

(setq jpl-font-lock-keywords
      `(
        (,jpl-sql-regexp . font-lock-builtin-face)
        (,jpl-tm-regexp . font-lock-constant-face)
        (,jpl-keywords-regexp . font-lock-keyword-face)
        (,jpl-types-regexp . font-lock-type-face)
        (,jpl-database-commands-regexp . font-lock-type-face)
;;        (,jpl-constants-regexp . font-lock-constant-face)
        ))

(define-derived-mode jpl-mode c-mode "JPL"
  "Major mode for editing Prolifics JPL files.
\\{jpl-mode-map}"
  (setq font-lock-defaults '((jpl-font-lock-keywords)))

  (setq jpl-keywords nil)
  (setq jpl-constants nil)
  (setq jpl-database-commands nil)
  (setq jpl-sql nil)
  )

(provide 'jpl-mode)
