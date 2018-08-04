(eval-after-load "plsql"
  '(progn
     (require 'sqlplus)
     (require 'fixpath)))

(add-hook 'plsql-mode-hook
          '(lambda ()
             (guess-style-guess-all)
             (global-guess-style-info-mode 1)
             (setq
              c-basic-offset 2
              tab-width 2)))

;;; Sql*Plus and PL/SQL Settings
(autoload 'sqlplus-mode "sqlplus" "SQL*Plus mode" t)
(autoload 'plsql-mode "plsql" "PL/SQL mode" t)
(add-to-list 'auto-mode-alist '("\.spc$" . plsql-mode))
(add-to-list 'auto-mode-alist '("\.bdy$" . plsql-mode))
(add-to-list 'auto-mode-alist '("\.sql$" . plsql-mode))

