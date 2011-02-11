(add-to-list 'load-path (expand-file-name "~/Emacs/company"))
(autoload 'company-mode "company" nil t)

(add-to-list 'load-path (expand-file-name "~/Apps/emacs-eclim/"))
;; only add the vendor path when you want to use the libraries provided with emacs-eclim
(add-to-list 'load-path (expand-file-name "~/Apps/emacs-eclim/vendor"))
(require 'eclim)
(require 'company-emacs-eclim)

(setq eclim-auto-save t)
(global-eclim-mode)

(setq company-begin-commands '(self-insert-command))
;;(company-emacs-eclim-setup)

;; Java
(add-hook 'java-mode-hook
          '(lambda ()
             (message "Java mode")
             (guess-style-guess-all)
             (global-guess-style-info-mode 1)))
;;             (local-set-key (kbd "M-/") 'company-dabbrev-code)))
