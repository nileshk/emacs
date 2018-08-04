;;; PHP Debugger
(add-to-list 'load-path (concat emacs-root-p "geben"))
(autoload 'geben "geben" "PHP Debugger on Emacs" t)

;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook
          '(lambda ()
             (guess-style-guess-all)
             (global-guess-style-info-mode 1)
             (setq
              c-basic-offset 4
              tab-width 4)))
