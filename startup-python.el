;;;; Python

;;; PyMacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(eval-after-load "pymacs"
  '(progn
     (if window-system (require 'fixpath))))

;;; Flymake-Pylint
;; (eval-after-load "flymake"
;;   '(progn
;;      (defun flymake-pylint-init ()
;;        (if window-system (require 'fixpath))
;;        (local-set-key (kbd "C-.") 'flymake-goto-next-error-with-display)
;;        (local-set-key (kbd "C->") 'flymake-goto-prev-error-with-display)
;;        (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                           'flymake-create-temp-inplace))
;;               (local-file (file-relative-name
;;                            temp-file
;;                            (file-name-directory buffer-file-name))))
;;          (list (concat emacs-root-p "support/epylint2.py") (list local-file))))
     
;;      (add-to-list 'flymake-allowed-file-name-masks
;;                   '("\\.py\\'" flymake-pylint-init))))

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (flymake-mode t)
;;              ))
