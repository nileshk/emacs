(autoload 'csharp-mode "csharp-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(setq omnisharp-server-executable-path "~/Apps/omnisharp/OmniSharp.exe")

(defun my-csharp-mode ()
;;  (add-to-list 'company-backends 'company-omnisharp)
  (setq c-basic-offset 4)
;;  (setq omnisharp-host "http://wvm:2000/")
  (omnisharp-mode)
;;  (company-mode)
  (flycheck-mode)
  (turn-on-eldoc-mode))

;;(eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook 'my-csharp-mode)

(eval-after-load 'omnisharp
  '(progn
     (define-key omnisharp-mode-map (kbd "s-/") 'omnisharp-auto-complete)
     (define-key omnisharp-mode-map (kbd "s-G") 'omnisharp-find-usages)
     (define-key omnisharp-mode-map (kbd "<f3>") 'omnisharp-go-to-definition)
     (define-key omnisharp-mode-map (kbd "s-1") 'omnisharp-fix-code-issue-at-point)))

(eval-after-load 'flycheck
  '(progn
     (define-key flycheck-mode-map (kbd "s-.") 'flycheck-next-error)
     (define-key flycheck-mode-map (kbd "s->") 'flycheck-previous-error)))
