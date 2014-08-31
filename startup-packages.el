;; Configuration that uses package managers such as package.el
(message "Begin loading startup-packages.el")
(require 'package)
(require 'cl)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun my-packages-installed-p ()
  (setq my-packages
        '(ack-and-a-half actionscript-mode
                         adaptive-wrap
                         csharp-mode
                         clojure-mode
                         color-theme
                         company
                         geben
                         groovy-mode
                         haskell-mode
                         highlight-parentheses
                         ipython
                         js2-mode
                         markdown-mode
                         omnisharp
                         php-mode
                         plsql
                         projectile
                         python
                         rainbow-delimiters
                         scala-mode
                         slime
                         solarized-theme
                         sqlplus
                         yaml-mode
                         yasnippet))
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun update-packages ()
  (interactive)
  (message "Updating/installing packages")
  (unless (my-packages-installed-p)
    ;; check for new packages (package versions)
    (package-refresh-contents)
    ;; install the missing packages
    (dolist (p my-packages)
      (when (not (package-installed-p p))
        (package-install p)))))

;;;; el-get configuration

;;(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))

;; (add-to-list 'el-get-recipe-path "~/Emacs/el-get-user/recipes")
;; (el-get 'sync)

;;(autoload 'jedi:setup "jedi" nil t)

;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:setup-keys t)
;;(setq jedi:complete-on-dot t)

