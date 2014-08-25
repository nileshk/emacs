;; Config for Functional Programming Languages, LISP, etc

;; If not installed by package.el
;; (add-to-list 'load-path (concat emacs-root-p "scala-mode"))
;; (load-if-exists (concat emacs-root-p "scala-mode/scala-mode-auto.el"))
;; (add-to-list 'load-path (concat emacs-root-p "haskell-mode"))
;; (add-to-list 'load-path (concat emacs-root-p "clojure-mode"))

;; TODO Load Slime via package.el
(add-to-list 'load-path (concat emacs-root-p "slime/contrib"))

(when macosx-p
  (message "Mac OS X detected")
  (setq inferior-lisp-program "/sw/bin/sbcl")
  (add-to-list 'load-path (concat script-root-p "Emacs/slime"))
  (load-if-exists (concat emacs-root-p "slime/slime-autoloads.el"))

;;;;;;;; Linux config ;;;;;;;;
(when linux-p
  (message "Linux detected")
  (add-to-list 'load-path "/usr/share/common-lisp/source/slime/")
  (setq inferior-lisp-program "/usr/bin/sbcl")


;; Clojure mode
;;(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(autoload 'clojure-mode "clojure" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(eval-after-load "clojure"
  '(progn
     (highlight-parentheses-mode t)))

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy
                    slime-fancy-inspector
                    slime-asdf
                    slime-indentation
                    slime-fontifying-fu))
     (slime-autodoc-mode)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

;; Haskell
(autoload 'haskell-mode "haskell-mode" "Haskell Mode" t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;;; Scala
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))
