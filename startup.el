(defvar *emacs-load-start* (current-time))
(message "Begin loading startup.el")
(require 'cl)

(message (concat "System type: " (symbol-name system-type)))

(defvar mswindows-p (string-match "windows-nt" (symbol-name system-type)))
(defvar cygwin-p (string-match "cygwin" (symbol-name system-type)))
(defvar macosx-p (string-match "darwin" (symbol-name system-type)))
(defvar linux-p (string-match "gnu/linux" (symbol-name system-type)))

(defvar script-root-p (if mswindows-p
			"c:/cygwin/home/Nilesh/"
			"~/"))

(defvar emacs-root-p (concat script-root-p "Emacs/"))

(add-to-list 'load-path (concat emacs-root-p "vendor"))
(add-to-list 'load-path (concat emacs-root-p "nileshk"))
(add-to-list 'load-path (concat emacs-root-p "slime/contrib"))
(add-to-list 'load-path (concat emacs-root-p "haskell-mode"))
(add-to-list 'load-path (concat emacs-root-p "scala-mode"))
;;(add-to-list 'load-path (concat emacs-root-p "clojure-mode"))
(load (concat emacs-root-p "nileshk/functions.el"))
(load (concat emacs-root-p "nileshk/desktops.el"))
(load-if-exists (concat emacs-root-p "scala-mode/scala-mode-auto.el"))

;; http://nschum.de/src/emacs/guess-style/
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)

(when macosx-p
  (message "Mac OS X detected")
  (setq inferior-lisp-program "/sw/bin/sbcl")
  (add-to-list 'load-path (concat script-root-p "Emacs/slime"))
  (load-if-exists (concat emacs-root-p "slime/slime-autoloads.el"))
  (setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 2000) (height . 2000)))
  (when window-system 
    (require 'fixpath)
    (set-default-font
     "-*-Andale Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
;;     "-outline-Inconsolata-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")
   ;; Don't open new frame when a file is drag-and-dropped or opened from Finder
    (setq ns-pop-up-frames nil)))
   ;; Set environment variables
   ;; (if window-system (ns-grabenv "/bin/bash" "source /Users/nil/.bashrc"))

(when linux-p
  (message "Linux detected")
  (set-default-font
   "-*-Andale Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
  (add-to-list 'load-path "/usr/share/common-lisp/source/slime/")
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (if (> emacs-major-version 20) (tool-bar-mode -1))
  (setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 2000) (height . 2000))))

(when mswindows-p
  (message "MS Windows detected")
  (if (> emacs-major-version 20) (tool-bar-mode -1))
  (set-default-font
   "-*-Andale Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
  (defun w32-maximize-frame ()
    "Maximize the current frame"
    (interactive)
    (w32-send-sys-command 61488))
  (add-hook 'window-setup-hook 'w32-maximize-frame t)
  (autoload 'powershell 
    "powershell" "Run powershell as a shell within emacs." t))

(when cygwin-p
  (message "Cygwin detected"))

;; Don't show menu bar in text mode
(when (not window-system)
  (menu-bar-mode 0))

;; Load csharp-mode for C#
(autoload 'csharp-mode "csharp-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))

;; Load js2-mode for Javascript
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq
 js2-auto-indent-flag nil
 js2-indent-on-enter-key t)
 
;; For .jpl files use jpl-mode
(autoload 'jpl-mode "jpl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jpl$" . jpl-mode))

;; Clojure mode
;;(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(autoload 'clojure-mode "clojure" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))

(eval-after-load "plsql"
  '(progn
     (require 'sqlplus)
     (require 'fixpath)))

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

; If GUI client, start server for gnuclient.  Ask when closing via C-x C-c
(when window-system 
  (server-start)
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

;; Haskell
(autoload 'haskell-mode "haskell-mode" "Haskell Mode" t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; Conditionally load other stuff (host specific)
(let ((host-specific-files (concat "~/.emacs.d/" system-name ".el")))
  (if (file-exists-p host-specific-files)
      (load host-specific-files)
    (message (concat "No host specific customizations for " system-name))
  ))

;;;; Groovy Setup
;;; turn on syntax highlighting
(global-font-lock-mode 1)

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; Sql*Plus and PL/SQL Settings
(autoload 'sqlplus-mode "sqlplus" "SQL*Plus mode" t)
(autoload 'plsql-mode "plsql" "PL/SQL mode" t)
(add-to-list 'auto-mode-alist '("\.spc$" . plsql-mode))
(add-to-list 'auto-mode-alist '("\.bdy$" . plsql-mode))
(add-to-list 'auto-mode-alist '("\.sql$" . plsql-mode))

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
(eval-after-load "flymake"
  '(progn
     (defun flymake-pylint-init ()
       (if window-system (require 'fixpath))
       (local-set-key (kbd "C-.") 'flymake-goto-next-error-with-display)
       (local-set-key (kbd "C->") 'flymake-goto-prev-error-with-display)
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list (concat emacs-root-p "support/epylint2.py") (list local-file))))
     
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init))))

(add-hook 'python-mode-hook
          '(lambda ()
             (flymake-mode t)
             ))

;;; Scala
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;;; XML
(add-to-list 'auto-mode-alist
             '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|xsd\\|svg\\|rss\\)\\'" . 
               nxml-mode))
(setq nxml-child-indent 4)
(setq nxml-outline-child-indent 4)
(add-hook 'nxml-mode-hook
          '(lambda ()
             (guess-style-guess-all)
             (global-guess-style-info-mode 1)
             (when (indent-tabs-mode)
               (setq default-tab-width 4)
               (setq standard-indent 4))))

;;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
(eval-after-load 'markdown-mode
  '(progn
     (if window-system (require 'fixpath))
     (define-key markdown-mode-map
       (kbd "<tab>")
       (lambda()
         (interactive)
         (let ((yas/fallback-behavior 'return-nil))
           (unless (yas/expand)
             (message "markdown-cycle should be called")
             (markdown-cycle)))))))

;; MediaWiki
(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)

(add-to-list 'auto-mode-alist
             '("opus_index\.php.*\\.txt$" . wikipedia-mode))

(add-hook 'wikipedia-mode-hook
   '(lamdba ()
       (longlines-mode -1)))

; TODO lazy load this
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;;  '(add-to-list 'pymacs-load-path YOUR-PYMACS-DIRECTORY"))

;;; YASnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(yas/load-directory (concat emacs-root-p "snippets"))

;; (require 'auto-complete)
;; (load "~/Emacs/scripts/auto-complete-python.el")

(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; Turn off the annoying default backup behaviour
;; TODO: Create folder if it does not exist
(if (file-directory-p "~/.emacs.d/backup")
    (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
    (message "Directory does not exist: ~/.emacs.d/backup"))

;;; General settings
(setq default-tab-width 4)
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq visible-bell 'top-bottom)
(transient-mark-mode t)
(setq delete-by-moving-to-trash t)
(column-number-mode t)
(setq line-move-visual nil)
; Don't prompt when closing a buffer that emacsclient is waiting on
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;;; org-mode config
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(when (> emacs-major-version 22)
  (require 'org-install))
(eval-after-load "org"
  '(progn
     (define-key org-mode-map "\M-p" 'org-metaup)
     (define-key org-mode-map "\M-n" 'org-metadown)))
;;(add-hook 'org-mode-hook
;;          '(lambda ()
;;             (setq truncate-lines nil)))

(setq org-agenda-files (list "~/org/work.org"))

(require 'highlight-parentheses)
(highlight-parentheses-mode t)
;(setq show-paren-mode t)
;(setq show-paren-style 'parenthesis)

;; Ido mode
(when (> emacs-major-version 21)
  (ido-mode 1)
  (setq 
;;   ido-ignore-buffers               ; ignore these files
;;   '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido")
;;   ido-work-directory-list '("~/" "~/Desktop" "~/Documents")
   ido-case-fold  t                 ; be case-insensitive
   ido-use-filename-at-point nil    ; don't use filename at point
   ido-use-url-at-point nil         ; don't use url at point
   ido-enable-flex-matching t       ; be flexible
   ido-max-prospects 30             ; how many displayed in minibuffer
   ido-confirm-unique-completion t  ; wait for RET, even with unique completion
   ido-create-new-buffer 'always    ; Don't prompt on creating new buffers
  ))

;;; Keyboard settings
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-s-right>") 'next-buffer)
(global-set-key (kbd "<C-s-left>") 'previous-buffer)
(global-set-key (kbd "s-}") 'next-buffer)
(global-set-key (kbd "s-{") 'previous-buffer)
(global-set-key (kbd "M-?") 'hippie-expand)
(global-set-key (kbd "s-F") 'indent-region)
;; (global-unset-key (kbd "C-z"))
;(global-set-key (kbd "<C-tab>") 'bury-buffer)

;; How long Emacs took to load
(message "startup.el loaded in %ds" 
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*)
                           (second *emacs-load-start*)))))
;; Should not be any code after this