(defvar *emacs-load-start* (current-time))
(message "Begin loading startup.el")
(require 'cl)

(message (concat "System type: " (symbol-name system-type)))

(defvar mswindows-p (string-match "windows-nt" (symbol-name system-type)))

(defvar emacs-root-p "M:/")

(add-to-list 'load-path (concat emacs-root-p "vendor"))
(add-to-list 'load-path (concat emacs-root-p "nileshk"))

(load (concat emacs-root-p "nileshk/functions.el"))
(load (concat emacs-root-p "nileshk/ido-filecache.el"))
(load (concat emacs-root-p "nileshk/desktops.el"))
;;(load-if-exists (concat emacs-root-p "vendor/php-mode.elc"))

;;(load-if-exists (concat emacs-root-p "nxhtml/autostart.el"))

(message "Loading platform specific settings...")

;;; YASnippet
;;(add-to-list 'load-path (concat emacs-root-p "vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat emacs-root-p "snippets"))
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

;;; PHP Debugger
(add-to-list 'load-path (concat emacs-root-p "geben"))
(autoload 'geben "geben" "PHP Debugger on Emacs" t)

;; guess-style auto-detects indentation style
;; http://nschum.de/src/emacs/guess-style/
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)

;;;;;;;; Windows Native config ;;;;;;;;
(when mswindows-p
  (message "MS Windows detected")
  (setq inhibit-startup-screen t)
;;  (if (> emacs-major-version 20) (tool-bar-mode -1))
;;  (set-default-font
;;   "-*-Andale Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
  (defun w32-maximize-frame ()
    "Maximize the current frame"
    (interactive)
    (w32-send-sys-command 61488))
  (add-hook 'window-setup-hook 'w32-maximize-frame t)
  (autoload 'powershell 
    "powershell" "Run powershell as a shell within emacs." t))

(add-hook 'js-mode-hook
          '(lambda ()
             (guess-style-guess-all)
             (global-guess-style-info-mode 1)
             (when (indent-tabs-mode)
               (setq default-tab-width 4)
               (setq standard-indent 4))))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; CSS
(add-hook 'css-mode-hook 'my-css-mode-hook)
(defun my-css-mode-hook ()
  (rainbow-mode 1))

;; PHP
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-hook 'php-mode-hook
          '(lambda ()
             (guess-style-guess-all)
             (global-guess-style-info-mode 1)
             (setq
              c-basic-offset 4
              tab-width 4)))

;; For .jpl files use jpl-mode
(autoload 'jpl-mode "jpl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jpl$" . jpl-mode))

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

; If GUI client, start server for gnuclient.  Ask when closing via C-x C-c
(when window-system 
  (server-start)
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

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

;;; XML
(add-to-list 'auto-mode-alist
             '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|xsd\\|svg\\|rss\\|jsp\\)\\'" . 
               nxml-mode))
(setq nxml-child-indent 4)
(setq nxml-outline-child-indent 4)
(add-hook 'nxml-mode-hook
          '(lambda ()
             (guess-style-guess-all)
             (global-guess-style-info-mode 1)
             (when (indent-tabs-mode)
               (setq default-tab-width 4)
               (setq tab-width 4)
               (setq standard-indent 4))))

;;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.\\(markdown\\|md\\)" . markdown-mode) auto-mode-alist))
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

;;; Processing

;;  (add-to-list 'load-path "/path/to/processing-emacs/")
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "/Applications/Processing.app")

;; MediaWiki
(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)

;; This is used to recognize MediaWiki files received from the Firefox
;; extension ViewSourceWith and from a specific host named "opus"
(add-to-list 'auto-mode-alist
             '("opus_index\.php.*\\.txt$" . wikipedia-mode))

(add-hook 'wikipedia-mode-hook
   '(lambda ()
      (longlines-mode -1)))

;; Twitter
;; (autoload 'twitter-get-friends-timeline "twitter" nil t)
;; (autoload 'twitter-status-edit "twitter" nil t)
;; (global-set-key "\C-xt" 'twitter-get-friends-timeline)
;; (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; Turn off the default backup behaviour
;; Instead, save backups to ~/.emacs.d/backup (creating folder if necessary)
(let ((backup-dir "~/.emacs.d/backup"))
  '(progn
     (if (not (file-directory-p backup-dir))
	 '(progn
	    (message (concat "Creating directory: " backup-dir))
	    (make-directory backup-dir))))
  (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))))

;;; General settings
(global-auto-revert-mode t)
(setq default-tab-width 4)
(setq standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t)

(defun my-terminal-visible-bell ()
  "A friendlier visual bell effect."
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))
 
(setq visible-bell nil
      ring-bell-function #'my-terminal-visible-bell)

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
     (define-key org-mode-map "\M-n" 'org-metadown)
     (setq org-todo-keywords
       '((sequence "WAIT" "TODO" "INPROGRESS" "|" "DONE")))))
;;(add-hook 'org-mode-hook
;;          '(lambda ()
;;             (setq truncate-lines nil)))

(setq org-agenda-files (list "~/Dropbox/org/work.org"))

;; TODO
;(require 'highlight-parentheses)
;(highlight-parentheses-mode t)


;; Modeline config
(set-face-background 'mode-line "#acccfc")

;; Adaptive wrap
(setq adaptive-wrap-extra-indent 4)

;;; Themes
;;(add-to-list 'load-path (concat emacs-root-p "themes/color-theme"))
;;(add-to-list 'load-path (concat emacs-root-p "themes/solarized"))

(require 'color-theme)
;;(require 'color-theme-solarized)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
;;     (color-theme-word-perfect)
     (if window-system (color-theme-blue-mood))))

;;; / Themes

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Smex ( http://github.com/nonsequitur/smex/ )
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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
(global-set-key (kbd "s-R") 'file-cache-ido-find-file)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;; Upcase / downcase commands enabled
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(blink-cursor-mode 0)

;;; Environment Variables

(setenv "PAGER" "cat")

;; How long Emacs took to load

;; Broken in 24.3
;; (message "startup.el loaded in %ds" 
;;          (destructuring-bind (hi lo ms) (current-time)
;;            (- (+ hi lo) (+ (first *emacs-load-start*)
;;                            (second *emacs-load-start*)))))
;; Should not be any code after this
