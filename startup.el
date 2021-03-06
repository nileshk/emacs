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

(load (concat emacs-root-p "nileshk/functions.el"))
(load (concat emacs-root-p "nileshk/ido-filecache.el"))
(load (concat emacs-root-p "nileshk/desktops.el"))

;;(load-if-exists (concat emacs-root-p "nxhtml/autostart.el"))

;;Edit server for Chrome's Edit in Emacs extension
(if (locate-library "edit-server")
    (progn
      (require 'edit-server)
      (setq edit-server-new-frame nil)
      (edit-server-start)))

;;; YASnippet
;;(add-to-list 'load-path (concat emacs-root-p "vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat emacs-root-p "snippets"))
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))

;; guess-style auto-detects indentation style
;; http://nschum.de/src/emacs/guess-style/
(autoload 'guess-style-set-variable "guess-style" nil t)
(autoload 'guess-style-guess-variable "guess-style")
(autoload 'guess-style-guess-all "guess-style" nil t)

;;;;;;;; Mac OS X config ;;;;;;;;
(when macosx-p
  (message "Mac OS X detected")
  (setq initial-frame-alist `((left . 1) (top . 1) (width . 256) (height . 64)))
  (if (> emacs-major-version 20) (tool-bar-mode -1))
  (when window-system 
    (require 'fixpath)
    (set-default-font
     "-*-Andale Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
;;     "-outline-Inconsolata-normal-r-normal-normal-13-97-96-96-c-*-iso8859-1")
   ;; Don't open new frame when a file is drag-and-dropped or opened from Finder
    (setq ns-pop-up-frames nil)))
   ;; Set environment variables
   ;; (if window-system (ns-grabenv "/bin/bash" "source /Users/nil/.bashrc"))

;;;;;;;; Linux config ;;;;;;;;
(when linux-p
  (message "Linux detected")
  (set-default-font
   "-*-Andale Mono-normal-r-*-*-14-*-*-*-c-*-iso8859-1")
  (if (> emacs-major-version 20) (if window-system (tool-bar-mode -1)))
  (setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 2000) (height . 2000))))

;;;;;;;; Windows Native config ;;;;;;;;
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

;;;;;;;; Cygwin config ;;;;;;;;
(when cygwin-p
  (message "Cygwin detected"))

;; Don't show menu bar in text mode
(when (not window-system)
  (menu-bar-mode 0))

(require 'flycheck)

;; Load js2-mode for Javascript
;; (autoload 'js2-mode "js2" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq
;;  js2-auto-indent-flag nil
;;  js2-indent-on-enter-key t
;;  js2-bounce-indent-p t)

(add-hook 'js-mode-hook
          '(lambda ()
             (guess-style-guess-all)
             (global-guess-style-info-mode 1)
             (when (indent-tabs-mode)
               (setq default-tab-width 4)
               (setq standard-indent 4))))

;(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;(defadvice web-mode-highlight-part (around tweak-jsx activate)
;  (if (equal web-mode-content-type "jsx")
;      (let ((web-mode-enable-part-face nil))
;        ad-do-it)
;    ad-do-it))

;; CSS
(add-hook 'css-mode-hook 'my-css-mode-hook)
(defun my-css-mode-hook ()
  (rainbow-mode 1))

;; For .jpl files use jpl-mode
(autoload 'jpl-mode "jpl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.jpl$" . jpl-mode))

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

(defun typescript-newline-and-indent ()
  (interactive)
  (typescript-indent-line)
  (newline)
  (typescript-indent-line))

;;; TypeScript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (guess-style-guess-all)
  (global-guess-style-info-mode 1)
  (electric-indent-mode 0)
  (setq default-tab-width 2)
  (setq standard-indent 2)
  (local-set-key "\C-j" 'typescript-newline-and-indent)
  (local-set-key "\r" 'typescript-newline-and-indent)
  (local-set-key (kbd "M-q") 'tide-documentation-at-point)
  (local-set-key (kbd "s-F") 'tide-format)
  (local-set-key (kbd "s-1") 'tide-fix)
  (local-set-key (kbd "TAB") 'typescript-indent-line))

(setq-default typescript-indent-level 2)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(flycheck-add-mode 'typescript-tslint 'web-mode)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;;     (define-key tide-mode-map "\C-q" 'tide-documentation-at-point)

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))


;; (with-eval-after-load 'flycheck
;;    (flycheck-add-mode 'typescript-tslint 'web-mode)
;;    (flycheck-add-mode 'javascript-eslint 'web-mode)
;;    (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

;; TypeScript END ;; -----------------------------

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
;(autoload 'processing-mode "processing-mode" "Processing mode" t)
;(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
;(setq processing-location "/Applications/Processing.app")

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

(require 'highlight-parentheses)
(highlight-parentheses-mode t)
;(setq show-paren-mode t)
;(setq show-paren-style 'parenthesis)

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
;;;;     (color-theme-word-perfect)
     (if window-system (color-theme-dark-laptop))))
;;     (if window-system (color-theme-blue-mood))))

;;; / Themes
;; Ido mode
(when (> emacs-major-version 21)
  (ido-mode 1)

;  (global-set-key (kbd "C-s") 'swiper)
  
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

;;; Ivy Mode
;; (ivy-mode 1)

;; (setq ivy-use-virtual-buffers t)
;; (setq ivy-count-format "(%d/%d) ")
;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;; Ivy Mode END

(require 'projectile)
;; (setq projectile-completion-system 'ivy)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

(tabbar-mode 1)

(defun my-tabbar-buffer-groups-by-project ()
  (list
   (cond
    ((memq major-mode '(mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-view-raw-mode
                                       twittering-mode weibo-timeline-mode
                                       jabber-roster-mode jabber-chat-mode erc-mode douban-music-mode
                                       ))
     "Activity"
     )
    ((memq major-mode '(eshell-mode term-mode shell-mode))
     (if (projectile-project-p) (projectile-project-name) "Common")
     )
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs"
     )
    ((memq major-mode '(fundamental-mode))
     "Emacs"
     )
    ((memq major-mode '(org-mode org-agenda-mode diary-mode))
     "OrgMode"
     )
    (t
     (if (projectile-project-p) (projectile-project-name) "Common")
     )
    )))

(defun my-tabbar-buffer-groups-by-all ()
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1))
     "Emacs"
     )
    (t "All"))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups-by-project)

(global-set-key (kbd "C-c C-t") `(lambda () (interactive)
                            (if (eq tabbar-buffer-groups-function 'my-tabbar-buffer-groups-by-all)
                                (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups-by-project)
                              (setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups-by-all)
                              )))

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

(global-set-key (kbd "s-{") 'tabbar-backward-tab)
(global-set-key (kbd "s-}") 'tabbar-forward-tab)
(global-set-key (kbd "C-s-{") 'tabbar-backward-group)
(global-set-key (kbd "C-s-}") 'tabbar-forward-group)

;(global-set-key (kbd "s-}") 'next-buffer)
;(global-set-key (kbd "s-{") 'previous-buffer)

(global-set-key (kbd "M-?") 'hippie-expand)
(global-set-key (kbd "s-F") 'indent-region)
(global-set-key (kbd "s-.") 'next-error)
(global-set-key (kbd "s->") 'previous-error)
;; (global-unset-key (kbd "C-z"))
;(global-set-key (kbd "<C-tab>") 'bury-buffer)

; Enable if ivy-mode
;(global-set-key (kbd "s-R") 'file-cache-ido-find-file)

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
