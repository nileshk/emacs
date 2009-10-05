(defun split-into-four ()
  "Split current window into 4 windows"
  (interactive)
  (split-window-vertically)
  (split-window-horizontally)
  (other-window 2)
  (split-window-horizontally)
  (other-window 2))

(defun split-into-four-switch-buffers ()
  "Split current window into 4 windows and switch buffers"
  (interactive)
  (split-window-vertically)
  (split-window-horizontally)
  (other-window 1)
  (next-buffer)
  (other-window 1)
  (next-buffer)
  (split-window-horizontally)
  (other-window 1)
  (next-buffer)
  (other-window 1)
  (next-buffer))

(global-set-key (kbd "C-x 4") 'split-into-four)
(global-set-key (kbd "C-x 8") 'split-into-four-switch-buffers)

(defun close-desktop ()
  (interactive)
  (desktop-kill)
  (desktop-save-mode 0)
  (mapc 'kill-buffer (buffer-list)))

;; TODO: Use the built-in functions for Emacs 23
(defun increase-font-size ()
  "Increase font size"
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun decrease-font-size ()
  "Decrease font size"
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))
(global-set-key (kbd "s-=") 'increase-font-size)
(global-set-key (kbd "s--") 'decrease-font-size)

(defun julian ()
  (interactive)
  (insert (substring (format-time-string "%y%j" (current-time)) 1)))

(defun switch-to-scratch ()
  "Switch to scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

;; Control-Super-s to switch to scratch buffer
(global-set-key (kbd "<C-s-268632083>") 'switch-to-scratch)

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (progn
        (close-desktop)
        (if (> emacs-major-version 22)
            (save-buffers-kill-terminal)
          (save-buffers-kill-emacs)))
    (message "Canceled exit")))
    
(defun refresh-file ()
  "Refresh the buffer from the disk (prompt if modified)"
  (interactive)
  (revert-buffer t (not (buffer-modified-p)) t))

(global-set-key [f5] 'refresh-file)

(defun flymake-goto-prev-error-with-display ()
  (interactive)
  (let ((err-buf nil))
    (condition-case err
        (setq err-buf (next-error-find-buffer))
      (error))
    (if err-buf
        (next-error)
      (progn
        (flymake-goto-prev-error)
        (let ((err (get-char-property (point) 'help-echo)))
          (when err
            (message err)))))))

(defun flymake-goto-next-error-with-display ()
  (interactive)
  (let ((err-buf nil))
    (condition-case err
        (setq err-buf (next-error-find-buffer))
      (error))
    (if err-buf
        (next-error)
      (progn
        (flymake-goto-next-error)
        (let ((err (get-char-property (point) 'help-echo)))
          (when err
            (message err)))))))

(defun toggle-transparency ()
  "Toggle transparency for the current frame"
   (interactive)
   (if (/=
        (cadr (find 'alpha (frame-parameters nil) :key #'car))
        100)
       (set-frame-parameter nil 'alpha '(100 100))
     (set-frame-parameter nil 'alpha '(85 60))))
