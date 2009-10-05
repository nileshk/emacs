(defun work-on-desktop (desktop)
  "Open a desktop and turn on desktop-save-mode"
  (interactive)
  (setq desktop-load-locked-desktop t) ;; Avoid being asked about PID in lock file
  (desktop-change-dir (concat "/Users/nil/Emacs/desktops/" desktop))
  (desktop-save-mode 1))

(defun desktop-work ()
  "Desktop work"
  (interactive)
  (work-on-desktop "work"))

(defun desktop-home ()
  "Desktop at home"
  (interactive)
  (work-on-desktop "home"))
