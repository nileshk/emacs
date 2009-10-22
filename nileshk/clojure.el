;;__________________________________________________________________________
;;;;    Programming - Clojure

(defvar clj-root (concat (expand-file-name "~") "/Apps/clj/"))
(defvar scripts-folder (concat (expand-file-name "~") "/Scripts"))
(setq load-path (append (list (concat clj-root "slime")
			      (concat clj-root "slime/contrib")
			      (concat clj-root "clojure-mode")
			      (concat clj-root "swank-clojure/src/emacs"))
			load-path))

(setq swank-clojure-binary "/Users/nil/Scripts/clj-cmd")
;(setq swank-clojure-binary (concat scripts-folder "/clj-cmd"))
(defvar clj-cmd)
;; (concat (expand-file-name "~") "/Scripts/clj-cmd"))

(eval-after-load "slime"
  '(progn
     (slime-setup)
     (setq slime-lisp-implementations
           `((clojure ("/Users/nil/Scripts/clj-cmd") :init swank-clojure-init)
;;	   `((clojure ((concat scripts-folder "/clj-cmd")) :init swank-clojure-init)
	     ,@slime-lisp-implementations))))
      
(require 'clojure-mode)
(require 'swank-clojure-autoload)

(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(clojure.contrib.javadoc/javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))

(setq slime-browse-local-javadoc-root (concat clj-root "java"))

(defun slime-browse-local-javadoc (ci-name)
  "Browse local JavaDoc documentation on Java class/Interface at point."
  (interactive (list (slime-read-symbol-name "Class/Interface name: ")))
  (when (not ci-name)
    (error "No name given"))
  (let ((name (replace-regexp-in-string "\\$" "." ci-name))
	(path (concat (expand-file-name slime-browse-local-javadoc-root) "/docs/api/")))
    (with-temp-buffer
      (insert-file-contents (concat path "allclasses-noframe.html"))
      (let ((l (delq nil
		     (mapcar #'(lambda (rgx)
				 (let* ((r (concat "\\.?\\(" rgx "[^./]+\\)[^.]*\\.?$"))
					(n (if (string-match r name)
					       (match-string 1 name)
					     name)))
				   (if (re-search-forward (concat "<A HREF=\"\\(.+\\)\" +.*>" n "<.*/A>") nil t)
				       (match-string 1)
				     nil)))
			     '("[^.]+\\." "")))))
	(if l
	    (browse-url (concat "file://" path (car l)))
	  (error (concat "Not found: " ci-name)))))))

(defun run-clojure ()
  "Starts clojure in Slime"
  (interactive)
  (setenv "CLJ_CMD" 
          (setq clj-cmd
                (concat "java "
                        "-server "
                        "-Xdebug -Xrunjdwp:transport=dt_socket,"
                        "server=y,suspend=n,address=8888 "
                        "-cp "
                        (concat clj-root "clojure/clojure-1.0.0.jar:")
                        (concat (expand-file-name "~") "/.clojure:")
                        (concat clj-root "clojure-contrib/clojure-contrib.jar:")
                        (concat clj-root "swank-clojure/swank-clojure.jar:")
                                        ;(concat clj-root "src/book-code:")
                        " clojure.lang.Repl")))
  (slime 'clojure))

(global-set-key [(control f5)] 'run-clojure)
(global-set-key [(control f11)] 'slime-selector)

(add-hook 'slime-connected-hook (lambda ()
				  (interactive)
				  (slime-redirect-inferior-output)
				  (define-key slime-mode-map (kbd "<return>") 'newline-and-indent)
				  (define-key slime-mode-map (kbd "C-j") 'newline)
				  (define-key slime-mode-map (kbd "C-c b") 'slime-browse-local-javadoc)
				  (define-key slime-repl-mode-map (kbd "C-c b") 'slime-browse-local-javadoc)			  
				  (define-key slime-mode-map (kbd "C-c d") 'slime-java-describe)
				  (define-key slime-repl-mode-map (kbd "C-c d") 'slime-java-describe)
				  (define-key slime-mode-map (kbd "C-c D") 'slime-javadoc)
				  (define-key slime-repl-mode-map (kbd "C-c D") 'slime-javadoc)))

;(clojure-slime-config "/Users/nil/Apps/clj")

;;__________________________________________________________________________
;;;;    Programming - Clojure


(defun maven-find-pom (path)
  "Finds the nearest pom to the given path."

  (when (not path)
	(setq path buffer-file-name))
  
  (unless (not path)
	(let ((current-path (if (file-directory-p (file-name-as-directory path))
							(file-name-as-directory path)
						  (file-name-directory path)))
		  (next-path nil)
		  (found-p nil))
	  (catch 'done
		(while (not found-p)
		  (let ((files (directory-files current-path t))
				(next-path nil))
			(dolist (file files)
			  (cond
			   ((equal "pom.xml" (file-name-nondirectory file))
				(setq found-p t)
				(throw 'done t))
			   ((and (equal ".." (file-name-nondirectory file))
					 (not (equal current-path (file-truename file))))
				(setq next-path (file-truename file)))))
			(if (not next-path)
				(throw 'done t))
			(setq current-path next-path)
			(setq next-path nil))))
	  (cond
	   (found-p
		(expand-file-name (concat (file-name-as-directory current-path) "pom.xml")))
	   (t
		nil)))))
  
(defun find-pom-file ()
  "Finds the nearest pom to the current file."
  (interactive)
  (let ((pom (maven-find-pom buffer-file-name)))
	(if pom (find-file pom)
	  (error "Couldn't find pom.xml"))))

(defun run-clojure-maven ()
  "Run mvn clojure:swank"
  (interactive)
  (let ((pom (maven-find-pom buffer-file-name)))
    (if pom
        (progn
          (setenv "CLJ_CMD" 
                  (setq clj-cmd (concat "mvn -f " pom " clojure:repl")))
          (slime 'clojure))
      (message "Failed to find pom.xml"))))

(global-set-key [(control f6)] 'run-clojure-maven)
