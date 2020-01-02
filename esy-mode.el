;;; esy.el --- Minor mode for assist esy, the package manager.

;; Copyright (C) 2010-2019 Manas Jayanth

;; Author: Manas Jayanth <prometheansacrifice@gmail.com>
;; Created: 1 Jan 2020
;; Keywords: Reason, OCaml
;; Homepage: http://example.com/foo


;;; Commentary:

;; esy-mode looks up the tools in the project's sandbox and set's
;; the respective variables with the path. For instance, it looks
;; up refmt in a project's sandbox and set refmt-command to
;; "esy exec-command refmt" only if it is present (in an OCaml only
;; project it need not be available).

;;; Change Log: TODO

;;; Code:
(require 'json)

;; Customization
(defgroup esy nil 
  "Manage esy configuration" 
  :group 'tools 
  :group 'convenience 
  :link '(url-link :tag "github"
		   "https://github.com/prometheansacrifice/esy-mode"))

;; Units
(defun add-two (p) 
  (+ p 2))

(defun esy/project--of-path (project-path) 
  "of-path(path): returns an abstract structure that can later be used to
                 obtain more info about the project"
  (let* ((default-directory project-path)
	 (json-str (shell-command-to-string "esy status")) 
	 (json-array-type 'list) 
	 (json-key-type 'string) 
	 (json-false 'nil)
	 (json-object-type 'hash-table)) 
    (progn
    (condition-case nil 
	(json-read-from-string json-str)
      (error (progn
	       (message "Error while json parsing")
	       (make-hash-table)))))))

(defun esy/project--of-file-path (file-path)
  "of-file-path(path): returns an abstract structure that can later be used
                       to obtain more info about the esy project"
  (let* ((project-path (file-name-directory file-path))) (progn (esy/project--of-path project-path))))

(defun esy/project--of-buffer (buffer)
  "of-buffer(buffer): returns an abstract structure that can later be used 
                      to obtain more info about the esy project"
  (let* ((file-name (buffer-file-name buffer))) (if file-name (esy/project--of-file-path file-name) nil)))

(defun esy/project--ready-p (json)
  "ready-p(project): returns if a given project is ready for development ie.
                     if the tools can be looked in it's sandbox"
  (gethash "isProjectReadyForDev" json))

(defun esy/project--p (json) 
  "ready-p(project): returns if a given project structure is a valid esy project"
  (gethash "isProject" json))

;;;###autoload
(define-minor-mode esy-mode () 
  "Minor mode for esy - the package manager for Reason/OCaml" 
  :lighter " esy"
  (progn
    ;; TODO: Check if esy is available on the system
    (make-local-variable 'compile-command) 
    (setq compile-command "esy")
    (let* ((project (esy/project--of-buffer (current-buffer))))
      (if (esy/project--p project)
	  (progn
	    (message "Activating esy-mode...")
	    (if (esy/project--ready-p project)
		(message "Project ready for development")
	      (if (y-or-n-p
		   "Seems like a valid esy project. Go ahead and install and build all dependencies?")
		  (compile "esy") t)))
	(message "Doesn't look like an esy project. esy-mode will stay dormant")))))

;;;###autoload
(add-hook 'reason-mode-hook 'esy-mode)

(provide 'esy-mode)
;;; esy.el ends here
