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
  (let* ((json-str (shell-command-to-string (format "esy status -P %s"
						    project-path))) 
	 (json-false 'nil)
	 (json-object-type 'hash-table)) 

    (condition-case nil 
	(json-read-from-string json-str)
      (error (make-hash-table))
      )))

(defun esy/project--of-file-path (file-path)
  "of-file-path(path): returns an abstract structure that can later be used
                       to obtain more info about the esy project"
  (let* ((project-path (file-name-directory file-path))) (esy/project--of-path project-path)))

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
  (let* ((project (esy/project--of-path (file-name-directory buffer-file-name)
  (message "esy-mode activated"))))))

;;;###autoload
(add-hook 'reason-mode-hook 'esy-mode)

(provide 'esy-mode)
;;; esy.el ends here
