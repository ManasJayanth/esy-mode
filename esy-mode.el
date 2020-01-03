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

(defun esy/f--read (file-path)
  "Return file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun esy/project--of-path (project-path) 
  "of-path(path): returns an abstract structure that can later
be used to obtain more info about the project"
  (let* ((default-directory project-path)
	 (json-str (shell-command-to-string "esy status")) 
	 (json-array-type 'list) 
	 (json-key-type 'string) 
	 (json-false 'nil)
	 (json-object-type 'hash-table)
	 (esy-status-json
	  (condition-case nil 
	      (json-read-from-string json-str)
	    (error (progn
		     (message "Error while json parsing \
'esy status'")
		     (make-hash-table)))))) 
    (list 'json esy-status-json
	   'path project-path)))

(defun esy/project--of-file-path (file-path)
  "of-file-path(path): returns an abstract structure that can
later be used to obtain more info about the esy project"
  (let* ((project-path (file-name-directory file-path))) (progn (esy/project--of-path project-path))))

(defun esy/project--of-buffer (buffer)
  "of-buffer(buffer): returns an abstract structure that can 
later be used to obtain more info about the esy project"
  (let* ((file-name (buffer-file-name buffer))) (if file-name (esy/project--of-file-path file-name) nil)))

(defun esy/project--ready-p (project)
  "ready-p(project): returns if a given project is ready for
development ie. if the tools can be looked in it's sandbox"
  (gethash "isProjectReadyForDev" (plist-get project 'json)))

(defun esy/project--p (project) 
  "ready-p(project): returns if a given project structure is a valid esy project"
  (gethash "isProject" (plist-get project 'json)))

(defun esy/command-env--of-project (project)
  "Given a project, it returns an abstract structure
command-env"
  nil)

(defun esy/command-env--to-process-environment (command-env)
  "Given a command-env, it turns it into a list
that can be assigned to 'process-environment"
  nil)

(defun esy/command-env--to-get-exec-path (command-env)
  "Given a command-env, it turns it into a list that
can be assigned to 'exec-path"
  nil)

(defun esy/setup--esy-get-available-tools (project)
  
  "setup--esy-return-missing-tools(project): looks into the 
esy sandbox and returns a plist of missing tools. Specifically,
it looks for 

1. refmt/ocamlfmt
2. lsp
3. merlin

"
  (let ((command-env esy/command-env--of-project project)
	(tools '()))
    (progn
      (make-local-variable 'process-environment)
      (setq process-environment
	    (esy/command-env--to-process-environment
	     command-env))
      (make-local-variable 'exec-path)
      (setq exec-path
	    (esy/command-env--get-exec-path command-env))
      (plist-put tools 'build "esy")
      (plist-put tools 'refmt (executable-find "refmt"))
      (plist-put tools 'merlin (executable-find "merlin"))
      (plist-put tools 'lsp (executable-find "ocamllsp")))))

(defun esy/setup--esy (project)
  "setup--esy(project): runs ops to ensure project is ready
for development"
  (if (esy/project--ready-p project)
      (progn
	(message "Project ready for development"))
    (if (y-or-n-p
	 "Seems like a valid esy project. Go ahead and install and build all dependencies?")
	(progn
	  (compile "esy"))))
  (esy/setup--esy-get-available-tools project))

(defun esy/setup--opam (_)
  "setup--opam(_): currently doesn't do anything. opam-user-setup works well enough, IMO!"
  ;; TODO: Look up opam switch
  '(build "echo TODO: look into opam file" refmt "opam exec refmt" merlin "opam exec merlin" lsp "opam exec lsp"))

(defun esy/setup--npm(project)
  
  "setup--npm(project): Although named 'npm', this function uses esy to setup the Reason/OCaml toolchain.

npm is incapable of
  a) handling prebuilts correctly
  b) Correctly setup environment for tools that assume they are the only ones running. 
     Eg: merlin expected the correct ocamlmerlin-reason available on it's path. This can be tricky in non-sandboxed setup where a user could have almost any version of ocamlmerlin installed

"
  '())

(defun esy/manifest--of-path (file-path)
  "Creates an abstract manifest structure given file path"
  (if (esy/manifest--json-p file-path)
      (let ((json-str (esy/f--read file-path)) 
	 (json-array-type 'list) 
	 (json-key-type 'string) 
	 (json-false 'nil)
	 (json-object-type 'hash-table)) 
    (progn
    (condition-case nil 
	(json-read-from-string json-str)
      (error (progn
	       (message "Error while json parsing")
	       nil))))
    (progn
      (message "Non JSON manifest not supported yet")
      nil))))

(defun esy/manifest--json-p (file-path)
  "Takes a file path and returns if file at said path is 
json or not"
  ;; Cheat!
  (if file-path (string-match "\.json$" file-path) nil))

(defun esy/manifest--package-json-p (file-path)
  "Takes a file path and returns if file at said path is 
package.json or not"
  (if file-path (string-match "package\.json$" file-path) nil))

(defun esy/project--get-manifest-file-path (project)
  "returns the path to manifest file"
  (gethash "rootPackageConfigPath" (plist-get project 'json)))

(defun esy/manifest--contains-esy-field-p (manifest)
  "Checks if a manifest structure contains esy field"
  (if manifest (gethash "esy" manifest) nil))

(defun esy/package-manager--of-project (project)
  "Detect the package manager of the project. Returns either
'esy|'opam|'npm"
  (let* ((manifest-file-path
	  (esy/project--get-manifest-file-path project)))
	 (if (esy/manifest--json-p manifest-file-path)
	     ;; The manifest file is a json.
	     (if (esy/manifest--package-json-p
		  manifest-file-path)
		 ;; Could be npm or esy
		 (if (esy/project--ready-p project)
		     ;; esy says this project with package.json
		     ;; is ready for development i.e. all it's
		     ;; dependencies were fetched and installed
		     ;; by esy. Definitely an esy project
		     'esy
		   (progn
		     ;; Checking if there is an esy field in
		     ;; the package.json. If there is one,
		     ;; it's an esy project
		     (if (esy/manifest--contains-esy-field-p
			  (esy/manifest--of-path manifest-file-path))
			 'esy
		       'npm)))
	       'esy)
	   'opam)))


;;;###autoload
(define-minor-mode esy-mode () 
  "Minor mode for esy - the package manager for Reason/OCaml" 
  :lighter " esy"
  (progn
    ;; TODO: Check if esy is available on the system
    (let* ((project (esy/project--of-buffer (current-buffer))))
      (if (esy/project--p project)
	  (progn
	    
	    ;;All npm and opam projects are valid esy projects
	    ;;too! Picking the right package manager is important
	    ;;- we don't want to run `esy` for a user who never
	    ;;intended to. Example: bsb/npm users. Similarly,
	    ;;opam users wouldn't want prompts to run `esy`. Why
	    ;;is prompting `esy i` even necessary in the first
	    ;;place? `esy ocamlmerlin-lsp` needs projects to
	    ;;install/solve deps

	    (let ((config-plist
		   (let ((project-type (esy/package-manager--of-project)))
	      (cond (((eq project-type 'opam) (esy/setup--opam project))
		     ((eq project-type 'esy) (esy/setup--esy project))
		     ((eq project-type 'npm) (esy/setup--npm project)))))))
	      (progn
		(make-local-variable 'compile-command)
		(setq compile-command
		      (plist-get config-plist :build))
		(make-local-variable 'refmt-command)
		(setq refmt-command
		      (plist-get config-plist :refmt))
		(make-local-variable 'merlin-command)
		(setq merlin-command
		      (plist-get config-plist :merlin))
		(make-local-variable 'lsp-command-tuple)
		(setq 'lsp-command-tuple
		      (plist-get config-plist
				 :lsp))
	    )))
	(message "Doesn't look like an esy project. esy-mode will stay dormant")))))

;;;###autoload
(add-hook 'reason-mode-hook 'esy-mode)

(provide 'esy-mode)
;;; esy.el ends here
