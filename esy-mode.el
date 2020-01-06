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

(setq lexical-binding t)

(defvar esy-lsp-command "")
(make-local-variable 'esy-lsp-command)
(defvar esy-refmt-command nil)
(make-local-variable 'esy-refmt-command)
(defvar esy-merlin-command nil)
(make-local-variable 'esy-merlin-command)
(defvar esy-compile-command nil)
(make-local-variable 'esy-compile-command)


;; Units
(defun add-two (p) 
  (+ p 2))

(defun esy/f--read (file-path)
  "Return file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun esy/f--write (fname data)
  "Write to file"
  (with-temp-file fname (insert data)))

(defun esy/internal--project--get-manifest-file-path
    (esy-status-json)
  "Given the json object of 'esy status' output,
it returns the manifest file"
  (gethash "rootPackageConfigPath" esy-status-json))

(defun esy/project--of-path (project-path) 
  "Returns an abstract structure that can later
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
	  'path (let* ((manifest-path (esy/internal--project--get-manifest-file-path esy-status-json)))
		  (if manifest-path
		      (file-name-directory manifest-path)
		    nil)))))

(defun esy/project--get-path (project)
  "Returns the root of the project"
  (plist-get project 'path))

(defun esy/project--get-manifest-file-path (project)
  "Returns the path to manifest file"
  (esy/internal--project--get-manifest-file-path
   (plist-get project 'json)))

(defun esy/project--of-file-path (file-path)
  "Returns an abstract structure that can
later be used to obtain more info about the esy project"
  (let* ((parent-path (file-name-directory file-path)))
    (progn
      (when (not
	     (file-directory-p parent-path))
	(make-directory parent-path)
	(message (format "esy-mode just created %s for you. If this is annoying, please raise a ticket." parent-path))
      (esy/project--of-path parent-path)))))

(defun esy/project--of-buffer (buffer)
  "Returns an abstract structure that can 
later be used to obtain more info about the esy project"
  (let* ((file-name (buffer-file-name buffer))) (if file-name (esy/project--of-file-path file-name) nil)))

(defun esy/project--ready-p (project)
  "Returns if a given project is ready for
development ie. if the tools can be looked in it's sandbox"
  (gethash "isProjectReadyForDev" (plist-get project 'json)))

(defun esy/project--p (project) 
  "Returns if a given project structure is a valid esy project"
  (gethash "isProject" (plist-get project 'json)))

(defun esy/command-env--of-project (project)
  "Given a project, it returns an abstract structure
command-env"
  (let*
      ((project-path (esy/project--get-path project))
       (default-directory project-path)
       (json-str
	(condition-case
	    nil
	    (shell-command-to-string
	     "esy command-env --json")
	  (error (progn
		   (message
		    "Error while running 'esy command-env --json'")
		   "{}"))))
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json-false 'nil)
	 (json-object-type 'hash-table)
	 (esy-command-env-json
	  (json-read-from-string json-str)))
    (list 'command-env esy-command-env-json)))

(defun esy/command-env--to-process-environment (command-env)
  "Given a command-env, it turns it into a list
that can be assigned to 'process-environment"
  (let ((command-env-json
	 (plist-get command-env 'command-env))
	(penv '()))
    (progn
      (maphash (lambda (k v)
		 (setq penv (cons (format "%s=%s" k v) penv)))
	       command-env-json)
      penv)))

(defun esy/command-env--get-exec-path (command-env)
  "Given a command-env, it turns it into a list that
can be assigned to 'exec-path"
  (let* ((penv
	  (esy/command-env--to-process-environment
	   command-env))
	 (exec-path-list '())
	 (path-env-str-list (seq-filter
			(lambda (s) (string-match "^path" s))
			penv)))
    (progn
      (dolist (e path-env-str-list)
	(let* ((parts (split-string e "=")))
	  (progn
	    (if (eq (length parts) 2)
		(progn
		  (setq exec-path-list
			(split-string
			 (nth 1 parts)
			 (if (string=
			      system-type
			      "windows-nt") ";" ":"))))))))
      exec-path-list)))

(defun esy/setup--esy-get-available-tools (project)
  
  "setup--esy-return-missing-tools(project): looks into the 
esy sandbox and returns a plist of missing tools. Specifically,
it looks for 

1. refmt/ocamlfmt
2. lsp
3. merlin

"
  (let* ((command-env (esy/command-env--of-project project))
	(tools '()))
    (progn
      (make-local-variable 'process-environment)
      (setq process-environment
	    (esy/command-env--to-process-environment
	     command-env))
      (make-local-variable 'exec-path)
      (setq exec-path
	    (esy/command-env--get-exec-path command-env))
      (setq tools (plist-put tools 'build "esy"))
      (setq tools (plist-put tools 'refmt (executable-find "refmt")))
      (setq tools (plist-put tools 'merlin (executable-find "ocamlmerlin")))
      (setq tools (plist-put tools 'lsp (executable-find "ocamllsp")))
      tools)))

(defun esy/setup--esy (project callback)
  "setup--esy(project): runs ops to ensure project is ready
for development"
  (if (esy/project--ready-p project)
      (progn
	(message "Project ready for development")
	(funcall callback
		 (esy/setup--esy-get-available-tools project)))
    (progn
	  (add-hook
	   'compilation-finish-functions
	   (lambda (buffer desc)
	     (funcall callback (esy/setup--esy-get-available-tools project))))
	  (compile "esy"))))

(defun esy/setup--opam (project callback)
  "setup--opam(_): currently doesn't do anything. opam-user-setup works well enough, IMO!"
  (message "Detected an opam project. Staying dormant"))

(defun esy/setup--npm(project callback)
  
  "setup--npm(project): Although named 'npm', this function uses esy to setup the Reason/OCaml toolchain.

npm is incapable of
  a) handling prebuilts correctly
  b) Correctly setup environment for tools that assume they are the only ones running. 
     Eg: merlin expected the correct ocamlmerlin-reason available on it's path. This can be tricky in non-sandboxed setup where a user could have almost any version of ocamlmerlin installed

"
  (if (y-or-n-p "Seems like an npm/bsb project. It is recommended that you we drop and esy.json for you. Go ahead?")
      (progn
	(esy/f--write
	 (concat
	  (file-name-as-directory
	   (esy/project--get-path project))
	  "esy.json")
	 "{
 \"dependencies\": {
    \"ocaml\": \"4.6.x\",
    \"@esy-ocaml/reason\": \"*\",
    \"@opam/ocaml-lsp-server\": \"ocaml/ocaml-lsp:ocaml-lsp-server.opam#e5e6ebf9dcf157\"
  }
}")
	(esy/setup--esy project callback))))

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
		   (let* ((project-type
			  (esy/package-manager--of-project
			   project))
			 (callback
			  (lambda (config-plist)
			    (progn
			      (setq-local esy-compile-command
					  (plist-get
					   config-plist
					   'build))
			      (setq-local esy-refmt-command
					  (plist-get
					   config-plist
					   'refmt))
			      (setq-local esy-merlin-command
					  (plist-get
					   config-plist
					   'merlin))
			      (setq-local esy-lsp-command
					  (plist-get
					   config-plist
					   'lsp))
			      ))))
		     (cond ((eq project-type 'opam)
			    (esy/setup--opam project
					     callback))
			    ((eq project-type 'esy)
			     (esy/setup--esy project
					     callback))
			    ((eq project-type 'npm)
			     (esy/setup--npm project
					     callback))))))
	      ))
	(message "Doesn't look like an esy project. esy-mode will stay dormant")))))

(provide 'esy-mode)
;;; esy.el ends here
