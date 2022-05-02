;;; esy.el --- Minor mode for esy, the package manager. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Manas Jayanth

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

(defvar esy-command "esy"
  "The 'esy' command. Can be full path to the esy binary.")

(defvar esy-mode-callback (lambda (&optional project-type) (message (format "%s project ready for development" project-type)))
  "The callback that can be run once an esy project is initialised.
Common use case is to enable ask lsp client to connect to the server
(since this can only be done after the esy project is ready)")

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
	 (json-str (shell-command-to-string (concat esy-command " status")))
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json-false 'nil)
	 (json-object-type 'hash-table)
	 (esy-status-json
	  (condition-case nil
	      (json-read-from-string json-str)
	    (error (progn
		     (message (format "Error while json parsing \
'esy status' -> %s" json-str))
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
	(make-directory parent-path t)
	(message (format "esy-mode just created %s for you. If this is annoying, please raise a ticket." parent-path)))
      (esy/project--of-path parent-path))))

(defun esy/project--of-buffer (buffer)
  "Returns an abstract structure that can
later be used to obtain more info about the esy project"
  (let* ((file-name (buffer-file-name buffer))) (if file-name (esy/project--of-file-path file-name) (esy/project--of-path default-directory))))

(defun esy/project--fetched-p (project)
  "Returns if a given project's sources have been solved and fetched. This
is necessary for commands like 'esy command-env', 'esy build-plan' etc to work."
  (gethash "isProjectFetched" (plist-get project 'json)))

(defun esy/project--ready-p (project)
  "Returns if a given project is ready for
development ie. if the tools can be looked in it's sandbox"
  (gethash "isProjectReadyForDev" (plist-get project 'json)))

(defun esy/project--p (project)
  "Returns if a given project structure is a valid esy project"
  (let ((esy-status-json (plist-get project 'json)))
    (when esy-status-json (gethash "isProject" esy-status-json))))

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
	     (concat esy-command " command-env --json"))
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
			(lambda (s) (string-match "^path$" s))
			penv)))
    (setq exec-path-list
			(split-string
			 (gethash "PATH" (nth 1 command-env))
			 (if (string=
			      system-type
			      "windows-nt") ";" ":")))))

(defun esy/setup--esy-get-available-tools (project)

  "setup--esy-return-missing-tools(project): looks into the
esy sandbox and returns a plist of missing tools. Specifically,
it looks for

1. refmt/ocamlfmt
2. lsp
3. merlin

"
  (let* ((tools '()))
      (setq tools (plist-put tools 'build "esy"))
      (setq tools (plist-put tools 'refmt (executable-find "refmt")))
      (setq tools (plist-put tools 'merlin (executable-find "ocamlmerlin")))
      (setq tools (plist-put tools 'lsp (executable-find "ocamllsp")))
  ))

(defun esy/setup--esy (project callback)
  "setup--esy(project): runs ops to ensure project is ready
for development"
  (if (esy/project--fetched-p project)
      (let* ((command-env (esy/command-env--of-project project)))
	  (setq process-environment
	    (esy/command-env--to-process-environment
	     command-env))
	  (setq exec-path
		(esy/command-env--get-exec-path command-env)))
    (if (y-or-n-p
	 "This project hasn't had it's dependencies fetched and built. Go ahead and do this first?")
	(run-esy
	 (list "i")
	 (lambda ()
	   (message "Project dependencies have been fetched. Building sandbox in the background")
	   (run-esy
	    (list "build-dependencies")
	    (lambda () (message "Project sandbox built!")))))))
  (if (esy/project--ready-p project)
      (progn
	(if (string= system-type "windows-nt")
            (setq find-program "esy b find" grep-program "esy b grep"))
	(funcall callback
		 (esy/setup--esy-get-available-tools project)))
    nil))

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
;;   (if (y-or-n-p "Seems like an npm/bsb project. It is recommended that you we drop and esy.json for you. Go ahead?")
;;       (progn
;; 	(esy/f--write
;; 	 (concat
;; 	  (file-name-as-directory
;; 	   (esy/project--get-path project))
;; 	  "esy.json")
;; 	 "{
;;  \"dependencies\": {
;;     \"ocaml\": \"4.6.x\",
;;     \"@esy-ocaml/reason\": \"*\",
;;     \"@opam/ocaml-lsp-server\": \"ocaml/ocaml-lsp:ocaml-lsp-server.opam#e5e6ebf9dcf157\"
;;   }
;; }")
  ;; 	(esy/setup--esy project callback)))

  nil)

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
	       (message (format "Failed to parse JSON at %s" file-path))
	       nil)))))
    (progn
      (message (format "File at %s doesn't appear to be a JSON. Non JSON manifest not supported yet" file-path))
      nil)))

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


(defun esy-mode-init ()
  "Initialises esy-mode with necessary config. Relies on global vars like esy-command esy-mode-callback"
 (make-local-variable 'process-environment)
 (make-local-variable 'exec-path)
 (if (file-exists-p esy-command)
     (let ((esy-bin-dir (file-name-directory esy-command)))
       (add-to-list 'exec-path esy-bin-dir)
       (setenv "PATH" (concat (getenv "PATH") (concat path-separator esy-bin-dir)))
 ))
 (not (not (executable-find "esy"))))

(defun esy-project-type (&optional file-path)
  "Returns type of project - 'esy | 'opam | 'npm"
  (let* ((project
	  (if file-path (esy/project--of-file-path file-path)
	    (esy/project--of-buffer (current-buffer)))))
    (esy/package-manager--of-project project)))

(defun run-cmd-legacy (buffer-name cmd-and-args &optional callback)
  "Run the cmd" 
  (interactive) 
  (lexical-let ((callback-lex callback))
    (let* ((output-buffer-name buffer-name) 
	   (process (apply #'start-process (car cmd-and-args) output-buffer-name (car cmd-and-args) (cdr cmd-and-args)))) 
     
      (if callback-lex (set-process-sentinel process (lambda (process sentinel-msg) (message sentinel-msg) (cond ((string= sentinel-msg "finished\n") (funcall callback-lex))))))
      (with-current-buffer (process-buffer process) 
	(require 'shell) 
	(shell-mode) 
	(set-process-filter process 'comint-output-filter)) 
      (switch-to-buffer output-buffer-name))))

(defun run-cmd (buffer-name cmd-and-args callback)
  (let ((compilation-buffer
		 (compilation-start (string-join cmd-and-args " ") 'compilation-mode)))
    (if (get-buffer "*esy*") nil (with-current-buffer compilation-buffer (rename-buffer buffer-name)))
    (add-hook
     'compilation-finish-functions
      (lambda (buf str)
	(funcall callback)))))

(defun esy/cmd-api (cmd-string)
  "Util to work with esy's CLI API"
  (string-trim
   (shell-command-to-string cmd-string)))

(defun esy-view-source (dependency)
  "Open dependency's source"
  (interactive "sDependency: ")
  (let* ((project (esy/project--of-buffer (current-buffer))))
    (if (esy/project--p project)
	(find-file (esy/cmd-api (format "%s -p %s echo #{%s.root}" esy-command dependency dependency)))
      (message (format "Current buffer (%s) is not a part of an esy project" (buffer-name current-buffer))))))

(defun esy-view-build-log (dependency)
  "Open dependency's build logs"
  (interactive (list (read-string "Dependency: " nil nil (concat "@" (thing-at-point 'symbol)))))
  (let* ((project (esy/project--of-buffer (current-buffer))))
    (if (esy/project--p project)
	(find-file (esy/cmd-api (format "%s -p %s echo #{%s.target_dir}.log" esy-command dependency dependency)))
      (message (format "Current buffer (%s) is not a part of an esy project" (buffer-name current-buffer))))))

(defun esy-view-build-dir (&optional dependency)
  "Opam a dependency's (if absent, root project's) build directory"
  (interactive (list (read-string "Dependency: " nil nil (concat "@" (thing-at-point 'symbol)))))
  (let* ((project (esy/project--of-buffer (current-buffer))))
    (if (esy/project--p project)
	(find-file (esy/cmd-api (format "%s -p %s echo #{%s.target_dir}" esy-command dependency dependency)))
      (message (format "Current buffer (%s) is not a part of an esy project" (buffer-name current-buffer))))))

(defun esy-pesy ()
  "Run esy pesy"
  (interactive)
  (run-esy (list "pesy") (lambda () (message "[esy] Ran esy pesy"))))

(defmacro plist-get-default (plist key default-value)
 `(let ((val (plist-get ,plist ,key)))
 (if val val ,default-value)))

(defmacro append-if-t (arg plist args)
  `(let ((val (plist-get ,plist ,arg)))
     (if
	 val
	 (let
	     ((arg-name (format "--%s" (string-trim (symbol-name ,arg) ":"))))
	   (append ,args `(,arg-name ,val)))
       ,args)))

(defun esy-install-menu-arguments nil
  "Arguments function for transient."
  (transient-args 'esy/cmd-install))

(defun esy/cmd-install (&optional args)
  "Run esy install"
  (interactive (list (esy-install-menu-arguments)))
  (print args)
  (run-esy (append '("install") args) (lambda () (message "[esy] Installed"))))

(defun esy/cmd-build ()
  "Run esy build"
  (interactive)
  (run-esy (list "build") (lambda () (message "[esy] Built"))))

(defun esy-add (dependency &optional dev-only)
  "Run esy add <dependency>"
  (interactive "sDependency: ")
  (run-esy (list "add" dependency) (lambda () (message "[esy] Added"))))

(defun esy-build-and-install ()
  "Run esy"
  (interactive)
  (run-esy '() (lambda () (message "[esy] Build and install done"))))

(defun esy-npm-release ()
  "Run esy npm-release"
  (interactive)
  (run-esy (list "npm-release") (lambda () (message "[esy]  NPM release done"))))

(define-transient-command esy-install ()
  "Open esy install transient menu pop up."
    ["Arguments"
     ("-p" "Package name providing the ocaml compiler"        "--ocaml-pkg-name=")
     ("-v" "OCaml compiler version"        "--ocaml-version=")
     ("-rl" "Local path to opam repository" "--opam-repository-local=")
     ("-rr" "HTTP url to remote opam repository" "--opam-repository-remote=")
     ("-ol" "Local path to opam override repository. For more info, see (TODO document this at esy.sh)" "--opam-override-repository-local=")
     ("-or" "HTTP url to remote opam override repository. For more info, see (TODO document this at esy.sh)" "--opam-override-repository-remote=")
    ]
    [["Command"
      ("i" "Install"       esy/cmd-install)]]
  (interactive)
  (transient-setup 'esy/cmd-install))

(define-transient-command esy-build ()
  "Open esy build transient menu pop up."
    ["Arguments"
     ("-p" "Package name providing the ocaml compiler"        "--ocaml-pkg-name")
     ("-v" " OCaml compiler version"        "--ocaml-version")
    ]
    [["Command"
      ("b" "Build"       esy-build)]]
  (interactive)
  (transient-setup 'esy/cmd-build))


(defun esy-test ()
  "Run esy test"
  (interactive)
  (run-esy (list "test") (lambda () (message "[esy] done"))))

(defun esy-run-script (command)
  "Run esy run-script. See https://esy.sh/docs/en/configuration.html#scripts"
  (interactive)
  (run-esy (list command) (lambda () (message "[esy] done"))))

;; Entrypoint menu
(define-transient-command esy-menu ()
  "Open esy transient menu pop up."
    [["Command"
      ("e" "Build and install"       esy-build-and-install)
      ("b" "Build"       esy-build)
      ("i" "Install"       esy-install)
      ("r" "Run Script"       esy-run-script)
      ("n" "Run npm-release"       esy-npm-release)
      ("t" "Test"       esy-test)
    ]]
  (interactive)
  (transient-setup 'esy-menu))

(defun esy ()
  "Entrypoint function to the esy-mode interactive functions
First checks if file backing the current buffer is a part of an esy project, then opens the menu. Else, recommends initialising a new project"
  (interactive)
  (let ((project (esy/project--of-buffer (current-buffer))))
  (if project (call-interactively #'esy-menu) (if (y-or-n-p "You are not in an esy project, would you like to initialize one? ")
          (call-interactively #'esy-init)))))

(defun run-esy (args callback)
  "Runs esy command in *esy* buffer"
  (let ((command (if args (append args esy-command) (list esy-command))))
  (run-cmd
   "*esy*"
   command
   (lambda ()
     (with-current-buffer
	 "*esy*"
       ; (make-local-variable 'compilation-directory-matcher) ; Buffer local not working :(
       (setq
	compilation-directory-matcher
	'("^\s+\\(# esy-build-package: pwd: \\| esy-build-package: exiting with errors above\\)\\([^\n]+\\)$" (2 . nil)))
       (callback))))))

(defun esy-init (project-directory)
  "Run esy"
  (interactive "sProject Directory: ")
  (run-cmd (list "pesy" "-d" project-directory) (lambda () (message "[esy] Finished"))))

(defun esy-dune-clean ()
  "Cleanup dune's build directory"
  (interactive)
  (run-esy (list "b" "dune" "clean") (lambda () (message "[esy] Finished"))))

(defun esy-project-is-ready?
    (file-path)
  "Given the path of a file in the project,
it returns if the project is ready for development"
  (let* ((project (esy/project--of-file-path file-path)))
    (esy/project--ready-p project)))

(defun esy-build-dependencies (project-directory)
  "Run esy build-dependencies"
  (interactive)
  (run-esy (list "build-dependencies") (lambda () (message "[esy] Finished"))))

;;;###autoload
(define-minor-mode esy-mode
  "Minor mode for esy - the package manager for Reason/OCaml"
  :lighter " esy"
  (if esy-mode
  (progn
    (if (esy-mode-init)
    (let* ((project (esy/project--of-buffer (current-buffer))))
      (if (esy/project--p project)
	  (progn
	    
	    ;; All npm and opam projects are valid esy projects
	    ;; too! Picking the right package manager is important
	    ;; - we don't want to run `esy` for a user who never
	    ;; intended to. Example: bsb/npm users. Similarly,
	    ;; opam users wouldn't want prompts to run `esy`. Why
	    ;; is prompting `esy i` even necessary in the first
	    ;; place? `esy ocamlmerlin-lsp` needs projects to
	    ;; install/solve deps

	    (let* ((project-type (esy/package-manager--of-project project)))
	      (cond ((eq project-type 'opam)
		     (esy/setup--opam
		      project
		      (lambda
			(config-plist)
			(funcall esy-mode-callback 'opam))))
		    ((eq project-type 'esy)
		     (esy/setup--esy
		      project
		      (lambda
			(config-plist)
			(funcall esy-mode-callback 'esy))))
		    ((eq project-type 'npm)
		     (esy/setup--npm
		      project
		      (lambda
			(config-plist)
			(funcall esy-mode-callback 'npm)))))))
	(message "Doesn't look like an esy project. esy-mode will stay dormant")))
     (message "esy command not found. Try 'npm i -g esy' or refer https://esy.sh")))))

(provide 'esy-mode)
;;; esy.el ends here
