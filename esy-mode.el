;;; esy.el --- Minor mode for esy, the package manager. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Manas Jayanth

;; Author: Manas Jayanth <prometheansacrifice@gmail.com>
;; Created: 1 Jan 2020
;; Keywords: Reason, OCaml
;; Package-Requires: ((transient "0.3.7.50") (aio "1.0"))
;; Package-Version: 20230415
;; Homepage: http://example.com/foo
;; Package-Requires: ((emacs "25.1") (transient "0.3.6"))


;;; Commentary:

;; esy-mode looks up the tools in the project's sandbox and set's
;; the respective variables with the path. For instance, it looks
;; up refmt in a project's sandbox and set refmt-command to
;; "esy exec-command refmt" only if it is present (in an OCaml only
;; project it need not be available).


;; TIP: To test individual defuns, consider the following example;
;; (esy/process-env-to-exec-path
;;   (esy/opam--process-environment-of-project
;;     (esy/project--of-path "/Users/manas/development/ligolang/ligo")))

;;; Change Log: TODO

;;; Code:
(require 'json)
(require 'transient)
(require 'aio)

;; Errors
(define-error 'esy-error "Internal esy-mode error occurred" 'error)
(define-error 'file-from-source-cache-error "File provided is from esy's source cache and cannot be accepted" 'esy-error)

;; Customization
(defgroup esy nil
  "Manage esy configuration"
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "github"
		   "https://github.com/prometheansacrifice/esy-mode"))

(defvar esy-command "esy"
  "The \'esy\' command. Can be full path to the esy binary.")

(defvar esy-package-command "esy-package"
  "Command (that the default shell can resolve by itself, if full path
isn't provided) to package libraries not written in Reason/OCaml. Usually, C")

(defvar esy-mode-callback (lambda (&optional project-type) (message (format "%s project ready for development" project-type)))
  "The callback that can be run once an esy project is initialised.
Common use case is to enable ask lsp client to connect to the server
(since this can only be done after the esy project is ready)")

(defun esy--make-hash-table ()
  (make-hash-table :test 'equal))

(defun esy/f--read (file-path)
  "Return file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun esy/f--write (fname data)
  "Write to file"
  (with-temp-file fname (insert data)))

(defun esy/internal--persist-obj (obj file-path)
  "Persists object to file"
  (esy/f--write file-path (prin1-to-string obj)))

(defun esy/internal--read-obj (file-path)
  "Reads object from file"
  (eval (car (read-from-string (esy/f--read file-path)))))

(defun esy/project--persist (project)
  "Persist project indexed by path"
  (let* ((project-db-name "esy-projects.db")
	 (project-db-path (concat "~/.emacs.d/" project-db-name))
	 (db (condition-case
		 nil
		 (esy/internal--read-obj project-db-path)
	       (error (esy--make-hash-table))))
	 (project-path (esy/project--get-path project)))
    (puthash project-path project db)
    (esy/internal--persist-obj db project-db-path)))

(defun esy/project--read-db (project-path)
    "Load a project"
  (let* ((project-db-name "esy-projects.db")
	 (project-db-path (concat "~/.emacs.d/" project-db-name))
	 (db (condition-case
		 err
		 (esy/internal--read-obj project-db-path)
	       (error (princ (format "The error was: %s" err)) (esy--make-hash-table)))))
    (gethash project-path db)))

(defun esy/internal-status--get-manifest-file-path (esy-status)
  "Given the json object of \'esy status\' output,
it returns the manifest file"
  (gethash "rootPackageConfigPath" esy-status))

(defun esy/internal-status--get-project-root (esy-status)
  "Given the json object of \'esy status\' output,
it returns the manifest file"
  (file-name-directory (esy/internal-status--get-manifest-file-path esy-status)))

(defun esy/utils--default-if-nil (expr default-value)
  "Returns `default-value' if `expr' is nil"
  (if expr expr default-value))

(defun esy/internal--get-prefix-path ()
  "Return's esy's prefix path (where the store and caches can be found"
  (esy/utils--default-if-nil (getenv "ESY__PREFIX") (expand-file-name "~/.esy")))

(defun esy/utils--parent-path (path)
  "Given a path, returns it's parent path"
  (if (equal "/" path)
      "/"
    (directory-file-name (file-name-directory (directory-file-name path)))))

(defun esy/internal--is-file-from-source-cache (path)
  "Given a file path, determines if the file is from esy' source cache. It makes very
little sense to load the esy sandbox there. This scenario is encountered when a package's
sources are viewed. Unfortunately, this mean, not editor tooling. Perhaps, in future, a
global toolchain could be loaded"
  (let ((expanded-path (expand-file-name path))
	(home (expand-file-name "~")))
    (if (string-equal expanded-path (esy/internal--get-prefix-path))
	t
      (if (string-equal expanded-path home)
	  nil
	(esy/internal--is-file-from-source-cache (esy/utils--parent-path expanded-path))))))


(defun esy/internal-buffer-file-name (buffer)
  "Wrapper around Emacs' buffer-file-name to catch file names that are in esy's source cache"
  (let ((file (buffer-file-name buffer)))
    (if file
	(if (esy/internal--is-file-from-source-cache file) (signal 'file-from-source-cache-error file)))
    file))

(defun esy/internal--cwd-of-buffer (buffer)
  "Given buffer, finds tries to find the cwd of the file attached to the buffer.
Returns nil, if it fails"
  (let* ((file-name (esy/internal-buffer-file-name buffer)))
    (if file-name (file-name-directory file-name) nil)))

(defun esy/internal--root-of-cwd (cwd)
  "Given current working directory, get\'s project root using \'esy status\'
command"
  (let* ((esy-status (esy/internal--esy-status cwd)))
    (esy/internal-status--get-project-root esy-status)))

(defun esy/internal--cwd-of-buffer-or-default (buffer)
  "Same as esy/internal--cwd-of-buffer, but returns default-directory if cwd of
attached buffer could not be found"
  (let ((cwd (esy/internal--cwd-of-buffer buffer)))
    (if cwd cwd default-directory)))

(defun esy/internal--esy-status (cwd)
  "Given a working directory path (default or a buffer's file directory),
returns project root"
  (let* ((default-directory cwd)
	 (json-str (shell-command-to-string (concat esy-command " status")))
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json-false 'nil)
	 (json-object-type 'hash-table))
    (condition-case nil
	(json-read-from-string json-str)
      (error (progn
	       (message (format "Error while json parsing \
'esy status' -> %s" json-str))
	       (esy--make-hash-table))))))

(defun esy/internal--esy-status-of-buffer (buffer)
  "Returns 'esy status' output for a project associated with the given buffer"
  (let* ((cwd (esy/internal--cwd-of-buffer-or-default buffer)))
    (esy/internal--esy-status cwd)))

(defun esy/project--of-path (project-path)
  "Returns an abstract structure that can later
be used to obtain more info about the project"
  (let* ((esy-status-json (esy/internal--esy-status project-path))
	 (manifest-path
	  (esy/internal-status--get-manifest-file-path esy-status-json))
	 (project-path (if manifest-path (file-name-directory manifest-path)
		    (read-file-name "Couldn't detect project root. Enter project root (where opam or esy manifests are present): "  (file-name-as-directory default-directory)))))
    (list 'json esy-status-json
	  'usable 'not-solved ;; | 'solved-not-fetched | 'fetched-not-built | 'built-and-ready
	  'path project-path
	  'type (esy/internal-package-manager--of-project manifest-path))))

(defun esy/project--of-cwd (project-path)
  "Alias for esy/project--of-path"
  (esy/project--of-path project-path))


;; Getters and setters for type project
(defun esy/project--get-path (project)
  "Returns the root of the project"
  (plist-get project 'path))

(defun esy/project--get-type (project)
  "Returns type (npm|opam|esy) of project"
  (plist-get project 'type))

(defun esy/project--get-manifest-file-path (project)
  "Returns the path to manifest file"
  (esy/internal-status--get-manifest-file-path
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
      (esy/project--of-cwd parent-path))))

(defun esy/project--of-buffer (buffer)
  "Returns an abstract structure that can
later be used to obtain more info about the esy project"
  (let* ((file-name (esy/internal-buffer-file-name buffer)))
    (if file-name
	(esy/project--of-file-path file-name)
      (esy/project--of-path default-directory))))

(defun esy/cached-project--of-buffer (buffer)
  "Looks up the project db first, then call esy/project--of-buffer if necessary"
  (let* ((project-root (esy/internal--root-of-cwd (esy/internal--cwd-of-buffer buffer)))
	(cached-project (esy/project--read-db project-root)))
    (if cached-project cached-project (esy/project--of-buffer buffer))))

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
	    err
	    (shell-command-to-string
	     (concat esy-command " command-env --json"))
	  (error (progn
		   (debug err)
		   (message "Error while running 'esy command-env --json' %s" (error-message-string err))
		   "{}"))))
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json-false 'nil)
	 (json-object-type 'hash-table)
	 (esy-command-env-json
	  (json-read-from-string json-str)))
    (list 'command-env esy-command-env-json)))

(defun esy/opam--process-environment-of-project (project)
  "Given a project, it returns an abstract structure
representing opam env"
  (let*
      ((project-path (esy/project--get-path project))
       (default-directory project-path)
       ;; We use opam exec -- env and not opam env,
       ;; because, opam env returns values that are meant
       ;; to be executed by a shell like bash
       ;; Ex: OPAM_SWITCH_PREFIX='/Users/<user>/.opam/default'; export OPAM_SWITCH_PREFIX;
       ;; We just need key, value pairs.
       (env-str
	(condition-case
	    err
	    (shell-command-to-string
	     "opam exec -- env")
	  (error (progn
		   (debug err)
		   (message "Error while running 'opam exec -- env' %s" (error-message-string err))
		   "{}")))))
    (split-string env-str "\n")))

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

(defun esy/process-env-to-exec-path (penv)
  "Given a list of environment variables (ex: \'(\"PATH=/foo/bar\"
\"LDFLAGS=some_values\")\'), gets just exec-path" 
  (let* ((path-env-str-list
	  (seq-filter (lambda (s) (string-match "^path=" s)) penv))
	 (path-env-str-key-value (car path-env-str-list))
	 (path-env-str (nth 1 (split-string path-env-str-key-value "="))))
    (split-string path-env-str
     (if (string= system-type "windows-nt") ";" ":"))))

(defun esy/command-env--get-exec-path (command-env)
  "Given a command-env, it turns it into a list that
can be assigned to \'exec-path"
  (let* ((penv
	  (esy/command-env--to-process-environment
	   command-env)))
    (setq exec-path-list (esy/process-env-to-exec-path penv))))

(defun esy/setup--esy-get-available-tools ()

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
  (message "Detected an opam project. Experimental support.")
  (setq process-environment
	(esy/opam--process-environment-of-project project))
  (setq exec-path (esy/process-env-to-exec-path process-environment))
  (funcall callback '()))


(defun esy/setup--npm()

  "setup--npm(project): Although named \'npm\', this function uses esy to setup
the Reason/OCaml toolchain.

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

(defun esy/internal-package-manager--of-project (manifest-file-path)
  "Detect the package manager of the project. Returns either
'esy|'opam|'npm. Note, manifest-file-path is expected to be either an opam file
or json. This assumes that this value comes from `esy status`'s output"
  (if (esy/manifest--json-p manifest-file-path)
      ;; The manifest file is a json.
      (if (esy/manifest--package-json-p
	   manifest-file-path)
	  ;; Could be npm or esy
	  ;; Checking if there is an esy field in
	  ;; the package.json. If there is one,
	  ;; it's an esy project
	  (if (esy/manifest--contains-esy-field-p
		   (esy/manifest--of-path manifest-file-path))
		  'esy
		'npm)
	;; Previously, we believed the following,
	;; > esy says this project with package.json
	;; > is ready for development i.e. all it's
	;; > dependencies were fetched and installed
	;; > by esy. Definitely an esy project
	;; Should we reconsider this?
	(if (esy/manifest--contains-esy-field-p
	     (esy/manifest--of-path manifest-file-path))
	    'esy
	  nil))
    'opam))


(defun esy-mode-init ()
  "Initialises esy-mode with necessary config. Relies on global vars like
esy-command esy-mode-callback"
 (make-local-variable 'process-environment)
 (make-local-variable 'exec-path)
 (if (file-exists-p esy-command)
     (let ((esy-bin-dir (file-name-directory esy-command)))
       (add-to-list 'exec-path esy-bin-dir)
       (setenv "PATH" (concat (getenv "PATH") (concat path-separator esy-bin-dir)))
 ))
 (not (not (executable-find "esy"))))

(defun esy-project-type (&optional file-path)
  "Returns type of project - \'esy | \'opam | \'npm"
  (let* ((project
	  (if file-path (esy/project--of-file-path file-path)
	    (esy/project--of-buffer (current-buffer)))))
    (esy/project--get-type project)))

(defun run-cmd-legacy (buffer-name cmd-and-args &optional callback)
  "Run the cmd" 
  (interactive) 
  (lexical-let ((callback-lex callback))
    (let* ((output-buffer-name buffer-name) 
	   (process (apply #'start-process (car cmd-and-args) output-buffer-name (car cmd-and-args) (cdr cmd-and-args)))) 
     
      (if callback-lex (set-process-sentinel process (lambda (process sentinel-msg) (cond ((string= sentinel-msg "finished\n") (funcall callback-lex))))))
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

(transient-define-prefix esy-install ()
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
      ("i" "Install"       esy/cmd-install)]])

(transient-define-prefix esy-build ()
  "Open esy build transient menu pop up."
    ["Arguments"
     ("-p" "Package name providing the ocaml compiler"        "--ocaml-pkg-name")
     ("-v" " OCaml compiler version"        "--ocaml-version")
    ]
    [["Command"
      ("b" "Build"       esy-build)]])


(defun esy-test ()
  "Run esy test"
  (interactive)
  (run-esy (list "test") (lambda () (message "[esy] done"))))

(defun esy-run-script (command)
  "Run esy run-script. See https://esy.sh/docs/en/configuration.html#scripts"
  (interactive)
  (run-esy (list command) (lambda () (message "[esy] done"))))

;; Entrypoint menu
(transient-define-prefix esy-menu ()
  "Open esy transient menu pop up."
    [["Command"
      ("e" "Build and install"       esy-build-and-install)
      ("b" "Build"       esy-build)
      ("i" "Install"       esy-install)
      ("r" "Run Script"       esy-run-script)
      ("n" "Run npm-release"       esy-npm-release)
      ("t" "Test"       esy-test)
    ]])

(defun esy/internal--get-buffer-contents (buffer)
  "Returns contents of `buffer'"
  (with-current-buffer buffer (buffer-string)))

(defun esy/internal--pp-command-list (command)
  "Return a printable string representation of `command' which is usually,
a list of strings"
  (string-join command " "))

(defun esy/package--run (args)
  "Runs esy-package command in *esy-package* buffer"
  ;; I will use some kind of async/await macro library
  ;; here, to manage all the async code. Using `emacs-aio'
  ;; for now. https://github.com/skeeto/emacs-aio
  ;; See ~/notes/async-await-in-elisp.org
  (let* ((command (if args
		     (push esy-package-command args)
		    (list esy-package-command)))
	 (stdout-buffer (generate-new-buffer "*esy-package-stderr<todo-buffer-name>*"))
	 (stderr-buffer (generate-new-buffer "*esy-package-stderr<todo-buffer-name>*"))
	 (promise (aio-promise))
	 (make-std-buffers (lambda ()
			     (list :stdout (esy/internal--get-buffer-contents stdout-buffer)
				   :stderr (esy/internal--get-buffer-contents stderr-buffer))))
	 (signal-process-error (lambda ()
				 (signal
				  'error
				  (format
				  "Process %s didn't exit with status finished"
				  (esy/internal--pp-command-list command)))))
	 (sentinel-fn (lambda (process reason-str)
			(pcase reason-str
			  ("finished\n" (aio-resolve promise make-std-buffers))
			  (_  (aio-resolve promise signal-process-error))))))
    (prog1 promise
      (make-process :name "esy-package-<todo-buffer-name>"
		    :buffer stdout-buffer
		    :command command
		    :stderr stderr-buffer
		    :sentinel sentinel-fn))))



(aio-defun aio-run () )
(plist-get (aio-wait-for (aio-run)) :stdout)

(aio-defun esy-build-shell ()
  "Fetches package and loads the isolated build environment locally
in the buffer. Helps in preparing patches for and preparing NPM tarballs"
  (interactive)
  (let* ((workable-path (aio-await (esy-package--run '("fetch")))))))


(defun esy-package-fetch ()
  "Entrypoint defun to fetch a package tarball mentioned in the current manifest"
  (run-esy-package '("fetch")))

(defun esy ()
  "Entrypoint function to the esy-mode interactive functions
First checks if file backing the current buffer is a part of an esy project, then opens the menu. Else, recommends initialising a new project"
  (interactive)
  (let ((project (esy/project--of-buffer (current-buffer))))
    (if project
	(call-interactively #'esy-menu)
      (if (y-or-n-p "You are not in an esy project, would you like to initialize one? ")
          (call-interactively #'esy-init)))))

(defun run-esy (args callback)
  "Runs esy command in *esy* buffer"
  (let ((command (if args (push esy-command args) (list esy-command))))
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
	(condition-case
	 nil
	 (let* ((project (esy/cached-project--of-buffer (current-buffer))))
      (esy/project--persist project)
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

	    (let* ((project-type (esy/project--get-type project)))
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
	 (file-from-source-cache-error (message "File is from esy's source cache. Not doing anything")))
     (message "esy command not found. Try 'npm i -g esy' or refer https://esy.sh")))))

(provide 'esy-mode)
;;; esy.el ends here

;;; esy-mode.el ends here
