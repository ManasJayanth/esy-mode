;;; esy-mode --- Minor mode for esy, the package manager. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Manas Jayanth

;; Author: Manas Jayanth <prometheansacrifice@gmail.com>
;; Created: 1 Jan 2020
;; Keywords: Reason, OCaml
;; Package-Requires: ((emacs "25.1") (transient "0.3.7.50"))
;; Package-Version: 20230415
;; Homepage: http://example.com/foo

;;; Commentary:

;; esy-mode looks up the tools in the project's sandbox and set's
;; the respective variables with the path. For instance, it looks
;; up refmt in a project's sandbox and set refmt-command to
;; "esy exec-command refmt" only if it is present (in an OCaml only
;; project it need not be available).


;; TIP: To test individual defuns, consider the following examples
;;
;; To create esy/project type,
;;
;;   (esy/project--of-path "C:/Users/foo/development/esy/esy")
;;
;; To get buffer local 'exec-path for an esy project,
;; 
;; (esy/process-env-to-exec-path
;;   (esy/opam--process-environment-of-project
;;     (esy/project--of-path "C:/Users/foo/development/esy/esy")))

;; TIP: try to keep the APIs centered around buffers. Not files. Emacs
;; is designed to work with buffers more than files.


;;; Change Log: TODO

;;; Code:
(require 'json)

;; esy libraries
(require 'esy-utils)

;; macros
(defmacro esy/macro--with-esy-project (buffer binding-project exp)
  `(let ((project (esy/project--of-buffer ,buffer)))
    (if (esy/project--p project)
	(let ((,binding-project project)) ,exp)
      (message "Doesn't look like an esy project. esy-mode will stay dormant"))))

(defmacro let-esy-project (binding exp)
  `(let ((,(car binding) ,(cadr binding)))
     (if (esy/project--p ,(car binding))
	 ,exp
       (message "Doesn't look like an esy project. esy-mode will stay dormant"))))

;; Errors
(define-error 'esy-mode-error "Internal esy-mode error occurred" 'error)
(define-error 'esy-file-from-source-cache-error "File provided is from esy's source cache and cannot be accepted" 'esy-mode-error)
(define-error 'esy-error "Internal esy error occurred" 'error-mode-error)

;; Customization
(defgroup esy nil
  "Manage esy configuration"
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "github"
		   "https://github.com/prometheansacrifice/esy-mode"))

(defvar esy-command "esy"
  "The \'esy\' command. Can be full path to the esy binary.")

(defvar esy-mode-callback (lambda (&optional project-type) (message (format "%s project ready for development" project-type)))
  "The callback that can be run once an esy project is initialised.
Common use case is to enable ask lsp client to connect to the server
(since this can only be done after the esy project is ready)")

(defun esy/internal-status--get-manifest-file-path (esy-status)
  "Given the json object of \'esy status\' output,
it returns the manifest file"
  (gethash "rootPackageConfigPath" esy-status))

(defun esy/prompt--ask-for-project-root ()
  "Prompts user for project root"
  (let ((prompt-msg "Couldn't detect project root. Enter project root (where opam or esy manifests are present): ")
	(default-value (file-name-as-directory default-directory)))
    (read-file-name prompt-msg default-value)))

;;;;;;;;;;;;;;;;;;; esy/internal-package-manager--of-project ;;;;;;;;;;;;;;;;;;;;;;;
(defun esy/manifest--of-path (file-path)
  "Creates an abstract manifest structure given file path"
  (if (esy/manifest--json-p file-path)
      (condition-case nil
	  (esy/utils--json--parse (esy/f--read file-path))
	(json-error (signal 'user-error (format "Failed to parse JSON at %s" file-path))))
    (signal 'user-error (format "File at %s doesn't appear to be a JSON." file-path))))

(defun esy/manifest--json-p (file-path)
  "Takes a file path and returns if file at said path is
json or not"
  ;; Cheat!
  (if file-path (string-match "\.json$" file-path) nil))

(defun esy/manifest--package-json-p (file-path)
  "Takes a file path and returns if file at said path is
package.json or not"
  (if file-path (string-match "package\.json$" file-path) nil))

(defun esy/manifest--esy-json-p (file-path)
  "Takes a file path and returns if file at said path is
esy.json or not"
  (if file-path (string-match "esy\.json$" file-path) nil))

(defun esy/manifest--contains-esy-field-p (manifest)
  "Checks if a manifest structure contains esy field"
  (if manifest (gethash "esy" manifest) nil))

(defun esy/internal-package-manager--of-project (manifest-file-path)
  "Detect the package manager of the project. Returns either
'esy|'opam|'npm. Note, manifest-file-path is expected to be either an opam file
or json. This assumes that this value comes from `esy status`'s output"
  (if (esy/manifest--json-p manifest-file-path)
      ;; The manifest file is a json.
      (if (esy/manifest--esy-json-p
	   manifest-file-path)
	  'esy
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
	  nil)))
    'opam))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun esy/internal--get-prefix-path ()
  "Return's esy's prefix path (where the store and caches can be found"
  (esy/utils--default-if-nil (getenv "ESY__PREFIX") (expand-file-name "~/.esy")))

(defun esy/source-cache--contains-p (path)
  "Given a file path, signals `esy-file-from-source-cache-error' if the file is from esy' source cache. It makes very
little sense to load the esy sandbox there. This scenario is encountered when a package's
sources are viewed. See comment just because autoload for more context."
  (let ((expanded-path (expand-file-name path))
	(home (expand-file-name "~")))
    (if (string-equal expanded-path (esy/internal--get-prefix-path))
	(signal 'esy-file-from-source-cache-error path)
      ;; We used to compare if expanded-path is equal to home to figure if we
      ;; could stop traversing up
      ;;
      ;; (if (string-equal expanded-path home) ...)
      ;;
      ;; On WSL, however, this doesn't work because it still evaluates to Windows
      ;; native paths - eg, C:/Users/johndoe
      ;;
      ;; In any case, the popular way to stop traversal is to stop when (directory-file-name ...)
      ;; returns the same as it's input - ie when it has reached the root of the file system
      (let ((parent-path (esy/utils--path--parent expanded-path)))
	(if (string= parent-path path) nil (esy/source-cache--contains-p parent-path))))))

(defun esy/internal--esy-status (cwd)
  "Given a working directory path (default or a buffer's file directory),
returns project root"
  (esy/source-cache--contains-p cwd)
  (let* ((default-directory cwd)
	 (json-str (esy/cmd-api-sync (format "%s status"
					     esy-command))))
    (condition-case nil
	(esy/utils--json--parse json-str)
      (json-error (signal 'esy-error (format "Error while json parsing 'esy status' -> %s" json-str))))))

;; The following function doesn't try to guarantee non-existence
;; of an esy/npm/opam project. Because 'esy status' command works in
;; any path. So, we don't return nil signalling missing a project. We,
;; instead, ask the user for a project root, just in case we're
;; wrong. This could be re-visited. We need a good project use-case to
;; see if we infact need to be lenient and fallback to user input. If
;; we dont find a compelling reason to be so lenient, we could fail
;; hard. Considering this, we can remove the test for invalid project.
(defun esy/project--of-path (project-path)
  "Returns an abstract structure that can later
be used to obtain more info about the project"
  (let* ((esy-status-json (esy/internal--esy-status project-path))
	 (manifest-path
	  (esy/internal-status--get-manifest-file-path esy-status-json))
	 (project-path (if manifest-path
			   (file-name-directory manifest-path)
			 (esy/prompt--ask-for-project-root)))) 
    (list 'json esy-status-json
	  ;; TODO 'usable 'not-solved | 'solved-not-fetched | 'fetched-not-built | 'built-and-ready
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

(defun esy/project--of-file-path (file-path)
  "Returns an abstract structure that can
later be used to obtain more info about the esy project"
  (let* ((parent-path (file-name-directory file-path)))
    (esy/project--of-cwd parent-path)))

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
       (json-str (esy/cmd-api-sync (format "%s command-env --json" esy-command))))
    (condition-case nil (esy/utils--json--parse json-str)
      (json-error (signal 'esy-error (format "Error while json parsing 'esy command --json' -> %s" json-str))))))

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

(defun esy/command-env--to-process-environment (command-env-json)
  "Given a command-env, it turns it into a list
that can be assigned to 'process-environment"
  (let ((penv '()))
    (progn
      (maphash (lambda (k v)
		 (setq penv (cons (format "%s=%s" k v) penv)))
	       command-env-json)
      penv)))

(defun esy/process-env-to-exec-path (penv)
  "Given a list of environment variables (ex: \'(\"PATH=/foo/bar\"
\"LDFLAGS=some_values\")\'), gets just exec-path" 
  (let* ((path-env-str-list
	  (seq-filter (lambda (s) (string-match "^PATH=" s)) penv))
	 (path-env-str-key-value (car path-env-str-list))
	 (path-env-str (nth 1 (split-string path-env-str-key-value "="))))
    (split-string path-env-str
     (if esy/utils--windows? ";" ":"))))

(defun esy/command-env--get-exec-path (command-env)
  "Given a command-env, it turns it into a list that
can be assigned to \'exec-path"
  (let* ((penv
	  (esy/command-env--to-process-environment
	   command-env)))
    (esy/process-env-to-exec-path penv)))

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

(defun esy/setup--esy-setup-buffer-environment (project callback)
  "Helper to esy/setup--esy to setup buffer local environment"
  (if (esy/project--ready-p project)
      (progn
	(let* ((command-env (esy/command-env--of-project project)))
	  (message "setting process-environment")
	  (setq process-environment
		(esy/command-env--to-process-environment command-env))
	  (setq exec-path
		(esy/command-env--get-exec-path command-env)))
	(if esy/utils--windows?
            (setq find-program "esy b find" grep-program "esy b grep"))
	(funcall callback
		 (esy/setup--esy-get-available-tools)))
    (message "Project not ready for development! Please run esy")
    (funcall callback '())))

(defun esy/setup--esy (project callback)
  "setup--esy(project): runs ops to ensure project is ready
for development"
  (if (esy/project--fetched-p project)
      (esy/setup--esy-setup-buffer-environment project callback)
    (message "This project hasn't had it's dependencies fetched and built. Consider running esy")))

(defun esy/setup--opam (project callback)
  (message "Detected an opam project. Experimental support.")
  (setq process-environment
	(esy/opam--process-environment-of-project project))
  (setq exec-path (esy/process-env-to-exec-path process-environment))
  (funcall callback '()))

(defun esy/command--available-p ()
  "Check if esy command is available"
 (make-local-variable 'process-environment)
 (make-local-variable 'exec-path)
 (if (file-exists-p esy-command)
     (let ((esy-bin-dir (file-name-directory esy-command)))
       (add-to-list 'exec-path esy-bin-dir)
       (setenv "PATH" (concat (getenv "PATH") (concat path-separator esy-bin-dir)))
 ))
 (not (not (executable-find "esy"))))

;; Turns buffer to cwd (current working directory) and decides whether
;; to create project of file's parent path, or assume buffer has no
;; backing file and use `default-directory'.
;; esy/project--of-buffer -> esy/project--of-cwd
(defun esy/project--of-buffer (buffer)
  "Returns an abstract structure that can
later be used to obtain more info about the esy project"
  (let ((cwd (esy/utils--cwd-of-buffer buffer)))
    (esy/project--of-cwd cwd)))

;; Given a buffer, this minor mode must figure out the relevant esy
;; sandbox. Most of the time, such buffers are backed by files on
;; disks. Ocassionally, however, it could be
;;
;; 1. org-mode code snippets.
;; 2. Backed by a file from source cache, where esy sandboxes are not
;;    retained necessarily - user's are not expected to keep the built
;;    artifacts of a source cache entry.
;; 3. Untested, but possible useful if user creates a new reason/ocaml buffer but
;;    hasn't saved. Such a buffer could be having a `default-buffer'
;;    that the user intentionally used so that tools pick up the nearest
;;    esy sandbox manifest (esy.json/package.json)
;;
;; Side note: if they do, we could figure a way to load the cmt files
;; while users browse the implementation after the goto-def'd a
;; symbol.
;; But, this also needs build systems to generate .merlin files which
;; is not happening anytime soon. We could drop this idea of making
;; tools work with ml/re files from source cache for now.
;;

(defun esy/project--setup-opam (project)
  "Setup opam tools for `project'"
  (esy/setup--opam
   project
   (lambda
     (config-plist)
     (funcall esy-mode-callback 'opam))))

(defun esy/project--setup-esy (project)
  "Setup esy tools for `project'"
  (esy/setup--esy
   project
   (lambda
     (config-plist)
     (funcall esy-mode-callback 'esy))))

(defun esy/load-buffer-locals (buffer)
  "Detects if project uses esy or opam and sets up the buffer local variables"
  ;; All npm and opam projects are valid esy projects
  ;; too! Picking the right package manager is important
  ;; - we don't want to run `esy` for a user who never
  ;; intended to. Example: bsb/npm users. Similarly,
  ;; opam users wouldn't want prompts to run `esy`. Why
  ;; is prompting `esy i` even necessary in the first
  ;; place? `esy ocamlmerlin-lsp` needs projects to
  ;; install/solve deps
  (esy/macro--with-esy-project
   buffer
   project
   (let* ((project-type (esy/project--get-type project)))
     (cond ((eq project-type 'opam) (esy/project--setup-opam project))
	   ((eq project-type 'esy) (esy/project--setup-esy project))))))

(defun esy/project--run-setup (buffer)
  "Setup buffer locals for the esy/opam project and handle errors if any"
  (condition-case
      project-creation-error
      (esy/load-buffer-locals buffer)
    (esy-error (message (format "Internal: esy command failed. Reason %s" project-creation-error)))
    (esy-file-from-source-cache-error (message "File is from esy's source cache. Not doing anything"))))

(defun esy/minor-mode--main (buffer)
  "Run minor mode"
  (if (esy/command--available-p)
      (esy/project--run-setup buffer)
    (message "esy command not found. Try 'npm i -g esy' or refer https://esy.sh")))

;;;###autoload
(define-minor-mode esy-mode
  "Minor mode for esy - the package manager for Reason/OCaml"
  :lighter " esy"
  (if esy-mode (esy/minor-mode--main (current-buffer))))

(provide 'esy-mode)
;;; esy.el ends here

;;; esy-mode.el ends here
