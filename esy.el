;;; esy.el --- library to interact with esy CLI. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Manas Jayanth

;; Author: Manas Jayanth <prometheansacrifice@gmail.com>
;; Created: 11 November 2024
;; Keywords: Reason, OCaml
;; Package-Requires: ((emacs "25.1") (transient "0.3.7.50"))
;; Package-Version: 20230415
;; Homepage: http://example.com/foo

;;; Commentary:


;;; Change Log: TODO

;;; Code:
(require 'transient)

(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defconst esy-cli-mode-map compilation-mode-map)

(define-derived-mode esy-cli-mode compilation-mode "esy-cli"
  "Major mode for the NPM compilation buffer."
  (use-local-map esy-cli-mode-map)
  (setq major-mode 'esy-cli-mode)
  (setq mode-name "esy-cli")
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer 0 t)
  (setq-local truncate-lines t))

(defun run-cmd (buffer-name cmd-and-args callback)
  (let ((compilation-buffer
		 (compilation-start (string-join cmd-and-args " ") 'compilation-mode)))
    (if (get-buffer buffer-name) nil (with-current-buffer compilation-buffer (rename-buffer buffer-name)))))

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
	'("^\s+\\(# esy-build-package: pwd: \\| esy-build-package: exiting with errors above\\)\\([^\n]+\\)$" (2 . nil))))))))

(defun propertized-flag (flag-value)
  "Given a boolean value, it turns into a human readable 'yes' | 'no' with appropriate faces"
  (propertize (if flag-value "yes" "no") 'face (if flag-value '(:foreground "green") '(:foreground "red"))))

(defun esy-status ()
  "Show status (parsed from 'esy status') of the current buffer"
  (interactive)
  (esy/macro--with-esy-project
   (current-buffer)
   project
   (let* ((esy-status (plist-get project 'json))
	  (manifest (esy/status--get-manifest-file-path esy-status))
	  (manifest-propertized (propertize manifest 'face 'bold))
	  (is-project-propertized (propertized-flag (esy/status--project-p esy-status)))
	  (is-solved-propertized (propertized-flag (esy/status--dependency-constraints-solved-p esy-status)))
	  (is-fetched-propertized (propertized-flag (esy/status--dependencies-installed-p esy-status)))
	  (is-ready-for-dev-propertized (propertized-flag (esy/status--ready-for-dev-p esy-status))))
     (message
      "manifest: %s valid-project: %s solved: %s dependencies-fetched: %s ready-for-dev: %s"
      manifest-propertized is-project-propertized is-solved-propertized is-fetched-propertized is-ready-for-dev-propertized))))

(defun esy-pesy ()
  "Run esy pesy"
  (interactive)
  (run-esy (list "pesy") (lambda () (message "[esy] Ran esy pesy"))))

(defun esy/cmd-install (&optional args)
  "Run esy install"
  (interactive (list (transient-args 'esy-install)))
  (run-esy (append '("install") args) (lambda () (message "[esy] Installed"))))

(defun esy/cmd-build (&optional args)
  "Run esy build"
  (interactive (list (transient-args 'esy-build)))
  (run-esy (append '("build") args) (lambda () (message "[esy] Built"))))

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
     ("-p" "Package name providing the ocaml compiler"        "--ocaml-pkg-name=")
     ("-v" " OCaml compiler version"        "--ocaml-version=")
    ]
    [["Command"
      ("b" "Build"       esy/cmd-build)]])


(defun esy-test ()
  "Run esy test"
  (interactive)
  (run-esy (list "test") (lambda () (message "[esy] done"))))

;; TODO: minibuffer completions
;; It must autocomplete to only one of entries from
;; "scripts" field in package.json/esy.json
;; (defun esy-run-script (command)
;;   "Run esy run-script. See https://esy.sh/docs/en/configuration.html#scripts"
;;   (interactive)
;;   (run-esy (list command) (lambda () (message "[esy] done"))))

;; Entrypoint menu
(transient-define-prefix esy-menu ()
  "Open esy transient menu pop up."
    [["Command"
      ("e" "Build and install"       esy-build-and-install)
      ("s" "Status"       esy-status)
      ("b" "Build"       esy-build)
      ("i" "Install"       esy-install)
      ("n" "Run npm-release"       esy-npm-release)
      ("t" "Test"       esy-test)
    ]])

(defun esy-init (project-directory)
  "Run esy"
  (interactive "sProject Directory: ")
  (run-cmd "*esy-init*" (list "pesy" "-d" project-directory) (lambda () (message "[esy-init] Finished"))))

;;;###autoload
(defun esy ()
  "Entrypoint function to the esy-mode interactive functions
First checks if file backing the current buffer is a part of an esy project, then opens the menu. Else, recommends initialising a new project"
  (interactive)
  (let ((project (esy/project--of-buffer (current-buffer))))
    (if project
	(call-interactively #'esy-menu)
      (if (y-or-n-p "You are not in an esy project, would you like to initialize one? ")
          (call-interactively #'esy-init)))))

(provide 'esy)
