;;; esy-package.el --- library to interact with esy-package CLI -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Manas Jayanth

;; Author: Manas Jayanth <prometheansacrifice@gmail.com>
;; Created: 09 November 2024
;; Keywords: Reason, OCaml, esy
;; Package-Requires: ((emacs "25.1") (aio "1.0"))
;; Package-Version: 20230415
;; Homepage: http://example.com/foo

;;; Commentary:

;;; Change Log: TODO

;;; Code:
(aio-defun esy/package--run (args)
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



(aio-defun esy-package-build-shell ()
  "Fetches package and loads the isolated build environment locally
in the buffer. Helps in preparing patches for and preparing NPM tarballs"
  (interactive)
  (let* ((workable-path (aio-await (esy/package--run '("fetch")))))))


(defun esy-package-fetch ()
  "Entrypoint defun to fetch a package tarball mentioned in the current manifest"
  (run-esy-package '("fetch")))
