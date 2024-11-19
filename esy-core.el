;;; esy-core --- core library for minor mode and esy.el. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Manas Jayanth

;; Author: Manas Jayanth <prometheansacrifice@gmail.com>
;; Created: 19 November 2024
;; Keywords: Reason, OCaml
;; Package-Requires: ((emacs "25.1"))
;; Homepage: http://example.com/foo

;;; Commentary:

;;; Change Log: TODO

;;; Code:

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

;;;;;;;;;;;;;;;;;;; esy/status--* defuns ;;;;;;;;;;;;;;;;;;;;;;;
(defun esy/status--get-manifest-file-path (esy-status)
  "Given the json object of \'esy status\' output,
it returns the manifest file"
  (gethash "rootPackageConfigPath" esy-status))

(defun esy/status--dependencies-installed-p (esy-status)
  "Given the json object of \'esy status\' output,
it returns if dependencies have been installed."
  (gethash "isProjectFetched" esy-status))

(defun esy/status--dependency-constraints-solved-p (esy-status)
  "Given the json object of \'esy status\' output,
it returns if dependencies have been installed."
  (gethash "isProjectSolved" esy-status))

(defun esy/status--ready-for-dev-p (esy-status)
  "Given the json object of \'esy status\' output,
it returns if dependencies have been installed."
  (gethash "isProjectReadyForDev" esy-status))

(defun esy/status--project-p (esy-status)
  "Given the json object of \'esy status\' output,
it returns if dependencies have been installed."
  (gethash "isProject" esy-status))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'esy-core)
