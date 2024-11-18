;;; esy-utils.el --- esy-mode's internal utils library. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Manas Jayanth

;;; Commentary:
;; Not for public consumption


;; Consider naming defuns in the format esy/util--<module-name>--<function-name>

;; fs utils
(defun esy/f--read (file-path)
  "Return file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun esy/f--write (fname data)
  "Write to file"
  (with-temp-file fname (insert data)))

;; system utils
(defconst esy/utils--windows? (eq system-type 'windows-nt))

;; path utils
(defun esy/utils--path--normalize (path)
  "Normalise slashes in the path. Useful for Windows portability"
  ;; We use file-truename to normalize paths
  ;; Fixes the tests like the following,
  ;;
  ;;  (string= "c:\\Users\\foo\\AppData\\Local\\Temp\\test-esy-project\\esy.json" "c:/Users/foo/AppData/Local/Temp/test-esy-project/esy.json")
  ;;
  (replace-regexp-in-string (regexp-quote "\\") "/" path nil 'literal))
  ;; Alternatively, we could try `file-truename' which works like the unix command
  ;; `realpath', but clearly more expensive with syscall overhead.
  ;;
  ;; (file-truename path)
  ;;

(defun esy/utils--path--parent (path)
  "Given a path, returns it's parent path"
  (if (equal "/" path)
      "/"
    (directory-file-name (file-name-directory (directory-file-name path)))))

;; buffer utils
(defun esy/utils--cwd-of-buffer (buffer)
  "Given a buffer, it returns the parent directory of it's backing
  file on disk, or returns `default-directory'. Always returns string
  path"
  (let* ((file-path (buffer-file-name buffer)))
    (if file-path
	(esy/utils--path--parent file-path)
      default-directory)))

;; shell command utils
(defun esy/cmd-api-sync (cmd-string)
  "Util to work with esy's CLI API"
  (string-trim
   (shell-command-to-string cmd-string)))


;; nullable type utils
(defun esy/utils--default-if-nil (expr default-value)
  "Returns `default-value' if `expr' is nil"
  (if expr expr default-value))

;; json utils
(defun esy/utils--json--parse (json-str)
  "Creates hash table representing the json string. Returns nil if JSON parsing fails."
  (let ((json-array-type 'list)
	(json-key-type 'string)
	(json-false 'nil)
	(json-object-type 'hash-table))
    (json-read-from-string json-str)))

(provide 'esy-utils)
