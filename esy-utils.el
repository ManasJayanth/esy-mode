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

(provide 'esy-utils)
