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


;; aio utils
;; taken from https://github.com/skeeto/emacs-aio/issues/19#issuecomment-729323058
(aio-defun foo-aio-call-process (program buffer &rest args)
  (let ((process (apply #'start-process program buffer program args))
        (promise (aio-promise)))
    (prog1 promise
      (setf (process-sentinel process)
            (lambda (_ status) (aio-resolve promise (lambda () 9999)))))))

(aio-wait-for (foo-aio-call-process "esy" "*foo*" "status"))

(provide 'esy-utils)
