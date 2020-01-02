(load-file "../esy-mode.el")
(require 'esy-mode)

;; Test utils

(defun ert/test-suite 
    (&rest 
     test-config
     ) 
  (let ((setup (plist-get test-config 
			  :setup)) 
	(body (plist-get test-config 
			 :body)) 
	(tear-down (plist-get test-config 
			      :tear-down))) 
    (progn (let* ((setup-result (funcall setup))) 
	     (funcall body setup-result) 
	     (funcall tear-down setup-result)))))


(ert-deftest 
    test-add-two
    ()
  "Add two must do so"
  (should (equal (add-two 5) 7)))

(ert-deftest 
    test-esy/project--utils-for-a-valid-project
    ()
  "Tests esy/project--* utils"
  (ert/test-suite :setup (lambda () 
			   (let* ((tmp-dir (getenv "TMPDIR")) 
				  (test-esy-project-dir (concat (file-name-as-directory tmp-dir)
								"test-esy-project"))
				  (test-esy-project-manifest (concat (file-name-as-directory test-esy-project-dir) "esy.json")))
			     (progn (delete-directory test-esy-project-dir t)
				    (make-directory test-esy-project-dir)
				    (with-temp-file test-esy-project-manifest (insert "{ \"dependencies\": {} }")) test-esy-project-dir)))
		  :body (lambda (fixture-project-path)
			  (let* ((project (esy/project--of-path fixture-project-path)))
							(should (esy/project--p project))
							(should (not (esy/project--ready-p project)))))
		  :tear-down (lambda (fixture-project-path)
			       )))
(ert-deftest 
    test-esy/project--utils-for-invalid-project
    ()
  "Tests esy/project--* utils"
  (ert/test-suite :setup (lambda () 
			   (let* ((tmp-dir (getenv "TMPDIR")) 
				  (test-esy-project-dir (concat (file-name-as-directory tmp-dir)
								"test-esy-project-failing")))
			     (progn (delete-directory test-esy-project-dir t)
				    (make-directory test-esy-project-dir)
				     test-esy-project-dir)))
		  :body (lambda (fixture-project-path)
			  (let* ((project (esy/project--of-path fixture-project-path)))
			    (should (not (esy/project--p project)))
			    (should (not (esy/project--ready-p project)))))
		  :tear-down (lambda (fixture-project-path)
			       (delete-directory fixture-project-path t))))
