(load-file "../esy-mode.el")
(require 'esy-mode)

;; Test utils
(defun esy-test-utils/f--write (fname data)
  (with-temp-file fname (insert data)))

(setq safe-guard nil)
(defun ert/test-suite 
    (&rest 
     test-config
     ) 
  (let ((setup (plist-get test-config 
			  :setup))
	(body (plist-get test-config 
			 :body))	
	(teardown (plist-get test-config 
			     :teardown))) 
    (progn (let* ((setup-result
		   (funcall setup (getenv "TMPDIR"))))
	     (if safe-guard
	     (condition-case err
		 (funcall body setup-result)
	    
	       (error (progn
			(funcall teardown setup-result)
			(signal (car err) (cdr err))
			)))
	     (funcall body setup-result))
	     (funcall teardown setup-result)))))

(defun esy-test-utils/fixture--create (tmp-dir)
  (let* ((test-esy-project-dir
	  (concat
	   (file-name-as-directory tmp-dir)
	   "test-esy-project"))
	 (test-esy-project-manifest
	  (concat
	   (file-name-as-directory test-esy-project-dir)
	   "esy.json")))
    (progn
      (delete-directory test-esy-project-dir t)
      (make-directory test-esy-project-dir)
      (esy-test-utils/f--write
       test-esy-project-manifest
       "{ \"dependencies\": {} }")
      test-esy-project-dir)))

(defun esy-test-utils/fixture--create-npm (tmp-dir)
  (let* ((test-esy-project-dir
	  (concat
	   (file-name-as-directory tmp-dir)
	   "test-esy-project"))
	 (test-esy-project-manifest
	  (concat
	   (file-name-as-directory test-esy-project-dir)
	   "package.json")))
    (progn
      (delete-directory test-esy-project-dir t)
      (make-directory test-esy-project-dir)
      (esy-test-utils/f--write
       test-esy-project-manifest
       "{ \"dependencies\": {} }")
      test-esy-project-dir)))

(defun esy-test-utils/fixture--create-opam (tmp-dir)
  (let* ((test-esy-project-dir
	  (concat
	   (file-name-as-directory tmp-dir)
	   "test-esy-project"))
	 (test-esy-project-manifest
	  (concat
	   (file-name-as-directory test-esy-project-dir)
	   "foo.opam")))
    (progn
      (delete-directory test-esy-project-dir t)
      (make-directory test-esy-project-dir)
      (esy-test-utils/f--write
       test-esy-project-manifest
       "name: \"foo\"")
      test-esy-project-dir)))

(ert-deftest 
    test-add-two
    ()
  "Add two must do so"
  (should (equal (add-two 5) 7)))

(ert-deftest
    test-esy/manifest--json-p
    ()
  "must return if file at path is json or not"
  (should (esy/manifest--json-p "/foo/bar/package.json"))
  (should (not (esy/manifest--json-p "/foo/opam")))
  (should (not (esy/manifest--json-p "/foo/foo.opam"))))

(ert-deftest
    test-esy/package-manager--of-project-when-esy
    ()
  "package-manager--of-project must return correct project type"
  (ert/test-suite
   :setup (lambda (tmp-dir) (esy-test-utils/fixture--create tmp-dir))
   :body (lambda (test-project-path)
	   (let ((test-project (esy/project--of-path test-project-path)))
	   (should (eq
		    (esy/package-manager--of-project test-project)
		    'esy))))
   :teardown (lambda (x) (delete-directory x t))))

(ert-deftest
    test-esy/package-manager--of-project-when-opam
    ()
  "package-manager--of-project must properly detect an opam project
with an opam file"
  (ert/test-suite
   :setup (lambda (tmp-dir)
	    (esy-test-utils/fixture--create-opam tmp-dir))
   :body (lambda (test-project-path)
	   (let ((test-project
		  (esy/project--of-path test-project-path)))
	   (should (eq
		    (esy/package-manager--of-project test-project)
		    'opam))))
   :teardown (lambda (x) (delete-directory x t))))

(ert-deftest
    test-esy/package-manager--of-project-when-npm
    ()
  "package-manager--of-project must properly detect an npm 
project with a package.json (but no esy field in it)"
  (ert/test-suite
   :setup (lambda
	    (tmp-dir)
	    (esy-test-utils/fixture--create-npm tmp-dir))
   :body (lambda
	   (test-project-path)
	   (let ((test-project
		  (esy/project--of-path test-project-path)))
	     (should (eq
		      (esy/package-manager--of-project
		       test-project)
		      'npm))))
   :teardown (lambda
	       (x)
	       (delete-directory x t))))

(ert-deftest
    test-esy/command-env--utils
    ()
  "command-env-* fns"
  (ert/test-suite
   :setup (lambda
	    (tmp-dir)
	    (let* ((test-project-path
		   (esy-test-utils/fixture--create tmp-dir))
		  (default-directory test-project-path))
	      (progn
		(shell-command "esy")
		default-directory)))
   :body (lambda
	   (test-project-path)
	   (let* ((test-project
		   (esy/project--of-path test-project-path))
		  (command-env
		   (esy/command-env--of-project test-project))
		  (penv
		   (esy/command-env--to-process-environment
		    command-env)))
	     (progn
	       (should penv)
	       (should (listp penv))
	       (dolist (e penv)
		 (progn
		   (should (stringp e))
		   (should (string-match "=" e))
		   (let* ((exec-path-list
			   (esy/command-env--get-exec-path
			    command-env)))
		     (progn
		       (should (listp exec-path-list)))))))))
   :teardown (lambda
	       (x)
	       (delete-directory x t))))

(ert-deftest
    test-esy/project--get-manifest-file-path
    ()
  "project--get-manifest-file-path must simply return the
/path/to/manifest.json"
  (ert/test-suite
   :setup (lambda (tmp-dir)
	    (esy-test-utils/fixture--create tmp-dir))
   :body (lambda (test-project-path)
	   (let* ((test-project
		   (esy/project--of-path test-project-path)))
	     (should
	      (string=
	       (esy/project--get-manifest-file-path
		test-project)
	       (concat
		(file-name-as-directory
		 (file-truename test-project-path))
		"esy.json")))))
   :teardown (lambda (x) (delete-directory x t))))
		      
(ert-deftest 
    test-esy/project--utils-for-a-valid-but-unsolved-project
    ()
  "Tests esy/project--* utils on trivial project (with no deps)"
  (ert/test-suite
   :setup (lambda (tmp-dir) 
	    (let* ((test-esy-project-dir
		    (concat
		     (file-name-as-directory tmp-dir)
		     "test-esy-project"))
		   (test-esy-project-manifest
		    (concat
		     (file-name-as-directory
		      test-esy-project-dir)
		     "esy.json")))
	      (progn 
		(make-directory test-esy-project-dir)
		(esy-test-utils/f--write
		 test-esy-project-manifest
		 "{ \"dependencies\": {} }")
		test-esy-project-dir)))
   :body (lambda (fixture-project-path)
	   (let* ((project
		   (esy/project--of-path
		    fixture-project-path)))
	     (should (esy/project--p project))
	     (should (not (esy/project--ready-p project)))))
   :teardown (lambda (fixture-project-path)
	       (delete-directory fixture-project-path t))))

(ert-deftest 
    test-esy/project--utils-for-a-reason-project
    ()
  "Tests esy/project--* utils on Reason project with refmt"
  (ert/test-suite
   :setup (lambda (tmp-dir) 
	    (let* ((test-esy-project-dir
		    (concat (file-name-as-directory tmp-dir)
			    "test-esy-reason-project"))
		   (test-esy-project-manifest
		    (concat
		     (file-name-as-directory
		      test-esy-project-dir)
		     "esy.json"))
		   (test-opam-file
		    (concat (file-name-as-directory
			     test-esy-project-dir)
			    "hello-reason.opam"))
		   (test-helloml-file
		    (concat (file-name-as-directory
			     test-esy-project-dir)
			    "hello.ml")))
	      (progn (delete-directory test-esy-project-dir t)
		     (make-directory test-esy-project-dir)
		     (esy-test-utils/f--write test-opam-file "")
		     (esy-test-utils/f--write test-esy-project-manifest "{ \"esy\": { \"buildInSource\": true, \"build\": \"dune build -name hello-reason\" }, \"dependencies\": { \"@opam/dune\": \"*\", \"@esy-ocaml/reason\": \"*\"} }")
		     (esy-test-utils/f--write test-helloml-file "let () = print_endline \"hello\"") test-esy-project-dir)))
   :body (lambda (fixture-project-path)
	   (let* ((project (esy/project--of-path fixture-project-path)))
	     (should (esy/project--p project))
	     (should (not (esy/project--ready-p project)))))
   :teardown (lambda (fixture-project-path) (delete-directory fixture-project-path t))))

(ert-deftest 
    test-esy/project--utils-for-an-ocaml-project
    ()
  "Tests esy/project--* utils on OCaml"
  (ert/test-suite
   :setup (lambda (tmp-dir) 
	    (let* ((test-esy-project-dir
		    (concat (file-name-as-directory tmp-dir)
			    "test-esy-ocaml-project"))
		   (test-helloml-file
		    (concat (file-name-as-directory
			     test-esy-project-dir)
			    "hello.ml"))
		   (test-esy-project-manifest
		    (concat
		     (file-name-as-directory
		      test-esy-project-dir)
		     "esy.json")))
	      (progn (delete-directory test-esy-project-dir t)
		     (make-directory test-esy-project-dir)
		     (esy-test-utils/f--write test-esy-project-manifest "{ \"esy\": { \"buildInSource\": true, \"build\": \"ocamlopt hello.ml\" }, \"dependencies\": { \"ocaml\": \"*\"} }")
		     (esy-test-utils/f--write test-helloml-file "let () = print_endline \"hello\"")
		     test-esy-project-dir)))
   :body (lambda (fixture-project-path)
	   (let* ((project
		   (esy/project--of-path
		    fixture-project-path)))
	     (should (esy/project--p project))
	     (should (not (esy/project--ready-p project)))))
   :teardown (lambda (fixture-project-path)
	       (delete-directory fixture-project-path t))))

(ert-deftest 
    test-esy/project--utils-for-invalid-project
    ()
  "Tests esy/project--* utils on an project with empty manifest"
  (ert/test-suite :setup (lambda (tmp-dir) 
			   (let* ((test-esy-project-dir (concat (file-name-as-directory tmp-dir)
								"test-esy-project-failing")))
			     (progn (delete-directory test-esy-project-dir t)
				    (make-directory test-esy-project-dir)
				     test-esy-project-dir)))
		  :body (lambda (fixture-project-path)
			  (let* ((project (esy/project--of-path fixture-project-path)))
			    (should (not (esy/project--p project)))
			    (should (not (esy/project--ready-p project)))))
		  :teardown (lambda (fixture-project-path)
			       (delete-directory fixture-project-path t))))
