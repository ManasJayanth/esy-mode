(load-file "../esy-mode.el")
(require 'esy-mode)

;; Test utils
(defun ert/f--write (fname data)
  (with-temp-file fname (insert data)))


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
    test-esy/project--utils-for-a-valid-but-unsolved-project
    ()
  "Tests esy/project--* utils on trivial project (with no deps)"
  (ert/test-suite :setup (lambda () 
			   (let* ((tmp-dir (getenv "TMPDIR")) 
				  (test-esy-project-dir (concat (file-name-as-directory tmp-dir)
								"test-esy-project"))
				  (test-esy-project-manifest (concat (file-name-as-directory test-esy-project-dir) "esy.json")))
			     (progn (delete-directory test-esy-project-dir t)
				    (make-directory test-esy-project-dir)
				    (ert/f--write test-esy-project-manifest "{ \"dependencies\": {} }")
				     test-esy-project-dir)))
		  :body (lambda (fixture-project-path)
			  (let* ((project (esy/project--of-path fixture-project-path)))
							(should (esy/project--p project))
							(should (not (esy/project--ready-p project)))))
		  :tear-down (lambda (fixture-project-path)
			       (delete-directory fixture-project-path t))))
(ert-deftest 
    test-esy/project--utils-for-a-reason-project
    ()
  "Tests esy/project--* utils on Reason project with refmt"
  (ert/test-suite :setup (lambda () 
			   (let* ((tmp-dir (getenv "TMPDIR")) 
				  (test-esy-project-dir (concat (file-name-as-directory tmp-dir)
								"test-esy-reason-project"))
				  (test-esy-project-manifest (concat (file-name-as-directory test-esy-project-dir) "esy.json"))
				  (test-opam-file (concat (file-name-as-directory test-esy-project-dir)
							  "hello-reason.opam"))
				  (test-helloml-file (concat (file-name-as-directory test-esy-project-dir)
							  "hello.ml")))
			     (progn (delete-directory test-esy-project-dir t)
				    (make-directory test-esy-project-dir)
				    (ert/f--write test-opam-file "")
				    (ert/f--write test-esy-project-manifest "{ \"esy\": { \"buildInSource\": true, \"build\": \"dune build -name hello-reason\" }, \"dependencies\": { \"@opam/dune\": \"*\", \"@esy-ocaml/reason\": \"*\"} }")
				    (ert/f--write test-helloml-file "let () = print_endline \"hello\"") test-esy-project-dir)))
		  :body (lambda (fixture-project-path)
			  (let* ((project (esy/project--of-path fixture-project-path)))
							(should (esy/project--p project))
							(should (not (esy/project--ready-p project)))))
		  :tear-down (lambda (fixture-project-path) (delete-directory fixture-project-path t))))
(ert-deftest 
    test-esy/project--utils-for-an-ocaml-project
    ()
  "Tests esy/project--* utils on OCaml"
  (ert/test-suite :setup (lambda () 
			   (let* ((tmp-dir (getenv "TMPDIR")) 
				  (test-esy-project-dir (concat (file-name-as-directory tmp-dir)
								"test-esy-ocaml-project"))
				  (test-esy-project-manifest (concat (file-name-as-directory test-esy-project-dir) "esy.json")))
			     (progn (delete-directory test-esy-project-dir t)
				    (make-directory test-esy-project-dir)
				    (ert/f--write test-esy-project-manifest "{ \"esy\": { \"buildInSource\": true, \"build\": \"ocamlopt hello.ml\" }, \"dependencies\": { \"ocaml\": \"*\"} }")
				    (ert/f--write "hello.ml" "let () = print_endline \"hello\"")
				     test-esy-project-dir)))
		  :body (lambda (fixture-project-path)
			  (let* ((project (esy/project--of-path fixture-project-path)))
							(should (esy/project--p project))
							(should (not (esy/project--ready-p project)))))
		  :tear-down (lambda (fixture-project-path)
			       (delete-directory fixture-project-path t))))
(ert-deftest 
    test-esy/project--utils-for-invalid-project
    ()
  "Tests esy/project--* utils on an project with empty manifest"
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
