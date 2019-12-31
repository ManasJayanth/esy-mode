(load-file "../esy-mode.el")
(require 'esy-mode)

(ert-deftest 
    test-add-two
    ()
  "Add two must do so"
  (should (equal (add-two 5) 7)))

(ert-deftest 
    test-esy/project--utils
    ()
  "Tests esy/project--* utils"
  (let* ((project (esy/project--of-path "/Users/manas/development/esy/esy")))
    (should (esy/project--p project))
    (should (esy/project--ready-p project))))
