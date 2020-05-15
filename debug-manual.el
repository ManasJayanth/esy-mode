;; Nothing new here. Feel free to use this file like
;; a workspace
(load-file "./esy-mode.el")
(require 'esy-mode)

(setq foo-project (esy/project--of-path "/your/path/here"))
(setq foo-cmd-env (esy/command-env--of-project foo-project))

