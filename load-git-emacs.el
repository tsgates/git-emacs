;; 
;; git-emacs delayed load
;; 
;; There are three cases requring to load git-emacs
;;  1. calling git-status
;;  2. calling git-init
;;  3. (usual) open files (automatically load if necessary)
;;  
;; case.1) when loading normal file : not load git-emacs
;; case.2) when loading git-controlled file : load git-emacs
;; 

(eval-when-compile (require 'vc))

(autoload 'git-status           "git-emacs" t)
(autoload 'git-init             "git-emacs" t)
(autoload 'git--update-modeline "git-emacs")

;; the only necessary function to check the file is under git
(defsubst git--in-vc-mode? ()
  "Check see if in vc-git is under vc-git"
  
  (and vc-mode (string-match "^ Git" (substring-no-properties vc-mode))))

;; vc-hook to check whether to load git-emacs or not
(defadvice vc-find-file-hook (after git--vc-git-find-file-hook activate)
  "vc-find-file-hook advice for synchronizing with vc-git interface"

  (when (git--in-vc-mode?) (git--update-modeline)))

(provide 'load-git-emacs)
