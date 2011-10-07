;; See git-emacs.el for project and license information.

;; Autoloads for git-emacs.
;;
;; Loads git-emacs in the following situations:
;;
;; 1) git-init
;; 2) git-status
;; 3) git-log-from-cmdline
;; 4) opening a git-controlled file

(eval-when-compile (require 'vc))
(eval-when-compile (require 'vc-git))
(add-to-list 'vc-handled-backends 'git)

(autoload 'git-status "git-status"
  "Launch git-emacs's status mode on the specified directory." t)
(autoload 'git-init             "git-emacs"
  "Initialize a git repository." t)
(autoload 'git-log-from-cmdline "git-log"
  "Launch a git log view from emacs --eval or gnuclient --eval")
(autoload 'git--update-modeline "git-emacs")

;; A couple of functions are needed to support autoload on opening a git file.
(defsubst git--in-vc-mode? ()
  "Returns true if the current buffer is under vc-git."
  
  (and vc-mode (string-match "^ Git" (substring-no-properties vc-mode))))

(defvar git-emacs-loaded nil)

;; vc-hook to check whether to load git-emacs or not
(defadvice vc-find-file-hook (after git--vc-git-find-file-hook activate)
  "vc-find-file-hook advice for synchronizing with vc-git interface"

  (when git-emacs-loaded 
    (git--uninstall-state-mark-modeline))
  
  (when (git--in-vc-mode?)
    (setq git-emacs-loaded t)
    (git--update-modeline)))

(provide 'git-emacs-autoloads)
