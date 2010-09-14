;; Global keys for git-emacs.
;;
;; See git-emacs.el for license and versioning.

(require 'easymenu)

(defcustom git-keyboard-prefix "\C-xg"
  "Keyboard prefix to use for global git keyboard commands."
  :type 'string
  :group 'git-emacs)

(define-prefix-command 'git-global-map)
(define-key global-map git-keyboard-prefix 'git-global-map)

(define-key git-global-map "a" 'git-add)
(define-key git-global-map "b" 'git-branch)

(define-prefix-command 'git--commit-map nil "Commit")
(define-key git-global-map "c" 'git--commit-map)
(define-key git--commit-map "f" '("[f]ile" . git-commit-file))
(define-key git--commit-map "i" '("[i]ndex" . git-commit))
(define-key git--commit-map "a" '("[a]ll" . git-commit-all))
(define-key git--commit-map (kbd "RET") 'git-commit-all)

(define-prefix-command 'git--diff-buffer-map nil "Diff against")
(define-key git-global-map "d" 'git--diff-buffer-map)
(define-key git--diff-buffer-map "o" '("[o]ther" . git-diff-other))
(define-key git--diff-buffer-map "i" '("[i]ndex" . git-diff-index))
(define-key git--diff-buffer-map "b" '("[b]aseline" . git-diff-baseline))
(define-key git--diff-buffer-map "h" '("[H]ead" . git-diff-head))
(define-key git--diff-buffer-map (kbd "RET") 'git-diff-head)

(define-prefix-command 'git--diff-all-map nil "Diff repo against")
(define-key git-global-map "D" 'git--diff-all-map)
(define-key git--diff-all-map "o" '("[o]ther" . git-diff-all-other))
(define-key git--diff-all-map "i" '("[i]ndex" . git-diff-all-index))
(define-key git--diff-all-map "b" '("[b]aseline" . git-diff-all-baseline))
(define-key git--diff-all-map "h" '("[H]ead" . git-diff-all-head))
(define-key git--diff-all-map (kbd "RET") 'git-diff-all-head)

(define-key git-global-map "g" 'git-grep)
(define-key git-global-map "h" 'git-stash)
(define-key git-global-map "i" 'git-add-interactively)

(define-key git-global-map "l" 'git-log)
(define-key git-global-map "L" 'git-log-files)
(define-key git-global-map "\C-l" 'git-log-other)

(define-key git-global-map "m" 'git-merge-next-action)

(define-key git-global-map "R" 'git-reset)

(define-key git-global-map "s" 'git-status)
(define-key git-global-map "." 'git-cmd)

(easy-menu-add-item nil '("tools" "vc") "---")
(easy-menu-add-item nil '("tools" "vc")
  `("Git"
    ("Add to Index"
     ["Current File" git-add t]
     ["Select Changes in Current File..." git-add-interactively t]
     ["New Files..." git-add-new t])
    ("Commit"
     ["All Changes" git-commit-all t]
     ["Index" git-commit t]
     ["Current File" git-commit-file t])
    ("Diff Current Buffer against"
      ["HEAD" git-diff-head t]
      ["Index" git-diff-index t]
      ["Baseline" git-diff-baseline t]
      ["Other..." git-diff-other t]
      )
    ("Diff Repository against"
     ["HEAD" git-diff-all-head t]
     ["Index" git-diff-all-index t]
     ["Baseline" git-diff-all-baseline t]
     ["Other..." git-diff-all-other t])
    ["Log for Entire Project" git-log t]
    ["Log for Current File" git-log-files t]
    ["Log for Branch or Tag..." git-log-other t]
    ["Branch List" git-branch t]
    ["Merge (start or continue)..." git-merge-next-action t]
    ["Reset..." git-reset t]
    ["Stash..." git-stash t]
    ["Status" git-status t]
    ["Grep..." git-grep t]
    ["Git Command..." git-cmd t]))


(provide 'git-global-keys)
