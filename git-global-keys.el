;; See git-emacs.el for License information
(require 'easymenu)

(defcustom git-keyboard-prefix "\C-xg"
  "Keyboard prefix to use for global git keyboard commands."
  :type 'string
  :group 'git-emacs)

(define-prefix-command 'git-global-map)
(define-key global-map git-keyboard-prefix 'git-global-map)

(define-key git-global-map "a" 'git-add)
(define-key git-global-map "c" 'git-commit-all)

(define-prefix-command 'git--diff-buffer-map nil "Diff against")
(define-key git-global-map "d" 'git--diff-buffer-map)
(define-key git--diff-buffer-map "o" '("[o]ther" . git-diff-buffer-other))
(define-key git--diff-buffer-map "i" '("[i]ndex" . git-diff-buffer-index))
(define-key git--diff-buffer-map "b" '("[b]aseline" . git-diff-buffer-baseline))
(define-key git--diff-buffer-map "h" '("[H]ead" . git-diff-buffer-head))
(define-key git--diff-buffer-map (kbd "RET") 'git-diff-buffer-head)

(define-prefix-command 'git--diff-all-buffer-map nil "Diff repo against")
(define-key git-global-map "D" 'git--diff-all-buffer-map)
(define-key git--diff-all-buffer-map "o" '("[o]ther" . git-diff-all-other))
(define-key git--diff-all-buffer-map "i" '("[i]ndex" . git-diff-all-index))
(define-key git--diff-all-buffer-map "b"
  '("[b]aseline" . git-diff-all-baseline))
(define-key git--diff-all-buffer-map "h" '("[H]ead" . git-diff-all-head))
(define-key git--diff-all-buffer-map (kbd "RET") 'git-diff-all-head)

(define-key git-global-map "i" 'git-add-interactively)

(define-key git-global-map "l" 'git-log)
(define-key git-global-map "L" 'git-log-all)

(define-key git-global-map "s" 'git-status)
(define-key git-global-map "." 'git-cmd)

(easy-menu-add-item nil '("tools" "vc") "---")
(easy-menu-add-item nil '("tools" "vc")
  `("Git"
    ("Add to Index"
     ["Current File" git-add t]
     ["Select Changes in Current File..." git-add-interactively t]
     ["New Files..." git-add-new t])
    ["Commit All" git-commit-all t]
    ("Diff Current Buffer against"
      ["HEAD" git-diff-buffer-head t]
      ["Index" git-diff-buffer-index t]
      ["Baseline" git-diff-buffer-baseline t]
      ["Other..." git-diff-buffer-other t]
      )
    ("Diff Repository against"
     ["HEAD" git-diff-all-head t]
     ["Index" git-diff-all-index t]
     ["Baseline" git-diff-all-baseline t]
     ["Other..." git-diff-all-other t])
    ["Log for Current File" git-log t]
    ["Log for Entire Project" git-log-all t]
    ["Status" git-status t]
    ["Git Command..." git-cmd t]))


(provide 'git-global-keys)
