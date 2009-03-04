;; See git-emacs.el for License information

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

(define-key git-global-map "s" '("Status" . git-status))

(provide 'git-global-keys)
