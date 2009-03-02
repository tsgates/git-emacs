;; See git-emacs.el for License information

(defcustom git-keyboard-prefix "\C-xg"
  "Keyboard prefix to use for git commands that apply either globally or
in buffers under version control. If changed, it will only apply to newly
opened buffers."
  :type 'string
  :group 'git-emacs)

(defvar git-file-commands-map
  ;; Normally attached to C-x g
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'git-add)
    (define-key map "c" #'git-commit-all)
    (define-key map "d" #'git-diff-current)
    (define-key map "s" #'git-status)
    map)
  "Keymap used in buffers under git version control, prefixed by
`git-keyboard-prefix'")

(defun git--install-file-commands()
  "Install `git-file-commands-map' in the current buffer"
  (define-key (current-local-map)
    git-keyboard-prefix git-file-commands-map))

;; Some commands useful during development
;; Install the keymap in this buffer
;; (git--install-file-commands)
;; Re-evaluate keymap
;; (progn (makunbound 'git-file-commands-map) (eval-buffer))

(provide 'git-keys)
