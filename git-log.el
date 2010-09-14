;; Git log mode support, part of git-emacs
;;
;; See git-emacs.el for license information

(require 'log-view)
(require 'git-emacs)

;; Based off of log-view-mode, which has some nice functionality, like
;; moving between comits
(define-derived-mode git-log-view-mode
  log-view-mode "Git-Log" "Major mode for viewing git logs"
  :group 'git
  ;; Customize log-view-message-re to be the git commits
  (set (make-local-variable 'log-view-message-re)
       "^[Cc]ommit[: ]*\\([0-9a-f]+\\)")
  ;; As for the file re, there is no such thing -- make it impossible
  (set (make-local-variable 'log-view-file-re)
       "^No_such_text_really$")
  (set (make-local-variable 'font-lock-defaults)
       (list 'git-log-view-font-lock-keywords t))
  (set (make-local-variable 'transient-mark-mode) t)

  ;; A long git log might still be running when we die. Avoid "deleted buffer".
  (add-hook 'kill-buffer-hook
            #'(lambda()
                (let ((proc (get-buffer-process (current-buffer))))
                  (when proc (delete-process proc))))
            nil t)                      ; prepend, local
  )


;; Highlighting. We could allow customizable faces, but that's a little
;; much right now.
(defvar git-log-view-font-lock-keywords
  '(("^\\([Cc]ommit\\|[Mm]erge\\):?\\(.*\\)$"
     (1 font-lock-keyword-face prepend)
     (2 font-lock-function-name-face prepend))
    ("^\\(Author\\):?\\(.*?\\([^<( \t]+@[^>) \t]+\\).*\\)$"
     (1 font-lock-keyword-face prepend) (2 font-lock-constant-face prepend)
     (3 font-lock-variable-name-face prepend))
    ("^\\(Date\\):?\\(.*\\)$"
     (1 font-lock-keyword-face prepend) (2 font-lock-doc-face prepend))
    )
  "Font lock expressions for git log view mode")
;; (makunbound 'git-log-view-font-lock-keywords)  ; <-- C-x C-e to reset


;; Keys
(let ((map git-log-view-mode-map))
  (define-key map "N" 'git-log-view-interesting-commit-next)
  (define-key map "P" 'git-log-view-interesting-commit-prev)

  (define-key map "m" 'set-mark-command) ; came with log-view-mode, nice idea
  (define-key map "d" 'git-log-view-diff-preceding)
  (define-key map "D" 'git-log-view-diff-current)
  
  (define-key map "c" 'git-log-view-cherry-pick)
  (define-key map "k" 'git-log-view-checkout)
  (define-key map "r" 'git-log-view-reset)
  (define-key map "v" 'git-log-view-revert)
  (define-key map "t" 'git-log-view-tag)

  (define-key map "g" 'git-log-view-refresh)
  (define-key map "q" 'git--quit-buffer))


;; Menu
(easy-menu-define
 git-log-view-menu git-log-view-mode-map
 "Git"
 `("Git-Log"
   ["Next Commit" log-view-msg-next t]
   ["Previous Commit" log-view-msg-prev t]
   ["Next Interesting Commit" git-log-view-interesting-commit-next t]
   ["Previous Interesting Commit" git-log-view-interesting-commit-prev t]
   "---"
   ["Mark Commits for Diff" set-mark-command t]
   ["Diff Commit(s)" git-log-view-diff-preceding t]
   ["Diff against Current" git-log-view-diff-current t]
   "---"
   ["Reset Branch to Commit" git-log-view-reset t]
   ["Checkout" git-log-view-checkout t]
   ["Cherry-pick" git-log-view-cherry-pick t]
   ["Revert Commit" git-log-view-revert t]
   ["Tag this Commit..." git-log-view-tag t]
   "---"
   ["Refresh" git-log-view-refresh t]
   ["Quit" git--quit-buffer t]))


;; Extra navigation
;; Right now this just moves between merges, but it would be nice to move
;; to the next/prev commit by a different author. But it's harder than a
;; simple RE.
(defvar git-log-view-interesting-commit-re "^Merge[: ]?"
  "Regular expression defining \"interesting commits\" for easy navigation")
(easy-mmode-define-navigation
 git-log-view-interesting-commit git-log-view-interesting-commit-re
 "interesting commit")


;; Implementation
(defvar git-log-view-filenames nil
  "List of filenames that this log is about, nil if the whole repository.")
(defvar git-log-view-qualifier nil
  "A short string representation of `git-log-view-filenames', e.g. \"2 files\"")
(defvar git-log-view-start-commit nil
  "Records the starting commit (e.g. branch name) of the current log view")


(defun git--log-view (&optional files start-commit dont-pop-buffer)
  "Show a log window for the given FILES; if none, the whole
repository. If START-COMMIT is nil, use the current branch, otherwise the
given commit. Assumes it is being run from a buffer whose
default-directory is inside the repo."
  (let* ((rel-filenames (mapcar #'file-relative-name files))
         (log-qualifier (case (length files)
                               (0 (abbreviate-file-name (git--get-top-dir)))
                               (1 (first rel-filenames))
                               (t (format "%d files" (length files)))))
         (log-buffer-name (format "*git log: %s%s*"
                                  log-qualifier
                                  (if start-commit (format " from %s"
                                                           start-commit)
                                    "")))
         (buffer (get-buffer-create log-buffer-name))
         (saved-default-directory default-directory))
    (with-current-buffer buffer
      ;; Subtle: a previous git process might still be running
      (let ((proc (get-buffer-process (current-buffer))))
        (when proc (delete-process proc)))
      (buffer-disable-undo)
      (let ((buffer-read-only nil)) (erase-buffer))
      (git-log-view-mode)
      ;; Tell git-log-view-mode what this log is all about
      (set (make-local-variable 'git-log-view-qualifier) log-qualifier)
      (set (make-local-variable 'git-log-view-start-commit) start-commit)
      (set (make-local-variable 'git-log-view-filenames) rel-filenames)
      ;; Subtle: the buffer may already exist and have the wrong directory
      (cd saved-default-directory)
      ;; vc-do-command does almost everything right. Beware, it misbehaves
      ;; if not called with current buffer (undoes our setup)
      (apply #'vc-do-command buffer 'async "git" nil "log"
             (append (when start-commit (list start-commit))
                     (list "--")
                     rel-filenames))
      ;; vc sometimes goes to the end of the buffer, for unknown reasons
      (vc-exec-after `(goto-char (point-min))))
    (if dont-pop-buffer
        buffer
      (pop-to-buffer buffer))))

;; Entry points
(defun git-log-files ()
  "Launch the git log view for the current file, or the selected files in
git-status-mode."
  (interactive)
  (git--require-buffer-in-git)
  (git--log-view (git--if-in-status-mode
                     (git--status-view-marked-or-file)
                   (list buffer-file-name))))
 
(defun git-log ()
  "Launch the git log view for the whole repository"
  (interactive)
  ;; TODO: maybe ask user for a git repo if they're not in one
  (git--log-view))

(defun git-log-other (&optional commit)
  "Launch the git log view for another COMMIT, which is prompted for if
unspecified. You can then cherrypick commits from e.g. another branch
using the `git-log-view-cherrypick'."
  (interactive (list (git--select-revision "View log for: ")))
  (git--log-view nil commit))

;; Take advantage of the nice git-log-view from the command line.
;; Recipes:
;; function gl() { gnuclient --batch --eval "(git-log-from-cmdline \"$DISPLAY\" \"$(pwd)\" \"$1\")"; }
;;
;; If you prefer a separate emacs instance:
;; function gl() { emacs -l ~/.emacs --eval "(git-log-from-cmdline nil nil \"$1\")"; }
;;
;; Then you can just run "gl" or "gl another-branch", for example.
(defun git-log-from-cmdline (&optional display directory start-commit)
  "Launch a git log view from emacs --eval or gnuclient --eval. If DISPLAY
is specified, create a frame on the specified display. If DIRECTORY is
specified, do git log for that directory (a good idea in gnuclient)
. If START-COMMIT if specified, log starting backwards from that commit, e.g.
a branch."
  (let ((default-directory (or directory default-directory))
        (frame (when display (select-frame (make-frame-on-display display)))))
    (switch-to-buffer
     (git--log-view nil (when (> (length start-commit) 0) start-commit) t))
    (when display
      ;; Delete the frame on quit if we created it and nothing else displayed
      (add-hook 'kill-buffer-hook
              (lexical-let ((git-log-gnuserv-frame frame))
                #'(lambda()
                    (dolist (window (get-buffer-window-list (current-buffer)))
                      (when (and (eq (next-window window) window)
                                 (eq (window-frame window)
                                     git-log-gnuserv-frame))
                          (delete-frame (window-frame window))))))
              t t))                      ; hook is append, local
  ""))

;; Actions
(defun git-log-view-checkout ()
  "Checkout the commit that the mark is currently in."
  (interactive)
  (let ((commit (substring-no-properties (log-view-current-tag))))
    (when (y-or-n-p (format "Checkout %s from %s? "
                            git-log-view-qualifier commit))
      (if git-log-view-filenames
          (progn
            (apply #'git--exec-string "checkout" commit "--"
                   git-log-view-filenames)
            (git-after-working-dir-change git-log-view-filenames))
        (git-checkout commit)))))     ;special handling for whole-tree checkout

(defun git-log-view-cherry-pick ()
  "Cherry-pick the commit that the cursor is currently in on top of the current
branch."
  (interactive)
  (let ((commit (substring-no-properties (log-view-current-tag)))
        (current-branch (git--current-branch)))
    (when (y-or-n-p (format "Cherry-pick commit %s on top of %s? "
                            commit (git--bold-face current-branch)))
      (git--exec-string "cherry-pick" commit "--")
      (git-after-working-dir-change))))

(defun git-log-view-reset ()
  "Reset the current branch to the commit that the cursor is currently in."
  (interactive)
  (let ((commit (substring-no-properties (log-view-current-tag)))
        (current-branch (ignore-errors (git--current-branch))))
    (when (y-or-n-p (format "Reset %s to commit %s? "
                            (if current-branch (git--bold-face current-branch)
                              "current state")
                            (git--abbrev-commit commit)))
      (git-reset commit))))

(defun git-log-view-diff-preceding ()
  "Diff the commit the cursor is currently on against the preceding commits.
If a region is active, diff the first and last commits in the region."
  (interactive)
  (let* ((commit (git--abbrev-commit
                 (log-view-current-tag (when mark-active (region-beginning)))))
        (preceding-commit
         (git--abbrev-commit
          (save-excursion
            (when mark-active
              (goto-char (region-end))
              ;; Go back one to get before the lowest commit, then
              ;; msg-next will find it properly. Unless the region is empty.
              (unless (equal (region-beginning) (region-end))
                (backward-char 1)))
            (log-view-msg-next)
            (log-view-current-tag)))))
    ;; TODO: ediff if single file, but git--ediff does not allow revisions
    ;; for both files
    (git--diff-many git-log-view-filenames preceding-commit commit t)))

(defun git-log-view-diff-current ()
  "Diff the commit the cursor is currently on against the current state of
the working dir."
  (interactive)
  (let* ((commit (git--abbrev-commit (log-view-current-tag))))
    (if (eq 1 (length git-log-view-filenames))
        (git--diff (first git-log-view-filenames)
                   (concat commit ":" ))
      (git--diff-many git-log-view-filenames commit nil))))

(defun git-log-view-revert ()
  "Revert the commit that the cursor is currently on"
  (interactive)
  (let ((commit (substring-no-properties (log-view-current-tag))))
    (when (y-or-n-p (format "Revert %s? " commit))
      (git-revert commit))))

(defun git-log-view-refresh ()
  "Refresh log view"
  (interactive)
  (unless (boundp 'git-log-view-start-commit) (error "Not in git log view"))
  (git--log-view git-log-view-filenames git-log-view-start-commit))

(defun git-log-view-tag (&optional tag-name)
  "Create a new tag for commit that the cursor is on."

  (interactive)
  (git-tag tag-name (git--abbrev-commit (log-view-current-tag))))

(provide 'git-log)
