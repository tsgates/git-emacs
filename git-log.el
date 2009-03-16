;; See git-emacs.el for license information

(require 'git-emacs)
(require 'log-view)

;; Based off of log-view-mode, which has some nice functionality, like
;; moving between comits
(define-derived-mode git-log-view-mode
  log-view-mode "Git log" "Major mode for viewing git logs"
  :group 'git
  ;; Customize log-view-message-re to be the git commits
  (set (make-local-variable 'log-view-message-re)
       "^[Cc]ommit[: ]*\\([0-9a-f]+\\)")
  ;; As for the file re, there is no such thing -- make it impossible
  (set (make-local-variable 'log-view-file-re)
       "^No_such_text_really$")
  (set (make-local-variable 'font-lock-defaults)
       (list 'git-log-view-font-lock-keywords t))
  ;;(when global-font-lock-mode (font-lock-mode t))
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
(define-key git-log-view-mode-map "q" 'git--quit-buffer)
(define-key git-log-view-mode-map "c" 'git-log-view-checkout)

(defun git--log-view (&rest files)
  "Show a log window for the given files; if none, the whole
repository. Assumes it is being run from a buffer whose
default-directory is inside the repo."
  (let* ((rel-filenames (mapcar #'file-relative-name files))
         (log-identification (case (length files)
                               (0 (abbreviate-file-name
                                   (git--get-top-dir default-directory)))
                               (1 (first rel-filenames))
                               (t (format "%d files" (length files)))))
         (log-buffer-name (format "*git log: %s*" log-identification))
         (buffer (get-buffer-create log-buffer-name))
         (saved-default-directory default-directory))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (let ((buffer-read-only nil)) (erase-buffer))
      (git-log-view-mode)
      ;; Tell git-log-view-mode what this log is all about
      (set (make-local-variable 'git-log-view-qualifier) log-identification)
      (set (make-local-variable 'git-log-view-filenames) rel-filenames)
      ;; Subtle: the buffer may already exist and have the wrong directory
      (cd saved-default-directory)
      ;; vc-do-command does almost everything right. Beware, it misbehaves
      ;; if not called with current buffer (undoes our setup)
      (apply #'vc-do-command buffer 'async "git" nil "rev-list"
             "--pretty" "HEAD" "--" rel-filenames)
      ;; vc sometimes goes to the end of the buffer, for unknown reasons
      (vc-exec-after `(goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun git-log ()
  "Launch the git log view for the current file"
  (interactive)
  (git--require-buffer-in-git)
  (git--log-view buffer-file-name))
 
(defun git-log-all ()
  "Launch the git log view for the whole repository"
  (interactive)
  ;; TODO: maybe ask user for a git repo if they're not in one
  (git--log-view))

(defun git-log-view-checkout ()
  (interactive)
  (let ((commit (substring-no-properties (log-view-current-tag))))
    (when (y-or-n-p (format "Checkout %s from %s? "
                            git-log-view-qualifier commit))
      (git--please-wait
       "Checking out"
       (apply #'git--exec-string-with-error "checkout" commit "--"
              git-log-view-filenames)))))
