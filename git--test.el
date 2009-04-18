;; See git-emacs.el for license and versioning.

(require 'git-emacs)
(require 'dired)

(defun git--test-with-temp-repo (function)
  "Run FUNCTION inside a temporary git repository"
  ;; DO NOT REASSIGN the temp dir variable below under any circumstances. We
  ;; wouldn't want to remove recursively some arbitrary dir.
  (let* ((git--test-tmp-dir-DONT-REASSIGN (make-temp-file "git-emacs-test-" t))
         (git--test-tmp-dir git--test-tmp-dir-DONT-REASSIGN) ; Here, change this
         (default-directory                                  ; or this
           (file-name-as-directory git--test-tmp-dir))) 
    (unwind-protect
        (progn
          (message "Created temporary test dir %s" default-directory)
          (git-init default-directory)  ; part of the suite, kind of
          (funcall function))
      (dired-delete-file git--test-tmp-dir-DONT-REASSIGN 'always)
      (message "Deleted temporary test dir %s"
               git--test-tmp-dir-DONT-REASSIGN))))

(defun git--tests-unittest-suite ()
  ;; git exec
  (assert (string= "\n" (git--exec-string "rev-parse" "--show-cdup")))
  (assert (string= (expand-file-name "./") (git--get-top-dir ".")))
  (assert (string= (expand-file-name "./")
                   (git--get-top-dir "./nO/sUCH/dIrectory/Exists")))

  ;; Create a file, and commit something.
  (with-temp-buffer
    (insert "sample text")
    (write-file "f1"))
  (assert (eq nil (git--status-file "f1")))
  (let ((fi (git--ls-files "--others")))
    (assert (eq 1 (length fi)))
    (assert (eq 'unknown (git--fileinfo->stat (car fi))))
    (assert (string= "f1" (git--fileinfo->name (car fi)))))
  
  (git--add "f1")
  (git--commit "test commit 1")
  (assert (eq 'uptodate (git--status-file "f1")))

  ;; create status buffer
  (require 'git-status)
  (assert (string= (buffer-name (git--create-status-buffer "."))
                   (git--status-buffer-name ".")))

  ;; open status buffer
  (assert (string= (buffer-name (git--create-status-buffer "."))
                   (git--status-buffer-name ".")))

  (git--kill-status-buffer ".")

  ;; tag stuff
  (assert (null (git-tag "at-first-commit")))
  (assert (stringp (git-tag "at-first-commit")))

  ;; test some of the buffer handling functions
  (with-temp-buffer
   (insert-file-contents "f1" t)        ; visit
   (vc-find-file-hook)
   (assert (equal (list (current-buffer)) (git--find-buffers-in-dir ".")))
   (assert (equal (list (current-buffer))
                  (git--find-buffers-from-file-list '("f1"))))
   (assert (eq 0 (git--maybe-ask-save)))
   (git--require-buffer-in-git)
   (git--if-in-status-mode (error "guess again"))

   (insert "something else")
   (save-buffer)
   )

  (assert (eq 'modified (git--status-file "f1")))

  ;; Try some gui commits
  (let ((git--commit-log-buffer "*git commit for unittest*")
        (first-commit-id (git--rev-parse "at-first-commit"))
        (second-commit-id nil))
    (unwind-protect
        (progn
          (condition-case err
              (progn (git-commit) (error "Expected error not raised"))
            (error
             (unless (string-match "^Nothing to commit"
                                   (error-message-string err))
               (signal (car err) (cdr err)))))
          (git-commit-all)
          (assert (equal '("-a") git--commit-args))
          (insert "another test commit")
          (git--commit-buffer)
          (assert (not (buffer-live-p (get-buffer git--commit-log-buffer))))
          (assert (eq 'uptodate (git--status-file "f1")))
          (assert (string-match "^[0-9a-f]* *another test commit"
                                (git--last-log-short)))
          ;; Should be one above last commit
          (setq second-commit-id (git--rev-parse "HEAD"))
          (assert (equal first-commit-id (git--rev-parse "HEAD^1")))
          ;; Do an amend commit
          (git-commit t)
          (assert (equal '("--amend") git--commit-args))
          (insert "Now amended")
          (git--commit-buffer)
          (assert (eq 'uptodate (git--status-file "f1")))
          (assert (equal "another test commit\nNow amended\n"
                         (git--last-log-message)))
          (assert (not (equal second-commit-id (git--rev-parse "HEAD"))))
          ;; Should still be one commit above the first
          (assert (equal first-commit-id (git--rev-parse "HEAD^1"))))
      (ignore-errors (kill-buffer git--commit-log-buffer))))

  ;; Some baseline testing. The usual suspects (git-svn, origin), won't exist.
  (let ((git-baseline-candidates '("git-svn" "origin" "at-first-commit"))
        git--test-func-was-called git-baseline-alist)
    (assert (equal "at-first-commit" (git-baseline)))
    ;; Now add a couple of functions into the mix
    (flet ((git--test-baseline-not-ok ()
                                     (setq git--test-func-was-called t)
                                     nil)
           (git--test-baseline-ok () "some-commit")
           (git--select-revision (&rest args) (error "%S" args)))
      (add-to-list 'git-baseline-candidates 'git--test-baseline-not-ok)
      (setcar (last git-baseline-candidates) 'git--test-baseline-ok)
      (assert (equal "some-commit" (git-baseline)))
      (assert git--test-func-was-called)
      ;; Now test interactive call. Fake some interactive functions
      (add-to-list 'git-baseline-candidates "at-first-commit" t)
      (flet ((git--select-revision
              (prompt prepend except)
              (assert (equal '("(git--test-baseline-ok)" "at-first-commit")
                             prepend))
              (assert (equal prepend except))
              "(git--test-baseline-ok)")
             (y-or-n-p (prompt) t)
             (customize-save-variable
              (symbol value)
              (assert (eq 'git-baseline-alist symbol))
              (assert (equal git-baseline-alist value))))
        (setq git--test-func-was-called nil)
        (assert (equal "some-commit" (call-interactively 'git-baseline)))
        (assert git--test-func-was-called)
        (assert (equal (list (cons default-directory 'git--test-baseline-ok))
                       git-baseline-alist))
        ;; if we call again (non-interactively), the value should be cached
        (setq git--test-func-was-called nil)
        (fset 'git--select-revision 'error)
        (assert (equal "some-commit" (git-baseline)))
        (assert (not git--test-func-was-called))
        ))
    )

  ;; Do some more fun stuff here...
  )

(defun git-regression ()
  (interactive)

  (message "Running unittest suite...")
  (save-window-excursion                ; some bufs might pop up, e.g. commit
    (git--test-with-temp-repo #'git--tests-unittest-suite))

  (message "git-regression passed"))
  