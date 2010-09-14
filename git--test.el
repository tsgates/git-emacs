;; See git-emacs.el for license and versioning.

(require 'git-emacs)
(require 'git-status)
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

(defun git--test-typical-repo-ops ()
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
          (assert (string-match "^[0-9a-f.]* *another test commit"
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
          ;; Unfortunately, git 1.6 has taken to mangling messages according
          ;; to the subject/body distinctions. This stinks and we'll need to
          ;; fix it; but there is simply no good way to do this now.
          (assert (equal "another test commit Now amended "
                         (replace-regexp-in-string "\n" " "
                                                   (git--last-log-message))))
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

  ;; Try a new branch.
  (flet ((git--select-revision (ignored-prompt prepend-choices excepts)
           (assert (equal '("master") prepend-choices))
           (assert (equal '("master") excepts))
          "master"))
    (let (seen-checkout-func-args)
      (git-checkout-to-new-branch "newbranch" "master"
                                  (lambda (&rest args)
                                    (setq seen-checkout-func-args args))
                                  "arg1" nil 'arg2)
      (assert (equal '("arg1" nil arg2) seen-checkout-func-args)))
    (let* ((branch-list-and-current (git--branch-list))
           (sorted-branch-list (sort (car branch-list-and-current) 'string<)))
      (assert (equal '("master" "newbranch") sorted-branch-list))
      (assert (equal "newbranch" (cdr branch-list-and-current))))
    ;; git--current-branch should return the same result.
    (assert (equal "newbranch" (git--current-branch))))

  ;; Check git-stash.
  (with-temp-buffer
    (insert "contents for stash")
    (write-file "f1"))
  (assert (eq 'modified (git--status-file "f1")))
  (let (saved-suggested-cmd cmd-to-return)
    (flet ((read-string (ignored-prompt suggested &rest ignored)
             (setq saved-suggested-cmd suggested)
             cmd-to-return)
           (sleep-for (&rest args) t))
      (setq cmd-to-return "save")
      (call-interactively 'git-stash)
      (message "suggested: %s" saved-suggested-cmd)
      (assert (equal "save" saved-suggested-cmd))
      (assert (eq 'uptodate (git--status-file "f1")))
      ;; Now it should suggest popping the stash.
      (setq cmd-to-return "pop")
      (call-interactively 'git-stash)
      (assert (equal "pop" saved-suggested-cmd))
      (assert (eq 'modified (git--status-file "f1")))
      ;; Contents should be restored too.
      (assert (string= "contents for stash"
                       (git--trim-string
                        (with-temp-buffer
                          (insert-file-contents "f1")
                          (buffer-string)))))
      ))
  ;; Check that git-stash buffer is deleted on error exit
  (let (saved-buffer)
    (flet ((read-string (&rest ignored)
             (setq saved-buffer (current-buffer))
             (error "test error")))
      (ignore-errors (call-interactively 'git-stash)))
    (assert saved-buffer)
    (assert (not (buffer-live-p saved-buffer))))
      
  ;; Do some more fun stuff here...
  
  )

(defun git--test-standalone-functions ()
   ;; Human-readable size
  (require 'git-status)
  (assert (equal "8" (git--status-human-readable-size 8)))
  (assert (equal "1023" (git--status-human-readable-size 1023)))
  (assert (equal "1.0K" (git--status-human-readable-size 1024)))
  (assert (equal "25K" (git--status-human-readable-size 25902)))
  (assert (equal "382K" (git--status-human-readable-size 391475)))
  (assert (equal "1.0M" (git--status-human-readable-size (* 1023 1024))))
  (assert (equal "2.5M" (git--status-human-readable-size (* 2570 1024))))

  ;; Some tests of fileinfo-lessp
  (flet ((check-compare (name1 type1 name2 type2 isless12 isless21)
           (let ((info1 (git--create-fileinfo name1 type1))
                 (info2 (git--create-fileinfo name2 type2)))
             (assert (eq isless12 (git--fileinfo-lessp info1 info2)))
             (assert (eq isless21 (git--fileinfo-lessp info2 info1))))))
    (check-compare "abc" 'blob "def" 'blob t nil)
    (check-compare "abc" 'tree "def" 'tree t nil)
    (check-compare "abc" 'blob "abc" 'blob nil nil)

    (check-compare "abc" 'blob "def/foo" 'blob nil t)
    (check-compare "def/foo" 'blob "def/foo" 'blob nil nil)
    (check-compare "abc/foo" 'blob "def" 'blob t nil)
    (check-compare "abc/def" 'tree "abc/def/aaa" 'blob t nil)
    (check-compare "abc/def" 'tree "abc/def/aaa" 'tree t nil)
    ;; This is the situation where an Unknown file comes in low in the tree
    (check-compare "abc/def" 'tree "abc/def/aaa/bbb" 'blob t nil)
    (check-compare "abc/hij" 'tree "abc/def/aaa/bbb" 'blob nil t)
    )

  )

(defun git--test-branch-mode ()
  ;; Virtualize git repo functions.
  (flet ((git--branch-list () '(("aa" "master" "foobar") . "master")))
    ;; Get rid of user hooks.
    (let (git--branch-mode-hook git-branch-annotator-functions)
      (unwind-protect
          (save-window-excursion
            (git-branch)
            (assert (looking-at "master"))
            (assert (string= (buffer-string) "   aa\n * master\n   foobar\n"))
            (assert (equal "master" (git-branch-mode-selected)))
            (forward-line)  ;; next-line errors out in batch for some reason
            (assert (equal "foobar" (git-branch-mode-selected)))
            ;; Let's try some annotations
            (setq git-branch-annotator-functions
                  (list (lambda (branch-list)
                          (assert (equal branch-list '("aa" "master" "foobar")))
                          '(("aa" . "an-aa-1")))
                        (lambda (branch-list)
                          (assert (equal branch-list '("aa" "master" "foobar")))
                          '(("aa" . "an-aa-2") ("foobar" . "an-foobar-1")))))
            (flet ((window-width () 80)) (git--branch-mode-refresh))
            ;; Point should stay the same
            (assert (looking-at "foobar"))
            (assert (string= (buffer-string)
                             (concat "   aa       - an-aa-1 an-aa-2\n"
                                     " * master\n"
                                     "   foobar   - an-foobar-1\n")))
          )
      (kill-buffer "*git-branch*"))
    )))
;; (git--test-branch-mode)

(defun git-regression ()
  (interactive)
  ;; (setq debug-on-error t)  ;; uncomment to debug test run from make
  (message "Running unittest suite...")
  (git--test-standalone-functions)
  (save-window-excursion                ; some bufs might pop up, e.g. commit
    (git--test-with-temp-repo #'git--test-typical-repo-ops))
  (git--test-branch-mode)

  (message "git-regression passed"))
  