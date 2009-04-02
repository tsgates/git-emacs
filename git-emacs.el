;;; git-emacs (v.1.4) : yet another git emacs mode for newbies
;;
;; Copyright (C) 2008  TSKim (tsgatesv@gmail.com)
;;
;; v.1.4 Modified by ovy            @ 22 March 2009
;; v.1.3 Modified by Con Digitalpit @ 29 March 2008
;; 
;; Authors    : TSKim : Kim Taesoo(tsgatesv@gmail.com)
;; Created    : 24 March 2007
;; License    : GPL
;; Keywords   : git, version control, release management
;;
;; Compatibility: Emacs22 and EmacsCVS (developed on 23.0.60.2)
;;                Git 1.5 and up

;; This file is *NOT* part of GNU Emacs.
;; This file is distributed under the same terms as GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; http://tsgates.cafe24.com/git/git-emacs.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; I referenced a lot of codes such as under
;;   - psvn.el (Stefan Reichoer)
;;   - vc-git.el (Alexandre Julliard)
;;   - git.el (Alexandre Julliard)
;;   - ido.el (Kim F. Storm)
;;   - ... 
;;   
;;; Installation
;; 
;; (add-to-list 'load-path "~/.emacs.d/git-emacs")
;; (require 'git-emacs)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; BUG FIXES
;;   2008.03.28 : git-diff just work on git root
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO : check git environment
;; TODO : status -> index
;; TODO : pull/patch
;; TODO : fetching 
;; TODO : regular exp selecting
;; TODO : enhance config! & tag!
;; TODO : save -> status-view update
;; TODO : git-log -> C-u command :=> cmd
;; TODO : status-mode function -> add status prefix
;; TODO : git set config
;; TODO : () -> fording/unfording for detail
;; TODO : show ignored files
;; TODO : locally flyspell
;; 
;; DONE : turn off ido-mode globally
;; DONE : git-add
;; DONE : remote branch list
;; DONE : separate branch-mode & status-view-mode to other files


(eval-when-compile (require 'cl))

(require 'ediff)                        ; we use this a lot
(require 'vc)                           ; vc
(require 'vc-git)                       ; vc-git advises
(add-to-list 'vc-handled-backends 'git) ; set backend management
(require 'electric)                     ; branch mode
(require 'time-stamp)                   ; today

(require 'git-global-keys)              ; global keyboard mappings

;; Autoloaded submodules
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git" t)

(autoload 'git-status "git-status"
  "Launch git-status-mode on the specified directory.")

(autoload 'git--update-state-mark "git-modeline"
  "Update modeline of git buffers with a customizable state marker" t)
(autoload 'git--update-all-state-marks "git-modeline"
  "Update the modelines of all git buffers" t)

(autoload 'git-log "git-log"
  "Launch the git log view for the current file or the selected files in git-status-mode" t)
(autoload 'git-log-all "git-log"
  "Launch the git log view for whole repository" t)
(autoload 'git-log-other "git-log"
  "Launch the git log view for an arbitrary branch or tag" t)
(autoload 'git-log-from-cmdline "git-log"
  "Launch a git log view from emacs --eval or gnuclient --eval" t)

(defalias 'electric-pop-up-window 'Electric-pop-up-window)
(defalias 'electric-command-loop  'Electric-command-loop)

;;-----------------------------------------------------------------------------
;; preference of ido-mode
;;-----------------------------------------------------------------------------
(defcustom git--use-ido t
  "Use ido for Git prompts. Affects the default of `git--completing-read'."
  :type '(boolean)
  :group 'git-emacs)

(defvar git--completing-read
  (if git--use-ido
      (progn
        (require 'ido)
        #'ido-completing-read)
    #'completing-read)
  "Function to use for git minibuffer prompts with choices. It should have
the signature of `completing-read'.")


(defgroup git nil
  "A user interface for the git versioning system."
  :group 'tools)

;; Face definition macros. Used mostly in git-status.
(defmacro git--face (name fore1 prop1 fore2 prop2)
  `(defface ,(intern (concat "git--" (symbol-name name) "-face"))
     '((((class color) (background light)) (:foreground ,fore1 ,@prop1))
       (((class color) (background dark))  (:foreground ,fore2 ,@prop2)))
    ,(concat "git " (symbol-name name) " face in status buffer mode")
    :group 'git))

(git--face bold       "tomato" (:bold t) "tomato"  (:bold t))

(defsubst git--bold-face (str) (propertize str 'face 'git--bold-face))

(defconst git--msg-critical  (propertize "Critical Error" 'face 'git--bold-face))
(defconst git--msg-failed    (propertize "Failed" 'face 'git--bold-face))

;;-----------------------------------------------------------------------------
;; internal variable
;;-----------------------------------------------------------------------------

(defvar git--commit-log-buffer "*git commit*")
(defvar git--log-flyspell-mode t "enable flyspell-mode when editing log")
(defvar git--repository-bookmarks
  '("git://github.com/tsgates/git-emacs.git"
    "git://git.kernel.org/pub/scm/git/git.git"
    "git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux-2.6.git"
    )
  "repository bookmarks")

(defvar git--repository-history nil)
(defconst git--repository-dir ".git")

(defconst git--reg-space   " ")
(defconst git--reg-status  "\\([A-Z?]\\)")
(defconst git--reg-tab     "\t")
(defconst git--reg-blank   "[\t\0 ]+")
(defconst git--reg-eof     "\0")
(defconst git--reg-perm    "\\([0-7]\\{6\\}\\)")
(defconst git--reg-type    "\\([^ ]+\\)")
(defconst git--reg-sha1    "\\([0-9a-f]\\{40\\}\\)")
(defconst git--reg-file    "\\([^\0]+\\)")
(defconst git--reg-branch  "\\([^\n]+\\)")
(defconst git--reg-stage   "\\([0-9]+\\)")

(defconst git--log-sep-line
  "# --------------------------- message ---------------------------")
(defconst git--log-file-line
  "# ---------------------------- files ----------------------------")
(defconst git--log-header-line
  "# ----------------------------- info ----------------------------")


;;-----------------------------------------------------------------------------
;; fork git process
;;-----------------------------------------------------------------------------

(defsubst git--exec (cmd outbuf infile &rest args)
  "Execute 'git' clumsily"

  (apply #'call-process
         "git"                          ; cmd
         infile                         ; in file
         outbuf                         ; out buffer
         nil                            ; display
         (cons cmd args)))              ; args

(defun git--exec-pipe (cmd input &rest args)
  "Execute 'echo input | git cmd args' and return result
string. INPUT can also be a buffer."

  (with-output-to-string
    (with-current-buffer standard-output
      (let ((tmp (make-temp-file "git-emacs-tmp")))
        (unwind-protect
            (progn
              (if (bufferp input)
                  (with-current-buffer input
                    (write-file tmp))
                (with-temp-buffer
                  (insert input)
                  (write-file tmp)))
              ;; tricky hide write to file message
              (message "")
              (apply #'git--exec cmd t tmp args))
          (delete-file tmp))))))

(defsubst git--exec-buffer (cmd &rest args)
  "Execute 'git' within the buffer"
  
  (apply #'git--exec cmd t nil args))

(defsubst git--exec-string-no-error (cmd &rest args)
  "Execute 'git' and return result string (which may be a failure message)."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'git--exec-buffer cmd args))))

(defsubst git--trim-string (str)
  "Trim the front and rear part of the string"
  
  (let ((begin 0) (end (- (length str) 1)))

    ;; trim front
    (while (and (< begin end)
                (memq (aref str begin) '(? ?\n)))
      (incf begin))

    ;; trim rear
    (while (and (<= begin end)
                (memq (aref str end) '(? ?\n)))
      (decf end))

    (substring str begin (+ end 1))))

(defun git--exec-string (cmd &rest args)
  "Executes the specified git command, raises an error with the git output
if it fails. If the command succeeds, returns the git output."
  (with-output-to-string
    (with-current-buffer standard-output
      (unless (eq 0
                  (apply #'git--exec-buffer cmd args))
        (error (git--trim-string (buffer-string)))))))

;;-----------------------------------------------------------------------------
;; utilities
;;-----------------------------------------------------------------------------

(defsubst git--trim-tail (str)
  "Trim only the tail of the string"
  
  (let ((end (- (length str) 1)))

    (while (and (< 0 end)
                (memq (aref str end) '(? ?\n)))
      (decf end))

    (substring str 0 (+ end 1))))

(defsubst git--join (seq &optional sep)
  "' '.join( seq ) in python"

  (mapconcat #'identity seq (if sep sep " ")))

(defsubst git--concat-path-only (path added)
  "Concatenate the path with proper separator"
  
  (concat (file-name-as-directory path) added))

(defsubst git--concat-path (path added)
  (expand-file-name (git--concat-path-only path added)))

(defsubst git--expand-to-repository-dir (dir)
  (git--concat-path dir git--repository-dir))

(defun git--quit-buffer ()
  "Delete the window and kill the current buffer"

  (interactive)
  (let ((buffer (current-buffer)))
    ;; Emacs refuses to delete a "maximized" window (i.e. just 1 in frame)
    (unless (one-window-p t) (delete-window))
    (kill-buffer buffer)))

(defsubst git--interpret-to-state-symbol (stat)
  "Interpret git state string to state symbol"

  (case (string-to-char stat)
    (?H 'uptodate )
    (?M 'modified )
    (?? 'unknown  )
    (?A 'added    )
    (?D 'deleted  )
    (?U 'unmerged )
    (?T 'modified )
    (?K 'killed   )
    (t nil)))

(defsubst git--build-reg (&rest args)
  (apply #'concat (add-to-list 'args "\0" t)))

(defsubst git--select-from-user (prompt choices &optional history default)
  "Select from choices. Shortcut to git--completing-read."
  (funcall git--completing-read prompt choices nil nil nil history default))

(defmacro git--please-wait (msg &rest body)
  "Macro to give feedback around actions that may take a long
time. Prints MSG..., executes BODY, then prints MSG...done (as per the elisp
style guidelines)."
  `(let ((git--please-wait-msg (concat ,msg "...")))
     (message git--please-wait-msg)
     ,@body
     (message (concat git--please-wait-msg "done"))))

(defun git--find-buffers-in-dir (repo &optional predicate)
  "Finds buffers corresponding to files in the given directory,
optionally satisfying PREDICATE (which should take a buffer object as
argument)."
  (let* ((absolute-repo (expand-file-name (file-name-as-directory repo)))
         (absolute-repo-length (length absolute-repo))
         (buffers))
    (dolist (buffer (buffer-list))
      (let ((filename (buffer-file-name buffer)))
        (when filename
          (when (and (eq t (compare-strings filename
                                            0 absolute-repo-length
                                            absolute-repo
                                            0 absolute-repo-length))
                     (or (not predicate) (funcall predicate buffer)))
              (add-to-list 'buffers buffer)))))
    buffers))

(defun git--find-buffers-from-file-list (filelist &optional predicate)
  "Finds buffers corresponding to files in the given list,
optionally satisfying the predicate."
  (let (buffers)
    (dolist (filename filelist)
      (let ((buffer (find-buffer-visiting filename predicate)))
        (when buffer (add-to-list 'buffers buffer))))
    buffers))

(defun git--find-buffers (&optional repo-or-filelist predicate)
  "Find buffers satisfying PREDICATE in the given
REPO-OR-FILELIST, which can be a string (path within a git
repository), a list (filelist) or nil (current git repository)."
  (cond
   ((eq nil repo-or-filelist) (git--find-buffers-in-dir
                               (git--get-top-dir default-directory)
                               predicate))
   ((stringp repo-or-filelist) (git--find-buffers-in-dir
                                (git--get-top-dir repo-or-filelist) predicate))
   (t (git--find-buffers-from-file-list repo-or-filelist predicate))))

(defun git--maybe-ask-save (&optional repo-or-filelist)
  "If there are modified buffers which visit files in the given REPO-OR-FILELIST,
ask to save them. If REPO-OR-FILELIST is nil, look for buffers in
the current git repo. Returns the number of buffers saved."
  (let ((buffers (git--find-buffers repo-or-filelist  #'buffer-modified-p)))
    (map-y-or-n-p
     (lambda(buffer) (format "Save %s? " (buffer-name buffer)))
     (lambda(buffer) (with-current-buffer buffer (save-buffer)))
     buffers
     '("buffer" "buffers" "save"))))

(defun git--maybe-ask-revert (&optional repo-or-filelist)
  "If there are buffers visiting files in the given REPO-OR-FILELIST that
have changed (buffer modtime != file modtime), ask the user whether to refresh
those buffers. Returns the number of buffers refreshed. Updates the state
mark of all the buffers not reverted (since revert updates it anyway)."
  (let ((buffers (git--find-buffers
                  repo-or-filelist
                  #'(lambda(buffer)
                      (not (verify-visited-file-modtime buffer)))))
        (buffers-that-exist nil) (buffers-that-dont-exist nil))
    (dolist (buffer buffers)
      (if (file-exists-p (buffer-file-name buffer))
          (push buffer buffers-that-exist)
        (push buffer buffers-that-dont-exist)))
    ;; Revert buffers that exist
    (let ((buffers-not-reverted (copy-sequence buffers-that-exist)))
      ;; Do the state mark update if the user quits the revert prompt series.
      (unwind-protect
          (map-y-or-n-p
           (lambda(buffer) (format "%s has changed, refresh buffer? "
                                   (buffer-name buffer)))
           (lambda(buffer)
             (with-current-buffer buffer (revert-buffer t t))
             ;; A hash table is probably not worth it here.
             (setq buffers-not-reverted (delq buffer buffers-not-reverted)))
           buffers-that-exist
           '("buffer" "buffers" "refresh"))
        (when buffers-not-reverted
          (git--update-all-state-marks (mapcar #'buffer-file-name
                                               buffers-not-reverted)))))
    (when buffers-that-dont-exist
      (message "Note: some open files no longer exist: %s"
               (git--join
                (let ((numfiles (length buffers-that-dont-exist)))
                  (if (> numfiles 2)
                      (list (buffer-name (first buffers-that-dont-exist))
                            (format "%d others" (- numfiles 1)))
                    (mapcar #'buffer-name buffers-that-dont-exist)))
                ", ")))))

;; This belongs later with all the commit functions, but the compiler complains
;; in git-log if we don't define it before its first use.
(defun git-commit-all ()
  "Runs git commit -a, prompting for a commit message"
  (interactive)
  (git-commit t))

;;-----------------------------------------------------------------------------
;; fileinfo structure
;;-----------------------------------------------------------------------------

;; ewoc file info structure for each list element
(defstruct (git--fileinfo 
            (:copier nil)
            (:constructor git--create-fileinfo-core
                          (name type &optional sha1 perm marked stat size refresh lessp))
            (:conc-name git--fileinfo->))
  marked   ;; t/nil
  expanded ;; t/nil
  refresh  ;; t/nil
  lessp    ;; sort priority (tree=3, sub=2, blob=1)
  stat     ;; 'unknown/'modified/'uptodate/'staged  etc.
  type     ;; 'blob/'tree
  name     ;; filename
  size     ;; size
  perm     ;; permission
  sha1)    ;; sha1

(defsubst git--create-fileinfo (name type &optional sha1 perm marked stat size refresh)
  "Create fileinfo through this function instead using 'git--create-fileinfo-core'"
  
  (git--create-fileinfo-core name type sha1 perm marked stat size refresh
                             (if (eq type 'tree) 3 (if (string-match "/" name) 2 1))))

(defun git--fileinfo-lessp (info1 info2)
  "Sorting rules of 'git--fileinfo' ref to 'git--create-fileinfo'"

  (let ((info1-level (git--fileinfo->lessp info1))
        (info2-level (git--fileinfo->lessp info2)))

    (if (eq info1-level info2-level)
        (string-lessp (git--fileinfo->name info1)
                      (git--fileinfo->name info2))
      (> info1-level info2-level))))

(defmacro git--if-in-status-mode (THEN &rest ELSE)
  "Macro that evaluates THEN when in git-status-mode, ELSE otherwise. Used to
grab status-mode filelists without the compiler complaining about the
autoloading which we know has already happened."
  `(if (eq major-mode 'git-status-mode)
       (progn (eval-when-compile (require 'git-status)) ,THEN)
     ,@ELSE))

;;-----------------------------------------------------------------------------
;; git execute command
;;-----------------------------------------------------------------------------

(defun git--init (dir)
  "Execute 'git-init' at 'dir' directory"
  
  (with-temp-buffer
    (when dir (cd dir))
    (git--exec-string "init")))

(defun git--checkout (&rest args)
  "git checkout 'git-checkout' with 'args'"

  (apply #'git--exec-string "checkout" args))

(defun git--clone-sentinal (proc stat)
  "git clone process sentinal"
  
  (let ((cmd (git--join (process-command proc))))
    (cond ((string= stat "finished\n")
           (message "%s : %s" (git--bold-face "Cloned") cmd))
          ;; TODO : popup or state
          ((string= stat "killed\n")
           (message "%s : %s" git--msg-failed cmd))
          (t
           (message "%s : %s" git--msg-critical cmd)))))

(defun git--clone (&rest args)
  "Execute 'git-clone' with 'args' and set sentinal
and finally 'git--clone-sentinal' is called"

  (let ((proc (apply #'start-process "git-clone" nil "git-clone" args)))
    (set-process-sentinel proc 'git--clone-sentinal)
    (message "%s : %s"
             (git--bold-face "Run")
             (git--join (process-command proc)))))

(defun git--commit (msg &rest args)
  "Execute 'git-commit' with 'args' and pipe the 'msg' string"

  (git--trim-string
   (apply #'git--exec-pipe "commit" msg "-F" "-" args)))

(defun git--reset (&rest args)
  "Execute 'git-rest' with 'args' and return the result as string"
  
  (apply #'git--exec-string "reset" args))

(defsubst git--config (&rest args)
  "Execute git-config with args"

  (git--trim-string (apply #'git--exec-string "config" args)))

(defun git--add (files)
  "Execute git-add for each files"
  
  (when (stringp files) (setq files (list files)))
  (apply #'git--exec-string "add" files))

(defun git--mv (src dst)
  "Execute git-mv for src and dst"
  (git--exec-string "mv" src dst))

(defun git--tag (&rest args)
  "Execute 'git-tag' with 'args' and return the result as string"

  (apply #'git--exec-string "tag" args))

(defsubst git--rev-parse (&rest args)
  "Execute 'git-rev-parse' with args and return as string"

  (apply #'git--exec-string "rev-parse" args))

(defvar git--tag-history nil "History variable for tags entered by user.")

(defalias 'git-snapshot 'git-tag)
(defun git-tag (&optional tag-name commit)
  "Create a new tag for the current commit, or a specified one.
git-snapshot is an alias to this. Returns the previous target of the tag,
nil if none."

  (interactive)
  (let* ((friendly-commit (if commit (git--bold-face commit)
                            "current tree"))
        ;; Don't use ido here, since the user will often select a new one
         (tag-name (or tag-name
                       (completing-read (format "Tag %s as: " friendly-commit)
                                        (git--tag-list)
                                        nil nil nil 'git--tag-history)))
         (old-tag-target (ignore-errors
                           (git--trim-string
                            (git--rev-parse "--short" tag-name)))))
    (apply #'git--tag (delq nil (list "-f" tag-name commit)))
    (message "Tagged %s with %s%s"
             friendly-commit (git--bold-face tag-name)
             (if old-tag-target (format " (previously %s)"
                                        (git--bold-face old-tag-target))
               ""))
    old-tag-target))

(defun git--tag-list ()
  "Get the list of known git tags, which may not always refer to commit objects"

  (split-string (git--tag "-l") "\n" t))

(defsubst git--diff-raw (args &rest files)
  "Execute 'git-diff --raw' with 'args' and 'files' at current buffer. This
gives, essentially, file status."
  ;; git-diff abbreviates by default, and also produces a diff.
  (apply #'git--exec-buffer "diff" "-z" "--full-index" "--raw" "--abbrev=40"
         (append args (list "--") files)))

(defun git--status-index (&rest files)
  "Execute 'git-status-index' and return list of 'git--fileinfo'"

  ;; update fileinfo -> unmerged index
  (let ((fileinfo nil)
        (unmerged-info (make-hash-table :test 'equal))
        (regexp (git--build-reg ":"
                                git--reg-perm    ; matched-1: HEAD perms
                                git--reg-blank
                                git--reg-perm    ; matched-2: index perms
                                git--reg-blank
                                git--reg-sha1    ; matched-3: HEAD sha1
                                git--reg-blank
                                git--reg-sha1    ; matched-4: index sha1
                                git--reg-blank
                                git--reg-status  ; matched-5
                                git--reg-eof
                                git--reg-file    ; matched-6
                                )))

    (dolist (stage-and-fi (git--ls-unmerged))
      ;; ignore the different stages, since we're not using the sha1s
      (puthash (git--fileinfo->name (cdr stage-and-fi))
               (git--fileinfo->stat (cdr stage-and-fi))
               unmerged-info))

    (with-temp-buffer
      (apply #'git--diff-raw (list "HEAD") files)

      (goto-char (point-min))

      (while (re-search-forward regexp nil t)
        (let ((perm (match-string 2))
              (stat (git--interpret-to-state-symbol (match-string 5)))
              (file (match-string 6)))

          ;; if unmerged file
          (when (gethash file unmerged-info) (setq stat 'unmerged))
          ;; modified vs. staged: the latter has a nonzero sha1
          (when (and (eq stat 'modified)
                     (not (equal (match-string 4)
                                 "0000000000000000000000000000000000000000")))
            (setq stat 'staged))

          ;; assume all listed elements are 'blob
          (push (git--create-fileinfo file 'blob nil perm nil stat) fileinfo))))

    fileinfo))

(defsubst git--symbolic-ref (arg)
  "Execute git-symbolic-ref with 'arg' and return sha1 string"

  (car (split-string (git--exec-string "symbolic-ref" arg) "\n")))

(defsubst git--current-branch ()
  "Execute git-symbolic-ref of 'HEAD' and return branch name string"

  (let ((branch (git--symbolic-ref "HEAD")))
    (if (string-match "^refs/heads/" branch)
        (substring branch (match-end 0))
      branch)))

(defsubst git--rev-list (&rest args)
  "Execute git-rev-list with 'arg' and print the result to the current buffer"
  
  (apply #'git--exec-buffer "rev-list" args))

(defsubst git--log (&rest args)
  "Execute git-log with 'arg' and return result string"

  (apply #'git--exec-string "log" "-z" args))

(defsubst git--last-log ()
  "Get the last log"
  
  (git--log "--max-count=1" "--pretty=full"))

(defsubst git--last-log-short ()
  "Get the last log as short form"
  
  (git--trim-string (git--log "--max-count=1" "--pretty=oneline")))

(defun git--get-top-dir (&optional dir)
  "Get the top-level git directory above DIR. If nil, use default-directory."
  (let ((default-directory (file-name-as-directory
                            (if dir (expand-file-name dir)
                              default-directory))))
    ;; The default-directory might be gone if a branch was switched! Walk up.
    (let (parent)
      (while (not (or (file-exists-p default-directory)
                      (eq (setq parent (file-name-as-directory
                                        (expand-file-name "..")))
                          default-directory)))
        (setq default-directory parent)))
    (let ((cdup (git--rev-parse "--show-cdup")))
      (git--concat-path default-directory (car (split-string cdup "\n"))))))

(defun git--get-relative-to-top(filename)
  (file-relative-name filename
                      (git--get-top-dir (file-name-directory filename))))

(defun git--ls-unmerged (&rest files)
  "Get the list of unmerged files. Returns an association list of
\(stage . git--fileinfo), where stage is one of 1, 2, 3. If FILES is specified,
only checks the specified files. The list is sorted by filename."
  
  (let (fileinfo)
    (with-temp-buffer
      (apply #'git--exec-buffer "ls-files" "-t" "-u" "-z" files)
      (goto-char (point-min))

      (let ((regexp (git--build-reg git--reg-perm    ; matched-1
                                    git--reg-space
                                    git--reg-sha1    ; matched-2
                                    git--reg-blank
                                    git--reg-stage   ; matched-3
                                    git--reg-blank
                                    git--reg-file))) ; matched-4

        (while (re-search-forward regexp nil t)
          (let ((perm (match-string 1))
                (sha1 (match-string 2))
                (stage (match-string 3))
                (file (match-string 4)))

              (push
               (cons (string-to-number stage)
                     (git--create-fileinfo file 'blob sha1 perm nil 'unmerged))
               fileinfo)))))
    (sort fileinfo #'(lambda(cell1 cell2)
                       (git--fileinfo-lessp (cdr cell1) (cdr cell2))))))

(defun git--ls-files (&rest args)
  "Execute 'git-ls-files' with 'args' and return the list of the
'git--fileinfo'. Does not differentiate between 'modified and
'staged."

  (let (fileinfo)
    (with-temp-buffer
      (apply #'git--exec-buffer "ls-files" "-t" "-z" args)
      (goto-char (point-min))

      (let ((regexp (git--build-reg git--reg-status ; matched-1
                                    git--reg-blank
                                    git--reg-file))) ; matched-2

        (while (re-search-forward regexp nil t)
          (let ((stat (match-string 1))
                (file (match-string 2)))

            (push (git--create-fileinfo file 'blob nil nil nil
                                        (git--interpret-to-state-symbol stat))
                  fileinfo)))))
    (sort fileinfo 'git--fileinfo-lessp)))

(defsubst git--to-type-sym (type)
  "Change string symbol type to 'blob or 'tree"
  
  (cond ((string= type "blob") 'blob)
        ((string= type "tree") 'tree)
        (t (error "strange type : %s" type))))

(defun git--ls-tree (&rest args)
  "Execute git-ls-tree with args and return the result as the list of 'git--fileinfo'"
  
  (let (fileinfo)
    (with-temp-buffer
      (apply #'git--exec-buffer "ls-tree" "-z" args)
      (goto-char (point-min))

      (let ((regexp (git--build-reg git--reg-perm    ; matched-1
                                    git--reg-space
                                    git--reg-type    ; matched-2
                                    git--reg-space
                                    git--reg-sha1    ; matched-3
                                    git--reg-tab
                                    git--reg-file))) ; matched-4

        (while (re-search-forward regexp nil t)
          (let ((perm (match-string 1))
                (type (match-string 2))
                (sha1 (match-string 3))
                (file (match-string 4)))

            (push (git--create-fileinfo file
                                        (git--to-type-sym type)
                                        sha1
                                        perm
                                        nil
                                        'uptodate)
                  fileinfo)))))
    (sort fileinfo 'git--fileinfo-lessp)))

(defun git--merge (&rest args)
  (apply #'git--exec-string "merge" args))

(defsubst git--branch (&rest args)
  (apply #'git--exec-string "branch" args))

(defun git--abbrev-commit(commit &optional size)
  "Returns a short yet unambiguous SHA1 checksum for a commit. The default
SIZE is 5, but it will be longer if needed (due to conflicts)."
  (git--trim-string
   (git--exec-string "rev-list" "--abbrev-commit"  "--max-count=1"
                     (format "--abbrev=%d" (or size 5)) commit)))

(defsubst git--today ()
  (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S"))

(defsubst git--interpret-state-mode-color (stat)
  "Interpret git state symbol to mode line color"

  (case stat
    ('modified "tomato"      )
    ('unknown  "gray"        )
    ('added    "blue"        )
    ('deleted  "red"         )
    ('unmerged "purple"      )
    ('uptodate "GreenYellow" )
    ('staged   "yellow"      )
    (t "red")))



;;-----------------------------------------------------------------------------
;; git application
;;-----------------------------------------------------------------------------

(defsubst git--managed-on-git? ()
  "Check see if vc-git mode is on the vc-git"

  (not (string-match "fatal: Not a git repository"
                     (git--rev-parse "HEAD"))))

(defun git--status-file (file)
  "Return the status of the file"
  
  (let ((fileinfo (git--status-index file)))
    (unless fileinfo (setq fileinfo (git--ls-files file)))
    (when (= 1 (length fileinfo))
      (git--fileinfo->stat (car fileinfo)))))

(defun git--branch-list ()
  "Get branch list"

  (let ((branchs)
        (regexp (concat "[ *]+" git--reg-branch "\n")))
    
    (with-temp-buffer
      (git--exec-buffer "branch" "-l")
      (goto-char (point-min))

      (while (re-search-forward regexp nil t)
        (let ((branch (match-string 1)))
          (unless (string= branch "(no branch)")
            (push branch branchs)))))


    branchs))

(defsubst git--select-branch (&rest excepts)
  "Select the branch"

  (let ((branches (git--branch-list)))
    (git--select-from-user
     "Select Branch : "
     (delq nil (mapcar (lambda (b) (unless (member b excepts) b))
                       branches)))))
                         
(defun git--symbolic-commits (&optional reftypes)
  "Find symbolic names referring to commits, using git-for-each-ref.
REFTYPES is a list of types of refs under .git/refs ; by default,
 (\"heads\" \"tags\" \"remotes\") , which gives branches, tags and remote
branches. Returns a list of the refs found (as strings), in the order
dictated by REFTYPES, then alphabetical."
  (let* ((reftypes (or reftypes '("heads" "tags" "remotes")))
         (git-ref-args (mapcar #'(lambda(reftype) (concat "refs/" reftype))
                               reftypes))
         ;; assoc list of reftype -> list of matches in reverse order
         (results (mapcar #'(lambda(reftype) (cons reftype nil)) reftypes)))
  (with-temp-buffer
    (apply #'git--exec-buffer "for-each-ref" "--format=%(objecttype) %(refname)"
           " --" git-ref-args)
    (goto-char (point-min))
    (while (re-search-forward "^commit refs/\\([^/]*\\)/\\(.*\\)$" nil t)
      ;; omit e.g. remotes/origin/HEAD, which is a confusing duplicate
      (unless (and (equal "remotes" (match-string 1))
                   (string-match "/HEAD$" (match-string 2)))
        (let ((result-cell (assoc (match-string 1) results)))
          (setcdr result-cell (cons (match-string 2) (cdr result-cell))))))
    )
  ;; reverse (destructively) each result type, then concatenate
  (apply #'append (mapcar #'(lambda(result-cell) (nreverse (cdr result-cell)))
                          results))))

(defvar git--revision-history nil "History for selecting revisions")

(defsubst git--select-revision (prompt &optional prepend-choices)
  "Offer the user a list of human-readable revisions to choose from. By default,
it shows branches, tags and remotes; additional choices can be
specified as a list."
  
  (git--select-from-user prompt
                         (append prepend-choices (git--symbolic-commits))
                         git--revision-history))
                                        
(defun git--maybe-ask-and-commit(after-func)
  "Helper for functions that switch trees. If there are pending
changes, asks the user whether they want to commit, then pops up
a commit buffer (and returns). Once the user has committed (or
immediately, if they chose not to or there are no pending
changes), AFTER-FUNC is called, which should do the tree
switching along with any confirmations. The return value is either the
pending commit buffer or nil if the buffer wasn't needed."
  ;; git status -a tells us if there's anything to commit
  (if (and (eq 0 (git--exec "status" nil nil "-a"))
           (y-or-n-p "Commit your pending changes first? (if not, they will be merged into the new tree) "))
      (with-current-buffer (git-commit-all)
        (add-hook 'git--commit-after-hook after-func t t) ; append, local
        (current-buffer)) 
    (funcall after-func)
    nil))
    

;;-----------------------------------------------------------------------------
;; vc-git integration
;;-----------------------------------------------------------------------------

(defsubst git--in-vc-mode? ()
  "Check see if in vc-git is under vc-git"
  
  (and vc-mode (string-match "^ Git" (substring-no-properties vc-mode))))

(defun git--update-modeline ()
  "Update modeline state dot mark properly"
  
  ;; mark depending on the fileinfo state
  (when (and buffer-file-name (git--in-vc-mode?))
    (git--update-state-mark
     (git--status-file (file-relative-name buffer-file-name)))))

(defalias 'git-history 'git-log-all)

(defadvice vc-find-file-hook (after git--vc-git-find-file-hook activate)
  "vc-find-file-hook advice for synchronizing with vc-git interface"

  (when (git--in-vc-mode?) (git--update-modeline)))

(defadvice vc-after-save (after git--vc-git-after-save activate)
  "vc-after-save advice for synchronizing when saving buffer"

  (when (git--in-vc-mode?) (git--update-modeline)))

(defadvice vc-next-action (around git--vc-git-next-action activate)
  "vc-next-action advice for synchronizing when committing"

  ;; vc-mode if nil -> fork git and ask
  ;;         else try to parse git -> if "git" -> ok
  ;;                                  else -> no

  (let ((on-git? (or (git--in-vc-mode?)
                     (unless vc-mode (git--managed-on-git?))))
        (filename ""))

    (when buffer-file-name
      (setq filename (file-relative-name buffer-file-name)))

    (if on-git?
      (case (git--status-file filename)
        ('modified (git-commit-all))    ; modified -> commit
        ('staged (git-commit-all))      ; staged -> commit
        ('unknown (git--add filename))  ; unknown  -> add
        ('unmerged (git--add filename)) ; unmerged -> add
        (t (git--add filename)))        ; TODO : add more
      ad-do-it)))

;;-----------------------------------------------------------------------------
;; public functions
;;-----------------------------------------------------------------------------

(defun git--config-get-author ()
  "Find appropriate user.name"

  (let ((config-user-name (git--config "user.name")))
      (or (and (not (string= config-user-name "")) config-user-name)
          (and (fboundp 'user-full-name) (user-full-name))
          (and (boundp  'user-full-name) user-full-name))))

(defun git--config-get-email ()
  "Find appropriate user.email"
  
  (let ((config-user-email (git--config "user.email")))
    (or (and (not (string= config-user-email "")) config-user-email)
        (and (fboundp 'user-mail-address) (user-mail-address))
        (and (boundp 'user-mail-address) user-mail-address))))

(defun git--insert-log-header-info ()
  "Insert the log header to the buffer"

  (insert git--log-header-line  "\n"
          "# Branch : " (git--current-branch)     "\n"
          "# Author : " (git--config-get-author)  "\n"
          "# Email  : " (git--config-get-email)   "\n"
          "# Date   : " (git--today)              "\n"))

;; Internal variables for commit
(defvar git--commit-after-hook nil
  "Hooks to run after comitting (and killing) the commit buffer.")
(defvar git--commit-args nil
  "Args to be passed to the current git commit once done editing.")
(defvar git--commit-targets nil
  "Records the targets parameter of `git-commit'. Buffer-local.")
(defvar git--commit-last-diff-file-buffer nil
  "Stores last diff buffer launched from a commit buffer.")

(defun git--commit-buffer ()
  "When you press C-cC-c after editing log, this function is called
Trim the buffer log and commit"
  
  (interactive)

  ;; check buffer
  (unless (string= (buffer-name (current-buffer))
                   git--commit-log-buffer)
    (error "Execute git commit on %s buffer" git--commit-log-buffer))

  ;; trail and commit
  (save-excursion
    (goto-char (point-min))

    (let ((begin (search-forward git--log-sep-line nil t))
          (end   (search-forward git--log-sep-line nil t)))
      (when (and begin end)
        (setq end (- end (length git--log-sep-line)))
        ;; TODO sophisticated message
        (message (apply #'git--commit
                        (git--trim-string (buffer-substring begin end))
                        git--commit-args)))))

  ;; update state marks, either for the files committed or the whole repo
  (git--update-all-state-marks
   (if (eq t git--commit-targets) nil git--commit-targets))
   
  ;; close window and kill buffer. Some gymnastics are needed to preserve
  ;; the buffer-local value of the after-hook.
  (let ((local-git--commit-after-hook
         (when (local-variable-p 'git--commit-after-hook)
           (cdr git--commit-after-hook)))) ; skip the "t" for local
    (unless (one-window-p t) (delete-window))
    (kill-buffer git--commit-log-buffer)
  
    ;; hooks (e.g. switch branch)
    (run-hooks 'local-git--commit-after-hook 'git--commit-after-hook)))

(defun git--resolve-fill-buffer (template side)
  "Make the new buffer based on the conflicted template on each
side ('ours or 'theirs)"

  (let* ((filename (file-relative-name (buffer-file-name template)))
         (buffer-name (format "*%s*: %s"
                              (capitalize (symbol-name side)) filename))
         (buffer (get-buffer-create buffer-name))
         (msg "Malformed conflict marker"))

    (with-current-buffer buffer
      (let ((buffer-read-only nil) (erase-buffer)))
      (insert-buffer-substring template)

      ;; find first mark
      (goto-char (point-min))
      (while (re-search-forward "^<<<<<<< \\([^\n]+\\)\n" nil t)
        (replace-match "")

        (let (conflict-begin conflict-sep conflict-end)
          (setq conflict-begin (match-beginning 0))

          ;; find mid mark
          (unless (re-search-forward "^=======\n" nil t) (error msg))
          (replace-match "")

          (setq conflict-sep (match-beginning 0))

          ;; find last mark
          (unless (re-search-forward "^>>>>>>> \\([^\n]+\\)\n" nil t) (error msg))
          (replace-match "")

          (setq conflict-end (match-beginning 0))

          (case side
            ('ours (delete-region conflict-sep conflict-end))
            ('theirs (delete-region conflict-begin conflict-sep))
            (t (error "Side must be one of 'ours or 'theirs"))))))
    buffer-name))

(defun git--resolve-fill-base()
  "Assumes that the current buffer is an unmerged file, gets its \"base\"
revision from git into a buffer named \"*Base*: filename\" and returns that
buffer. If there is no common base, returns nil."
  (let* ((rel-filename (file-relative-name buffer-file-name))
         (stage-and-fileinfo (git--ls-unmerged rel-filename))
         (base-fileinfo (cdr-safe (assq 1 stage-and-fileinfo)))
         (base-buffer (when base-fileinfo
                        (git--cat-file (format "*Base*: %s" rel-filename)
                                       "blob"
                                       (git--fileinfo->sha1 base-fileinfo)))))
    base-buffer))

(defun git-merge ()
  "Git merge"

  (interactive)

  (let ((branch (git--select-branch (git--current-branch))))
    (git--merge branch)
    (git-status ".")))

(defun git--resolve-merge-buffer (result-buffer)
  "Implementation of resolving conflicted buffer"
  (interactive)

  (setq result-buffer (current-buffer))
  
  (let* ((filename (file-relative-name buffer-file-name))
         (our-buffer (git--resolve-fill-buffer result-buffer 'ours))
         (their-buffer (git--resolve-fill-buffer result-buffer 'theirs))
         (base-buffer (git--resolve-fill-base))
         (config (current-window-configuration))
         (ediff-default-variant 'default-B))

    ;; set merge buffer first
    (set-buffer (if base-buffer
                    (ediff-merge-buffers-with-ancestor
                     our-buffer their-buffer base-buffer)
                  (ediff-merge-buffers our-buffer their-buffer)))

    (add-hook
     'ediff-quit-hook
     (lexical-let ((saved-config config)
                   (saved-result-buffer result-buffer)
                   (saved-base-buffer base-buffer))
         #'(lambda ()
             (let ((buffer-A ediff-buffer-A)
                   (buffer-B ediff-buffer-B)
                   (buffer-C ediff-buffer-C))
               (ediff-cleanup-mess)
               (set-buffer saved-result-buffer)
               (erase-buffer)
               (insert-buffer-substring buffer-C)
               (kill-buffer buffer-A)
               (kill-buffer buffer-B)
               (kill-buffer buffer-C)
               (when saved-base-buffer (kill-buffer saved-base-buffer))
               (set-window-configuration saved-config)
               (message "Conflict resolution finished, you may save the buffer"))))
     nil t)                             ; hook is prepend, local
    (message "Please resolve conflicts now, exit ediff when done")))

(defun git-resolve-merge ()
  "Resolve merge for the current buffer"
  
  (interactive)
  (git--resolve-merge-buffer (current-buffer)))

(defconst git--commit-status-font-lock-keywords
  '(("^#\t\\([^:]+\\): +[^ ]+$"
     (1 'git--bold-face))
    ("^# \\(Branch\\|Author\\|Email\\|Date\\) +:" (1 'git--bold-face))
    ("^# \\(-----*[^-]+-----*\\).*$" (1 'git--log-line-face))))
;; (makunbound 'git--commit-status-font-lock-keywords)

(define-button-type 'git--commit-diff-committed-link
  'help-echo "mouse-2, RET: view changes that will be committed"
  'action 'git--commit-diff-file)
(define-button-type 'git--commit-diff-uncomitted-link
  'help-echo "mouse-2, RET: view changes that will NOT be committed"
  'action 'git--commit-diff-file)

(defun git--commit-buttonize-filenames (single-block type)
  "Makes clickable buttons (aka hyperlinks) from filenames in git-status
outputs. The filenames are found with a simple regexp.
If SINGLE-BLOCK, stops after the first \"block\" of files, i.e.
when it would move forward more than one line after a filename. The buttons
created are of the given TYPE. Leaves the cursor at the end of the last
button, or at the end of the file if it didn't create any."
  (let (last-match-pos)
    (while (and (re-search-forward "^#\t[^:]+: +\\(.*\\)" nil t)
                (or (not single-block)
                    (not last-match-pos)
                    (<= (count-lines last-match-pos (point)) 2)))
      (make-text-button (match-beginning 1) (match-end 1)
                        'type type)
      (setq last-match-pos (point)))
    (when last-match-pos (goto-char last-match-pos))))

(defun git--commit-diff-file (button)
  "Click handler for filename links in the commit buffer"
  (with-current-buffer git--commit-log-buffer
    (let ((diff-from "HEAD") (diff-to nil)) ; rev1, rev2 inputs to diff--many
      ;; the commit-index case is the complicated one, adjust.
      (if (eq nil git--commit-targets)
          (if (eq (button-type button) 'git--commit-diff-committed-link)
              (setq diff-to t)          ; diff HEAD -> index
            (setq diff-from nil))       ; diff index -> working
        )
      ;; Use diff--many which is much less intrusive than ediff. Reuse the
      ;; same buffer so the user can easily look at multiple files in turn.
      (let ((buffer
            (git--diff-many (list (button-label button)) diff-from diff-to t
                            git--commit-last-diff-file-buffer)))
        ;; Subtle: git-diff-many switched buffers
        (with-current-buffer git--commit-log-buffer
          (set (make-local-variable 'git--commit-last-diff-file-buffer)
               buffer)))
    )))

(defun git-commit (&optional targets)
  "Does git commit with a temporary prompt buffer. TARGETS can be nil
\(commit staged files), t (commit all) or a list of files. Returns the buffer."

  (interactive)
  ;; Don't save anything on commit-index
  (when targets (git--maybe-ask-save (if (eq t targets) nil targets)))
  
  (let ((cur-pos nil)
        (buffer (get-buffer-create git--commit-log-buffer))
        (current-dir default-directory))
    (with-current-buffer buffer
      ;; Tell git--commit-buffer what to do
      (set (make-local-variable 'git--commit-targets) targets)
      (set (make-local-variable 'git--commit-args)
           (cond ((eq nil targets) '())
                 ((eq t targets) '("-a"))
                 ((listp targets) targets)
                 (t (error "Invalid targets: %S" targets))))
                 
      (local-set-key "\C-c\C-c" 'git--commit-buffer)
      (local-set-key "\C-c\C-q" 'git--quit-buffer)
      (buffer-disable-undo)
      (erase-buffer)
      (flyspell-mode 0)               ; disable for the text we insert
      (cd current-dir)                ; if we reused the buffer

      (set (make-local-variable 'font-lock-defaults)
           (list 'git--commit-status-font-lock-keywords t))
      (when global-font-lock-mode (font-lock-mode t))
      ;; insert info
      (git--insert-log-header-info)

      ;; real log space
      (insert (propertize git--log-sep-line 'face 'git--log-line-face) "\n")

      (insert "\n")
      ;; Insert .git/MERGE_MSG if exists
      (let ((merge-msg-file
             (expand-file-name ".git/MERGE_MSG" (git--get-top-dir))))
        (when (file-readable-p merge-msg-file)
          (git--please-wait (format "Reading %s" merge-msg-file)
            (insert-file-contents merge-msg-file)
            (goto-char (point-max))        ; insert-file didn't move point
            (insert "\n"))))
      (setq cur-pos (point))
      (insert "\n\n")

      ;;git status -- give same args as to commit
      (insert git--log-sep-line "\n")
      (git--please-wait "Reading git status"
        (unless (eq 0 (apply #'git--exec-buffer "status" git--commit-args))
          (kill-buffer nil)
          (error "Nothing to commit%s"
                 (if (eq t targets) "" ", try git-commit-all"))))

      ;; Remove "On branch blah" it's redundant
      (goto-char cur-pos)
      (when (re-search-forward "^# On branch.*$" nil t)
        (delete-region (line-beginning-position) (line-beginning-position 2)))

      ;; Buttonize files to be committed, with action=diff. Assume
      ;; that the first block of files is the one to be committed, and all
      ;; others won't be committed.
      (goto-char cur-pos)
      (git--commit-buttonize-filenames t 'git--commit-diff-committed-link)
      (git--commit-buttonize-filenames nil 'git--commit-diff-uncomitted-link)
      ;; Delete diff buffers when we're gone
      (add-hook 'kill-buffer-hook
                #'(lambda()
                    (ignore-errors
                      (kill-buffer git--commit-last-diff-file-buffer)))
                t t)                    ; append, local
      
      ;; Set cursor to message area
      (goto-char cur-pos)

      (when git--log-flyspell-mode (flyspell-mode t))

      ;; comment hook
      (run-hooks 'git-comment-hook)

      (buffer-enable-undo)
      (message "Type 'C-c C-c' to commit, 'C-c C-q' to cancel"))
    (pop-to-buffer buffer)
    buffer))

(defun git-commit-file ()
  "Runs git commit with the file in the current buffer, or with the selected
files in git-status. Only changes to those files will be committed."
  (interactive)
  (git--require-buffer-in-git)
  ;; Here and in other functions below we rely on the fact that git-status has
  ;; surely been loaded if the current major mode is git-status
  (git-commit (git--if-in-status-mode
                  (git--status-view-marked-or-file)
                (list (file-relative-name buffer-file-name)))))

(defun git-init (dir)
  "Initialize the git repository"

  (interactive "DGit Repository: ")
  (message (git--trim-string (git--init dir)))
  (git-config-init))

(defun git-init-from-archive (file)
  "Initialize the git repository based on the archive"
  
  (interactive "fSelect archive: ")

  ;; TODO dirty enough
  ;; simple rule to uncompress -> dired-compress not working propery
  (with-temp-buffer
    (cd (file-name-directory file))
    
    (when (file-exists-p file)
      (let ((ext (file-name-extension file)))
        (cond
         ((string= ext "tar")
          (shell-command (format "tar xf \"%s\"" (file-relative-name file))))
         ((string= ext "gz")
          (shell-command (format "tar xzf \"%s\"" (file-relative-name file))))
         (t (error (concat ext " is not supported"))))))

    (let ((dir (file-name-sans-extension
                (file-name-sans-extension file))))
      ;; move to git repository
      (cd dir)

      ;; init
      (git-init dir)
      (git--add ".")
      (git-commit-all))))

(defun git-clone (dir)
  "Clone from repository"
  
  (interactive "DLocal Directory : ")
  (let ((repository
         (git--select-from-user "Clone repository: "
                                git--repository-bookmarks
                                git--repository-history
                                "")))
    (with-temp-buffer
      (cd dir)
      (git--clone repository))))

(defun git-reset-hard (&optional commit)
  "Reset the current branch and the working directory to the given COMMIT.
\(prompts the user if not specified)."

  (interactive (list (git--select-revision
                      "Reset branch and working directory to: ")))
  (let ((saved-head (git--abbrev-commit "HEAD" 10)))
    (git--reset "--hard" commit)
    (git--maybe-ask-revert)
    (message "You can recover the old HEAD as %s" saved-head)))

;; TODO: Maybe support reverting multiple commits at once. Would need nicer
;; commit support.
(defun git-revert (commit)
  "Revert a commit, prompting the user if unspecified."
  (interactive
   ;; TODO: for this to really make sense we need some SHA1 completion
   (list (git--select-revision "Revert commit: ")))
  (let ((output (git--trim-string (git--exec-string "revert" commit))))
    (git--maybe-ask-revert)
    (message output)))
  
(defcustom gitk-program "gitk"
  "The command used to launch gitk."
  :type '(string)
  :group 'git-emacs)

(defun gitk ()
  "Launch gitk in emacs"

  (interactive)
  (start-process "gitk" nil gitk-program))
    
(defun git-checkout (&optional commit confirm-prompt &rest args)
  "Checkout a commit. When COMMIT is nil, shows tags and branches
for selection. May pop up a commit buffer to commit pending
changes; in this case, the function is asynchronous and returns
that commit buffer. If CONFIRM-PROMPT is non-nil, ask for
confirmation, replacing %s in CONFIRM-PROMPT with the commit.
Although the function allows ARGS, it is not suitable for
checking out individual files due to the assumptions it makes
about the nature of the checkout (full)."
  (interactive)
  (git--maybe-ask-save)
  (git--maybe-ask-and-commit
   (lexical-let ((commit (or commit (git--select-revision "Checkout: ")))
                 (confirm-prompt confirm-prompt)
                 (repo-dir default-directory)
                 (args args))
     (lambda()
       (when (or (not confirm-prompt)
                 (y-or-n-p (format confirm-prompt commit)))
         (apply #'git--checkout commit args)
         (git--maybe-ask-revert repo-dir))))))


(defalias 'git-create-branch 'git-checkout-to-new-branch)

(defun git-checkout-to-new-branch (&optional branch)
  "Checkout new branch, based on commit prompted from the user"
  (interactive)
  (git--maybe-ask-save)
  (git--maybe-ask-and-commit
   (lexical-let ((branch branch)
                 (repo-dir default-directory))
     (lambda()
       (let* ((branch (or branch (read-from-minibuffer "Create new branch: ")))
              (commit (git--select-revision
                       (format "Create %s based on: "
                               (git--bold-face branch)))))
         (git--checkout "-b" branch commit)
         (git--maybe-ask-revert repo-dir))))))

(defun git-delete-branch (&optional branch)
  "Delete branch after selecting branch"

  (interactive)
  ;; select branch if not assigned
  (unless branch (setq branch (git--select-branch "master")))

  (let ((saved-head (git--abbrev-commit branch 10))) ; safer abbrev
    (git--branch "-D" branch)              ; we've asked already
    (message "You can recover the deleted branch %s as %s"
             (git--bold-face branch) saved-head)))

(defun git-delete-tag (tag)
  "Delete tag after selecting tag"
  
  (interactive
   (list (git--select-from-user "Delete tag: " (git--tag-list))))
  (let ((saved-tag-target
         (ignore-errors
           (git--trim-string (git--exec-string "rev-parse" "--short" tag)))))
    (git--tag "-d" tag)
    (message "%s tag '%s'; you can recover it as %s"
             (git--bold-face "Deleted") tag saved-tag-target)))

(defun git-regression ()
  "Regression tests on git-emacs, to be enhanced. Must run from a git
repository."

  (interactive)

  ;; git exec
  (assert (string= "\n" (git--exec-string "rev-parse" "--show-cdup")))
  (assert (string= (expand-file-name "./") (git--get-top-dir ".")))
  (assert (string= (expand-file-name "./")
                   (git--get-top-dir "./nO/sUCH/dIrectory/Exists")))

  ;; create status buffer
  (require 'git-status)
  (assert (string= (buffer-name (git--create-status-buffer "."))
                   (git--status-buffer-name ".")))

  ;; open status buffer
  (assert (string= (buffer-name (git--create-status-buffer "."))
                   (git--status-buffer-name ".")))

  (git--kill-status-buffer ".")

  ;; testing
  (assert (null (string-match "asdf/" "asdf")))

  ;; tag stuff
  (ignore-errors (git-delete-tag "git-regression-test-tag"))
  (unwind-protect
      (progn
        (assert (null (git-tag "git-regression-test-tag")))
        (assert (stringp (git-tag "git-regression-test-tag"))))
    (assert (git-delete-tag "git-regression-test-tag")))

  (message "git-regression passed"))

(defun git-cmd (str)
  "git-cmd for user"

  (interactive "s>> git ")
  (message (git--trim-tail
            (apply #'git--exec-string (split-string str)))))

(defun git--cat-file (buffer-name &rest args)
  "Execute git-cat-file and return the buffer with the file content"
  
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer

      ;; set buffer writable
      (setq buffer-read-only nil)
      (erase-buffer)

      ;; set tricky auto mode for highlighting
      (let ((buffer-file-name buffer-name)) (set-auto-mode))

      ;; ok cat file to buffer
      (apply #'git--exec-buffer "cat-file" args)

      ;; set buffer readonly & quit
      (setq buffer-read-only t)

      ;; check see if failed
      (goto-char (point-min))
      (when (looking-at "^\\([Ff]atal\\|[Ff]ailed\\|[Ee]rror\\):")
        (let ((msg (buffer-string)))
          (kill-buffer nil)
          (setq buffer nil)
          (error (git--trim-tail msg)))))
    buffer))

(defun git--diff (file rev &optional before-ediff-hook after-ediff-hook)
  "Starts an ediff session between the FILE and its specified revision.
REVISION should include the filename. The latter should not
include the filename, e.g. \"HEAD:\". If BEFORE-EDIFF-HOOK is specified,
it is executed as an ediff setup hook. If AFTER-EDIFF-HOOK is specified,
it is executed as an ediff quit hook. Both hooks run in the ediff context,
i.e. with valid ediff-buffer-A and B variables, among others.
"
  (let* ((buf1 (find-file-noselect file))
	 (buf2 nil)
	 (config (current-window-configuration)))
  
    ;; build buf2
    (with-temp-buffer 
      (let ((abspath (expand-file-name file))
	    (filename nil))

	;; get relative to git root dir
	(cd (git--get-top-dir (file-name-directory abspath)))
	(let ((filerev (concat rev (file-relative-name abspath))))
              (setq buf2 (git--cat-file (if (equal rev ":")
                                            (concat "<index>" filerev)
                                          filerev)
                                        "blob" filerev)))))

    (set-buffer
     (ediff-buffers buf1 buf2
                    (when before-ediff-hook (list before-ediff-hook))))
    
    (add-hook 'ediff-quit-hook
              (lexical-let ((saved-config config)
                            (saved-after-ediff-hook after-ediff-hook))
                #'(lambda ()
                    (let ((buffer-B ediff-buffer-B))
                      (unwind-protect ; an error here is a real mess
                          (when saved-after-ediff-hook
                            (funcall saved-after-ediff-hook))
                        (ediff-cleanup-mess)
                        (kill-buffer buffer-B)
                        (set-window-configuration saved-config)))))
              nil t)                     ; prepend, buffer-local
    ))

(defun git--diff-many (files &optional rev1 rev2 dont-ask-save reuse-buffer)
  "Shows a diff window for the specified files and revisions, since we can't
do ediff on multiple files. FILES is a list of files, if empty the whole
git repository is diffed. REV1 and REV2 are strings, interpreted roughly the
same as in git diff REV1..REV2. If REV1 is unspecified, we use the index.
If REV2 is unspecified, we use the working dir. If REV2 is t, we use the index.
If DONT-ASK-SAVE is true, does not ask to save modified buffers under the
tree (e.g. old revisions). If REUSE-BUFFER is non-nil and alive, uses that
buffer instead of a new one."
  (unless dont-ask-save (git--maybe-ask-save files))
  (when (and (not rev1) (eq t rev2) (error "Invalid diff index->index")))
  (let* ((rel-filenames (mapcar #'file-relative-name files))
         (friendly-rev1 (or rev1 "<index>"))
         (friendly-rev2 (if (eq t rev2) "<index>" (or rev2 "<working>")))
         (diff-buffer-name (format "*git diff: %s %s..%s*"
                                   (case (length files)
                                     (0 (abbreviate-file-name
                                         (git--get-top-dir)))
                                     (1 (file-relative-name (first files)))
                                     (t (format "%d files" (length files))))
                                   friendly-rev1 friendly-rev2))
         (buffer (if (buffer-live-p reuse-buffer)
                     (with-current-buffer reuse-buffer
                       (rename-buffer diff-buffer-name t)
                       reuse-buffer)
                   (get-buffer-create diff-buffer-name))))
    (with-current-buffer buffer
      (buffer-disable-undo)
      (let ((buffer-read-only nil)) (erase-buffer))
      (setq buffer-read-only t)
      (diff-mode)
      ;; See diff-mode.el, search for "neat trick", for why this is necessary
      (let ((diff-readonly-map (assq 'buffer-read-only
                                     minor-mode-overriding-map-alist)))
        (when diff-readonly-map
          (setcdr diff-readonly-map (copy-keymap (cdr diff-readonly-map)))
          (define-key (cdr diff-readonly-map) "q" 'git--quit-buffer)))
      (let ((diff-qualifier
             (if rev1
                 (if (eq t rev2) (list "--cached" rev1)
                   (if rev2 (list (format "%s..%s" rev1 rev2))
                     (list rev1)))
               ;; rev1 is index. swap sides of git diff when diffing
               ;; against the index, for consistency (rev1 -> rev2)
               (if rev2 (list "--cached" "-R" rev2)
                 '()))))
        (apply #'vc-do-command buffer 'async "git" nil "diff"
               (append diff-qualifier (list "--") rel-filenames)))
      (vc-exec-after `(goto-char (point-min))))
    (pop-to-buffer buffer)))


(defun git-config-init ()
  "Set initial configuration, it query the logined user information"

  (interactive)

  (let ((name (git--trim-string (git--config "user.name")))
        (email (git--trim-string (git--config "uscer.email"))))

    (when (or (null name) (string= "" name))
      (setq name (read-from-minibuffer "User Name : "
                                       (git--config-get-author)))

      (git--config "--global" "user.name" name))
    
    (when (or (null email) (string= "" email))
      (setq email (read-from-minibuffer "User Email : "
                                        (git--config-get-email)))

      (git--config "--global" "user.email" email))

    (message "Set user.name(%s) and user.email(%s)" name email)))

(defun git-ignore (ignored-opt)
  "Add ignore file"
  
  (interactive "sIgnore Option : ")

  (with-temp-buffer
    (insert ignored-opt "\n")
    (append-to-file (point-min) (point-max) ".gitignore")))
  
(defun git-switch-branch (&optional branch)
  "Git switch branch, selecting from a list of branches."
  (interactive)
  (git--maybe-ask-save)
  (git--maybe-ask-and-commit
   (lexical-let ((branch branch)
                 (repo-dir default-directory))
     (lambda()
       (let ((branch (or branch (git--select-branch (git--current-branch)))))
         (git--checkout branch)
         (git--maybe-ask-revert repo-dir))))))

(defun git-add ()
  "Add files to index. If executed in a buffer currently under git control,
adds the current contents of that file to the index. Otherwise, prompts
for new files to add to git."
  (interactive)
  (if (not (and buffer-file-name (git--in-vc-mode?)))
      (git-add-new)
    (let ((rel-filename (file-relative-name buffer-file-name)))
      (if (not (git--ls-files "--modified" "--" rel-filename))
          (error "%s is already current in the index" rel-filename)
        (when (y-or-n-p (format "Add the current contents of %s to the index? "
                                rel-filename))
          (git--add rel-filename)
          (git--update-modeline))))))

(defun git-add-new ()
  "Add new files to the index, prompting the user for filenames or globs"
  ;; TODO: ido doesn't give good feedback on globs
  (let* ((files (git--select-from-user "Add new files (glob) >> "
                                       (mapcar #'(lambda (fi)
                                                   (git--fileinfo->name fi))
                                               (git--ls-files "--others"))))
         (matched-files (mapcar #'(lambda (fi) (git--fileinfo->name fi))
                                    (git--ls-files "--others" "--" files))))
    (if (not matched-files) (error "No files matched \"%s\"" files)
      (let ((output (replace-regexp-in-string "[\s\n]+$" ""
                                             (git--add matched-files))))
        (if (not (equal output ""))
            (error "git: %s" output)
          (message "Added %s to git" (git--join matched-files ", "))
          ;; refresh vc-git.
          (dolist (filename matched-files)
            (let ((added-file-buffer (get-file-buffer filename)))
              (when added-file-buffer
                (with-current-buffer added-file-buffer (vc-find-file-hook))))))
      ))))


;;-----------------------------------------------------------------------------
;; branch mode
;;-----------------------------------------------------------------------------

(defvar git--branch-mode-map nil)
(defvar git--branch-mode-hook nil)

(let ((map (make-keymap)))
  (suppress-keymap map)

  (define-key map "q"     'git--branch-mode-quit)
  (define-key map "n"     'next-line)
  (define-key map "p"     'previous-line)

  (define-key map "d"     'git--branch-mode-delete)
  (define-key map "c"     'git--branch-mode-create)
  (define-key map "s"     'git--branch-mode-switch)
  (define-key map "\C-m"  'git--branch-mode-switch)

  (setq git--branch-mode-map map))

(easy-menu-define gitemacs-menu-branch git--branch-mode-map
  "Git-Branch"
  `("Git-Branch"
    ["Next Line" next-line t]
    ["Previous Line" previous-line t]
    ["Switch Branch" git--branch-mode-switch t]
    ["Create New Branch" git--branch-mode-create t]
    ["Delete Branch" git--branch-mode-delete]
    ["Quit" git--branch-mode-quit t]))


(defun git--branch-mode-throw (data)
  "Git branch mode template to exit buffer"

  (let ((branch (buffer-substring (point) (line-end-position))))
    (throw 'git--branch-mode-selected (cons data branch))))

(defun git--branch-mode-quit ()
  "Git branch mode quit"

  (interactive)
  (throw 'git--branch-mode-selected nil))

(defun git--branch-mode-delete ()
  "Git branch mode delete"

  (interactive)
  (git--branch-mode-throw 'delete))

(defun git--branch-mode-switch ()
  "Git branch mode switch"

  (interactive)
  (git--branch-mode-throw 'switch))

(defun git--branch-mode-create ()
  "Git branch mode checkout"

  (interactive)
  (git--branch-mode-throw 'create))

(defun git--branch-mode ()
  "Set current buffer as branch-mode"
  
  (kill-all-local-variables)
  (buffer-disable-undo)

  ;; set major mode
  (setq mode-name "git branch")
  (setq major-mode 'git-branch-mode)

  (use-local-map git--branch-mode-map)

  ;; delete buffer
  (let ((buffer-read-only nil)) (erase-buffer))

  (setq buffer-read-only t)
  (setq header-line-format "Branch List")

  (run-hooks 'git--branch-mode-hook))

;; copied from electric buffer
(defvar git--branch-mode-overlay nil)
(defun git--branch-mode-highlight ()
  "Highlight the one line, it is copied from electric buffer"
  
  (when (eq major-mode 'git-branch-mode)
    (or git--branch-mode-overlay
        (progn
          (make-local-variable 'git--branch-mode-overlay)
          (setq git--branch-mode-overlay (make-overlay (point) (point)))))

    (move-overlay git--branch-mode-overlay
                  (save-excursion (beginning-of-line) (point))
                  (save-excursion (end-of-line) (+ 1 (point))))

    (overlay-put git--branch-mode-overlay 'face 'highlight)))

(defun git--branch-mode-view ()
  "Display the branch list to branch-mode buffer and return the end mark of the buffer"
  
  ;; beginning of buffer name position
  (setq goal-column 3)

  (let ((current-branch (git--current-branch))
        (branch-list (git--branch-list))
        (buffer-read-only nil))

    (dolist (branch branch-list)
      (insert (format "%2s %s\n"
                      (if (string= current-branch branch)
                          (git--bold-face "*") " ")
                      branch)))

    (goto-char (point-min))
    (length branch-list)))

(defun git-branch ()
  "Launch git-branch mode"

  (interactive)

  (let ((selected-branch nil)
        (buffer (get-buffer-create "*git-branch*"))
        (windows (current-window-configuration))
        (nbranchs 0)
        ;; Subtle: a pre-existing *git-branch* buffer might have the wrong dir
        (directory default-directory))

    (with-current-buffer buffer
      (setq default-directory directory)
      ;; set branch mode 
      (git--branch-mode)

      ;; set branch mode view
      (setq nbranchs (git--branch-mode-view))

      ;; pop up
      (electric-pop-up-window buffer)
      (git--branch-mode-loop nbranchs nil)
      
      (unwind-protect
          (setq selected-branch
                (catch 'git--branch-mode-selected
                  (electric-command-loop 'git--branch-mode-selected
                                         nil
                                         t
                                         'git--branch-mode-loop
                                         nbranchs))))

      ;; exit buffer and restore configuration of the windows
      (set-window-configuration windows)
      (kill-buffer buffer)

      ;; interpret command 
      (git--branch-mode-interpret selected-branch))))

(defun git--branch-mode-interpret (selected-branch)
  "git-branch command interpreter,
if 'delete -> call 'git-delete-branch
if 'switch -> call 'git-checkout with tweaked arguments
if 'create -> call git-checkout-to-new-branch"

  (when selected-branch
    (let ((command (car selected-branch))
          (branch (cdr selected-branch)))
    (case command
      ('delete
       (when (y-or-n-p (format "Would you like to %s the branch %s ? "
                               (git--bold-face "delete")
                               (git--bold-face branch)))
         (git-delete-branch branch)))
      ('switch
       (git-checkout branch
                     (format "Switch from branch %s to %s? "
                             (git--bold-face (git--current-branch))
                             (git--bold-face "%s")) ; checkout replaces this
                     )
        )
      ('create (call-interactively 'git-checkout-to-new-branch))))))

(defun git--branch-mode-loop (stat cond)
  "git-branch mode loop interpreter, update the highlight"

  (interactive)

  ;; adjust when end of the branches
  (if (> (line-number-at-pos) stat)
      (previous-line))

  ;; adjust column
  (beginning-of-line)
  (forward-char goal-column)

  ;; highlight
  (git--branch-mode-highlight))

;;-----------------------------------------------------------------------------
;; Expanded diff functions
;;-----------------------------------------------------------------------------

(defun git--require-buffer-in-git ()
  "Error out from interactive if current buffer is not in git revision control,
and it's not a git-status-buffer."
  (if (not (or (and buffer-file-name (git--in-vc-mode?))
               (eq major-mode 'git-status-mode)))
      (error "Current buffer must be a file under git revision control")))

;; The git-diff-* family of functions diffs a buffer or the selected file
;; in git status against HEAD, the index, etc..
                 
(defun git-diff-head()
  "Diff current buffer, or current file in git-status,  against HEAD version,
using ediff."
  (interactive)
  (git--require-buffer-in-git)
  (git--diff (git--if-in-status-mode
                 (git--status-view-select-filename)
               buffer-file-name)
             "HEAD:"))

(defun git-diff-index()
  "Diff current buffer, or current file in git-status,  against version
in index, using ediff"
  (interactive)
  (git--require-buffer-in-git)
  (git--diff (git--if-in-status-mode
                 (git--status-view-select-filename)
               buffer-file-name)
             ;; luckily ":file" means file as currently in the index
             ":"))

;; baseline stuff
(defvar git-baseline-commit '()
  "Association list of (REPOSITORY-DIR . BASELINE-COMMIT). Both
REPOSITORY-DIR and BASELINE-COMMIT are strings.")
;; (makunbound 'git-baseline-commit)  ;;eval to clear variable

(defvar git-baseline-functions '()
  "List of functions that should be offered as choices in git-set-baseline.
Each function will be called with the current buffer inside of the repository
and no parameters; it should return a sha1 string or signal a readable error.
A function will only be offered as a choice if it completes without error
in the repository being prompted for. Use symbols instead of lambdas so
the functions are human-readable.")

(defun git-set-baseline(&optional use-previous)
  "Set and return the baseline commit used in (`git-diff-current' 'baseline).
If the optional parameter use-previous is true and the baseline
commit was already set, simply returns it. The baseline commit is
per-repository and can be optionally stored in .emacs after being set.
The result might be a function, one of git-baseline-functions, if the
user chose so."
  (interactive)
  ;; This function is a bit too long. Consider extracting parts that may
  ;; be useful elsewhere.
  (let* ((repo-dir
          ;; either the repo of the current buffer
          (cond
           (buffer-file-name
            (git--get-top-dir (or (file-name-directory buffer-file-name) "")))
           ((eq major-mode 'git-status-mode)
            (git--get-top-dir default-directory))
           (t
            ;; or one prompted from the user
            (let ((prompted-repo-dir
                   (read-directory-name "Set baseline for repository: "
                                        default-directory nil t)))
              ;; verify that it's a git repo indeed
              (if (equal
                   (expand-file-name (file-name-as-directory prompted-repo-dir))
                   (expand-file-name (git--get-top-dir prompted-repo-dir)))
                  prompted-repo-dir
                (error "Not a git repository: %s" prompted-repo-dir))))))
         ;; canonicalize, for storage / lookup
         (canonical-repo-dir
          (expand-file-name (file-name-as-directory repo-dir)))
         (previous-baseline-assoc
          (assoc canonical-repo-dir git-baseline-commit)))
    ;; found among previous associations?
    (if (and use-previous previous-baseline-assoc)
        (cdr previous-baseline-assoc)
      ;; prompt for new one, possibly a function
      (let* ((names-to-functions
              (apply #'append
                     (mapcar (lambda(func)
                               (when (condition-case nil (funcall func)
                                       (error nil))
                                 (list (cons (format "(%S)" func) func))))
                             git-baseline-functions)))
             (new-baseline-str
              (with-temp-buffer
               (cd canonical-repo-dir)
               (git--select-revision "Select baseline commit: "
                                     (mapcar #'car names-to-functions))))
             (new-baseline (or (cdr-safe
                                (assoc new-baseline-str names-to-functions))
                               new-baseline-str)))
        ;; store in variable
        (if previous-baseline-assoc
            (setcdr previous-baseline-assoc new-baseline)
          (add-to-list 'git-baseline-commit
                       (cons canonical-repo-dir new-baseline)))
        ;; ... which we possibly save in .emacs
        (when (y-or-n-p "Save for future sessions? ")
          (customize-save-variable 'git-baseline-commit
                                   git-baseline-commit))
        new-baseline))))

(defun git-diff-baseline()
  "Diff current buffer against a selectable \"baseline\" commit"
  (interactive)
  (git--require-buffer-in-git)
  (let* ((baseline (git-set-baseline t))
         (baseline-str (if (functionp baseline) (funcall baseline) baseline)))
    (git--diff (git--if-in-status-mode
                   (git--status-view-select-filename)
                 buffer-file-name)
               (concat baseline-str ":"))))

(defun git-diff-other(commit)
  "Diff current buffer against an arbitrary commit"
  (interactive
   (progn
     (git--require-buffer-in-git)
     (list (git--select-revision "Diff against commit: "))))
  (git--require-buffer-in-git)
  (git--diff (git--if-in-status-mode
                 (git--status-view-select-filename)
               buffer-file-name)
             (concat commit ":")))

;; git-diff-all variants
(defun git-diff-all-head (&optional files)
  "Diff all of the repository, or just FILES, against HEAD."
  (interactive)
  (git--diff-many files "HEAD"))

(defun git-diff-all-index (&optional files)
  "Diff all of the repository, or just FILES, against the index."
  (interactive)
  (git--diff-many files))

(defun git-diff-all-baseline (&optional files)
  "Diff all of the repository, or just FILES, against the \"baseline\" commit."
  (interactive)
  (let* ((baseline (git-set-baseline t))
         (baseline-str (if (functionp baseline) (funcall baseline) baseline)))
    (git--diff-many files baseline-str)))

(defun git-diff-all-other (commit &optional files)
  (interactive
   (list (git--select-revision "Diff against commit: ")))
  (git--diff-many files commit))

;;-----------------------------------------------------------------------------
;; Add interactively
;;-----------------------------------------------------------------------------
(defun git-add-interactively()
  "A friendly replacement git add -i, using ediff"
  (interactive)                         ; haha
  (git--require-buffer-in-git)
  (git--diff
   (git--if-in-status-mode
       (git--status-view-select-filename)
     buffer-file-name )
   ":"                                  ; index
   ;; before ediff
   (lambda()
     (with-current-buffer ediff-buffer-B
       (setq buffer-read-only nil)
       (set-buffer-modified-p nil))
     (message "Your changes to the second buffer will be added to the index"))
   ;; after ediff
   (lambda()
     (let ((file-buf ediff-buffer-A)
           (index-buf ediff-buffer-B))
       (if (not (buffer-modified-p index-buf))
           (message "No changes to the index")
         (with-current-buffer file-buf
           (let ((filename (file-relative-name buffer-file-name)))
             (when (y-or-n-p "Add changes to the git index? ")
               ;; insert index-buf as blob object,  get its hash
               (let ((new-content-hash
                      (git--trim-string (git--exec-pipe
                                         "hash-object"
                                         index-buf
                                         "-t" "blob" "-w" "--stdin")))
                     (fileinfo (car-safe (git--status-index filename))))
                 ;; update index with the new object
                 (git--exec-string
                  "update-index" "--cacheinfo"
                  (git--fileinfo->perm fileinfo)
                  new-content-hash
                  (git--get-relative-to-top buffer-file-name))))))))
   )))

;;-----------------------------------------------------------------------------
;;grep
;;-----------------------------------------------------------------------------

(defvar git--grep-history nil "History for git-grep")
(defun git-grep (args)
  "Runs git grep with the given ARGS (a string) and output to a buffer"
  (interactive
   (let ((default (find-tag-default)))  ; often, user looks up an identifier
     (list (read-string (format "git grep %s>> "
                                (if default (format "(default %s) " default)
                                  ""))
                        nil 'git--grep-history default))))

  (require 'grep)
  (let ((default-directory (git--get-top-dir default-directory))
        (git-grep-setup-hook (lambda() (setenv "GIT_PAGER" "")))
        (compilation-buffer-name-function
         (lambda (&rest ignore) "*git grep*")))
    (add-hook 'grep-setup-hook git-grep-setup-hook)
    (unwind-protect
         (grep (concat "git grep -n " args))
      (remove-hook 'grep-setup-hook git-grep-setup-hook))))

(provide 'git-emacs)
