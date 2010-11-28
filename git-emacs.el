;;; git-emacs (v.1.4.3) : yet another git emacs mode for newbies
;;
;; Copyright (C) 2008  TSKim (tsgatesv@gmail.com)
;;
;; v.1.4.3 Modified by ovy            @ 20 September 2009
;; v.1.4   Modified by ovy            @ 22 March 2009
;; v.1.3   Modified by Con Digitalpit @ 29 March 2008
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
;; Some related packages were used directly or as inspiration:
;;   - psvn.el (Stefan Reichoer)
;;   - vc-git.el (Alexandre Julliard)
;;   - git.el (Alexandre Julliard)
;;   - ido.el (Kim F. Storm)
;;   - ...
;;
;;; Installation
;;
;; First, make sure that vc-git.el is in your load path. Emacs 23 ships it by
;; default, for older versions you can get it from git distributions prior
;; to 1.6.x.
;; 
;; 1) Load at startup (simplest)
;;
;; (add-to-list 'load-path "~/.emacs.d/git-emacs")  ; or your installation path
;; (require 'git-emacs)
;;
;; 2) Autoload (slimmer statup footprint, will activate when visiting a git
;;   file or running some top-level functions)
;;
;; (add-to-list 'load-path "~/.emacs.d/git-emacs") ; or your installation path
;; (fmakunbound 'git-status)   ; Possibly remove Debian's autoloaded version
;; (require 'git-emacs-autoloads)
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


(require 'cl)                           ; common lisp
(require 'ediff)                        ; we use this a lot
(require 'vc)                           ; vc
(require 'vc-git)                       ; vc-git advises
(add-to-list 'vc-handled-backends 'git) ; set backend management
(require 'time-stamp)                   ; today

(require 'git-global-keys)              ; global keyboard mappings
(require 'git-emacs-autoloads)          ; the minimal autoloads

;; Autoloaded submodules, those not declared in git-emacs-autoloads

(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git" t)

(autoload 'git--update-state-mark "git-modeline"
  "Update modeline of git buffers with a customizable state marker" t)
(autoload 'git--update-all-state-marks "git-modeline"
  "Update the modelines of all git buffers" t)

(autoload 'git-log-files "git-log"
  "Launch the git log view for the current file or the selected files in git-status-mode" t)
(autoload 'git-log "git-log"
  "Launch the git log view for whole repository" t)
(autoload 'git-log-other "git-log"
  "Launch the git log view for an arbitrary branch or tag" t)



;;-----------------------------------------------------------------------------
;; Global preferences.
;;-----------------------------------------------------------------------------
(defgroup git-emacs nil
  "A user interface for the git versioning system."
  :group 'tools)

(defcustom git--use-ido t
  "Whether to use ido completion for git-emacs prompts."
  :type '(boolean)
  :group 'git-emacs)

(defvar git--completing-read
  (if git--use-ido
      (progn
        (require 'ido)
        ;; But this is not enough apparently. We need to strobe ido-mode
        ;; for ido-completing-read to work. Ugh.
        (unless ido-mode (ido-mode t) (ido-mode -1))
        #'ido-completing-read)
    #'completing-read)
  "Function to use for git-emacs minibuffer prompts with choices. It should have
the signature of `completing-read'.")

(defgroup git-emacs-faces nil
  "Face customizations for git-emacs."
  :group 'git-emacs)

;; Face definition macros. Used mostly in git-status.
(defmacro git--face (name fore1 prop1 fore2 prop2)
  `(defface ,(intern (concat "git--" (symbol-name name) "-face"))
     '((((class color) (background light)) (:foreground ,fore1 ,@prop1))
       (((class color) (background dark))  (:foreground ,fore2 ,@prop2)))
    ,(concat "git " (symbol-name name) " face in status buffer mode")
    :group 'git-emacs-faces))

(git--face bold       "tomato" (:bold t) "tomato"  (:bold t))

(defsubst git--bold-face (str) (propertize str 'face 'git--bold-face))

(defconst git--msg-critical (propertize "Critical Error" 'face 'git--bold-face))
(defconst git--msg-failed (propertize "Failed" 'face 'git--bold-face))

;;-----------------------------------------------------------------------------
;; Internal variables.
;;-----------------------------------------------------------------------------


(defvar git--executable "git" "Main git executable")
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
;; Low-level execution functions.
;;-----------------------------------------------------------------------------

(defsubst git--exec (cmd outbuf infile &rest args)
  "Low level function for calling git. CMD is the main git subcommand, args
are the remaining args. See `call-process' for the meaning of OUTBUF and
INFILE. Reeturns git's exit code."
  (apply #'call-process git--executable infile outbuf nil (cons cmd args)))

(defun git--exec-pipe (cmd input &rest args)
  "Execute 'git cmd args', piping INPUT (which can be a buffer or string).
Return result string."
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
              (message "")              ;hide write to file message
              (apply #'git--exec cmd t tmp args))
          (delete-file tmp))))))

(defsubst git--exec-buffer (cmd &rest args)
  "Execute 'git' within the buffer. Return the exit code."
  (apply #'git--exec cmd t nil args))

(defsubst git--exec-string-no-error (cmd &rest args)
  "Execute 'git CMD ARGS' and return result string, which may be
a failure message."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'git--exec-buffer cmd args))))

(defun git--trim-string (str)
  "Trim the spaces / newlines from the beginning and end of STR."
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
        (error "%s" (git--trim-string (buffer-string)))))))


;; This is nasty: the git devs changed the meaning of "git status" in git
;; 1.7, but commit --dry-run is not available in older git. Thanks much
;; guys -- I get to learn to learn how to do horrible hacks like this.
;; Hopefully the messages aren't translated or something.
(defun git--commit-dryrun-compat(outbuf &rest args)
  "Executes commit --dry-run with the specified args, falls back to the
older git status if that command is not present. If OUTBUF is not nil, puts
the standard output there. Returns the git return code."
  ;; Going forward, this will simply succeed.
  (let ((rc (apply #'git--exec "commit" (list outbuf nil) nil
                   "--dry-run" args)))
    (when (eq rc 129)
      ;; gotta distinguish between bad args or no --dry-run.
      (let ((has-dry-run
             (string-match
              "--dry-run"
              (git--exec-string-no-error "commit" "--no-such-arg-show-help"))))
        (unless has-dry-run
          (setq rc (apply #'git--exec "status" outbuf nil args)))))
    rc))


;;-----------------------------------------------------------------------------
;; utilities
;;-----------------------------------------------------------------------------

(defun git--trim-tail (str)
  "Trim spaces / newlines from the end of STR."
  (let ((end (- (length str) 1)))
    (while (and (< 0 end)
                (memq (aref str end) '(? ?\n)))
      (decf end))
    (substring str 0 (+ end 1))))

(defun git--pop-to-buffer(buffer)
  "Wrapper around `pop-to-buffer', stores window configuration
from before `pop-to-buffer' call for later restoration. Every
buffer popped to with this function will have a local
`kill-buffer-hook' to restore previous window configuration."
  (let ((git--saved-window-configuration (current-window-configuration))
        (popped-buffer (pop-to-buffer buffer)))
    (with-current-buffer popped-buffer
      (add-hook 'kill-buffer-hook
                (lexical-let ((saved-config git--saved-window-configuration))
                  #'(lambda()
                      (set-window-configuration saved-config)
                      )) nil t)) ;; !append local
    popped-buffer))

(defsubst git--join (seq &optional sep)
  "Joins the strings in SEQ with the SEP (defaults to blank)."
  (mapconcat #'identity seq (if sep sep " ")))

(defsubst git--concat-path-only (path added)
  "Concatenate the path with proper separator"
  (concat (file-name-as-directory path) added))

(defsubst git--concat-path (path added)
  (expand-file-name (git--concat-path-only path added)))

(defsubst git--expand-to-repository-dir (dir)
  (git--concat-path dir git--repository-dir))

(defun git--quit-buffer ()
  "Kill the current buffer, calls `kill-buffer'. Every
  buffer popped with `git--pop-to-buffer' will restore previous
  window-configuration when killed."
  (interactive)
  (kill-buffer))

(defsubst git--rev-parse (&rest args)
  "Execute 'git rev-parse ARGS', return result string."
  (apply #'git--exec-string "rev-parse" args))

(defmacro git-in-lowest-existing-dir (dir &rest BODY)
  "Runs \"BODY\" with `default-directory' set to the nearest
existing parent of DIR; useful because git directories can come
and go when switching parents, and Emacs refuses to execute
commands in a non-existing directory.  If DIR is nil, defaults to
`default-directory'. Only use this for commands that don't take
filenames, such as git branch, because relative filenames may
become invalid when we walk up -- in that case, it's better to
let the user see the invalid directory error."
   `(let ((default-directory (file-name-as-directory
                              (if ,dir (expand-file-name ,dir)
                                default-directory))))
      ;; The default-directory might be gone if a branch was switched! Walk up.
      (let (parent)
        (while (not (or (file-exists-p default-directory)
                        (eq (setq parent (file-name-as-directory
                                          (expand-file-name "..")))
                            default-directory)))
          (setq default-directory parent)))
      ,@BODY))

(defmacro git--if-in-status-mode (THEN &rest ELSE)
  "Macro that evaluates THEN when in git-status-mode, ELSE otherwise. Used to
grab status-mode filelists without the compiler complaining about the
autoloading which we know has already happened."
  `(if (eq major-mode 'git-status-mode)
       (progn (eval-when-compile (require 'git-status)) ,THEN)
     ,@ELSE))

(defun git--get-top-dir (&optional dir)
  "Get the top-level git directory above DIR. If nil, use default-directory."
  (git-in-lowest-existing-dir dir
   (let ((cdup (git--rev-parse "--show-cdup")))
     (git--concat-path default-directory (car (split-string cdup "\n"))))))

(defsubst git--interpret-to-state-symbol (stat)
  "Interpret a one-letter git state string to our state symbols."
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
  "If there are modified buffers which visit files in the given
REPO-OR-FILELIST,ask to save them. If REPO-OR-FILELIST is nil,
look for buffers in the current git repo. Returns the number of
buffers saved."
  (let ((buffers (git--find-buffers repo-or-filelist  #'buffer-modified-p)))
    (map-y-or-n-p
     (lambda(buffer) (format "Save %s? " (buffer-name buffer)))
     (lambda(buffer) (with-current-buffer buffer (save-buffer)))
     buffers
     '("buffer" "buffers" "save"))))

(defcustom git-working-dir-change-behaviour
  'git-ask-for-all-saved
  "Controls the buffer-refreshing behaviour after a git working dir change
 (e.g. branch switch), when there are buffers visiting files that
 have been modified by the change."
  :type '(radio (const :tag "Ask about refreshing all saved buffers"
                       git-ask-for-all-saved)
                (const :tag "Refresh all saved buffers"
                       git-refresh-all-saved)
                (const :tag "Don't refresh" nil))
  :group 'git-emacs)

(defun git-after-working-dir-change (&optional repo-or-filelist)
  "This function should be called after a change to the git working dir.
If there are buffers visiting files in the given REPO-OR-FILELIST that
have changed (buffer modtime != file modtime), ask the user whether to refresh
those buffers. Updates the state mark of all the buffers not reverted
\(since revert updates the ones reverted anyway). If currently in status
buffer, refreshes the status buffer. Warns the user if there are any buffers
visiting files that no longer exist."
  (interactive)
  (let ((buffers (git--find-buffers
                  repo-or-filelist
                  #'(lambda(buffer)
                      (not (verify-visited-file-modtime buffer)))))
        (buffers-that-exist nil) (buffers-that-dont-exist nil)
        (num-buffers-refreshed 0))
    (dolist (buffer buffers)
      (if (file-exists-p (buffer-file-name buffer))
          (push buffer buffers-that-exist)
        (push buffer buffers-that-dont-exist)))
    ;; Revert buffers that exist
    (unwind-protect
        (let ((buffers-not-reverted (copy-sequence buffers-that-exist))
              buffers-that-exist-unsaved buffers-that-exist-saved)
          (flet ((buffer-refresh-func (buffer)
                  (with-current-buffer buffer (revert-buffer t t))
                  ;; A hash table is probably not worth it here.
                  (setq buffers-not-reverted
                        (delq buffer buffers-not-reverted))
                  (incf num-buffers-refreshed)))
            ;; Filter buffers by their saved status.
            (dolist (buffer buffers-that-exist)
              (if (buffer-modified-p buffer)
                  (push buffer buffers-that-exist-unsaved)
                (push buffer buffers-that-exist-saved)))
            ;; Do the state mark update if the user quits the revert prompts.
            ;; Or, on all unsaved buffers.
            (unwind-protect
                (case git-working-dir-change-behaviour
                  ('git-ask-for-all-saved
                   (map-y-or-n-p
                    (lambda(buffer) (format "%s has changed, refresh buffer? "
                                            (buffer-name buffer)))
                    #'buffer-refresh-func
                    buffers-that-exist-saved
                    '("buffer" "buffers" "refresh")))
                  ('git-refresh-all-saved
                   (mapc #'buffer-refresh-func buffers-that-exist-saved)))
              (when buffers-not-reverted
                (git--update-all-state-marks (mapcar #'buffer-file-name
                                                   buffers-not-reverted)))))
          ;; Refresh status buffer
          (git--if-in-status-mode (git--status-view-refresh)))
      ;; But display the [important] files don't exist / buffers refreshed
      ;; warnings on failure / quit
      (let ((submessages
             (append
              (when (> num-buffers-refreshed 0)
                (list (format "%d buffers refreshed" num-buffers-refreshed)))
              (when buffers-that-dont-exist
                (list
                 (format "some open files no longer exist: %s"
                   (git--join
                    (let ((numfiles (length buffers-that-dont-exist)))
                      (if (> numfiles 2)
                       (list (buffer-name (first buffers-that-dont-exist))
                             (format "%d others" (- numfiles 1)))
                       (mapcar #'buffer-name buffers-that-dont-exist)))
                    ", ")))))))
        (when submessages
          (message "Note: %s" (git--join submessages "; ")))))))

;; This belongs later with all the commit functions, but the compiler complains
;; in git-log if we don't define it before its first use.
(defun git-commit-all (&optional amend)
  "Runs git commit -a, prompting for a commit message. With a prefix argument,
runs git commit --amend -a, alowing an update of the previous commit."
  (interactive "P")
  (git-commit amend t))

;;-----------------------------------------------------------------------------
;; fileinfo structure
;;-----------------------------------------------------------------------------

;; ewoc file info structure for each list element
(defstruct (git--fileinfo
            (:copier nil)
            (:constructor git--create-fileinfo
                          (name type &optional sha1 perm marked
                                               stat size refresh))
            (:conc-name git--fileinfo->))
  marked   ;; t/nil
  expanded ;; t/nil
  refresh  ;; t/nil
  stat     ;; 'unknown/'modified/'uptodate/'staged  etc.
  type     ;; 'blob/'tree/'commit (i.e. submodule)
  name     ;; filename
  size     ;; size
  perm     ;; permission
  sha1)    ;; sha1

(defsubst git--fileinfo-is-dir (info)
  "Returns true if a file info is directory-like (expandable, sorted first)"
  (not (eq 'blob (git--fileinfo->type info))))

(defsubst git--fileinfo-dir (info)
  "Returns the directory component of a fileinfo's path. If the fileinfo is
directory-like, the directory is the path itself, with a slash appended."
  (if (git--fileinfo-is-dir info)
      (file-name-as-directory (git--fileinfo->name info))
    (or (file-name-directory (git--fileinfo->name info)) "")))

(defun git--fileinfo-lessp (info1 info2)
  "Sorting rule for git--fileinfos, such that the ordering in git-status is
right. The rule is rather complicated, but it basically results in a
properly expanded tree."
  (let ((info1-dir (git--fileinfo-dir info1))
        (info2-dir (git--fileinfo-dir info2)))
    (let ((cmp (compare-strings info1-dir 0 nil info2-dir 0 nil)))
      (if (not (eq t cmp))
          ;; A file in a subdirectory should always come before a file
          ;; in the parent directory.
          (if (< cmp 0)
              ;; info1-dir < info2-dir
              (if (eq (length info1-dir) (- -1 cmp))
                  ;; info1-dir is a subdir of info2-dir. less == false,
                  ;; unless info1 is a directory itself.
                  (git--fileinfo-is-dir info1)
                t)
            ;; info1-dir > info2-dir
            (if (eq (length info2-dir) (- cmp 1))
                ;; info2-dir is a subdir of info1-dir. less == true, unless
                ;; info2 is a directory itself.
                (not (git--fileinfo-is-dir info2))
              nil))
        ;; same directory, straight-up comparison
        (string< (git--fileinfo->name info1)
                 (git--fileinfo->name info2))))))


;;-----------------------------------------------------------------------------
;; git commands
;;-----------------------------------------------------------------------------

(defun git--init (dir)
  "Execute 'git init' in DIR (current dir, if unspecified)."
  (with-temp-buffer
    (when dir (cd dir))
    (git--exec-string "init")))

(defun git--checkout (&rest args)
  "Execute 'git checkout ARGS' and return resulting string."
  (apply #'git--exec-string "checkout" args))

(defun git--clone-sentinel (proc stat)
  "Process sentinel for 'git clone' processes."
  (let ((cmd (git--join (process-command proc))))
    (cond ((string= stat "finished\n")
           (message "%s : %s" (git--bold-face "Cloned") cmd))
          ;; TODO : popup or state
          ((string= stat "killed\n")
           (message "%s : %s" git--msg-failed cmd))
          (t
           (message "%s : %s" git--msg-critical cmd)))))

(defun git--clone (&rest args)
  "Execute 'git clone ARGS', with a process sentinel showing status."
  (let ((proc (apply #'start-process "git-clone" nil "git" "clone" args)))
    (set-process-sentinel proc 'git--clone-sentinel)
    (message "%s : %s"
             (git--bold-face "Run")
             (git--join (process-command proc)))))

(defun git--commit (msg &rest args)
  "Execute 'git commit ARGS',  pipe the MSG string"
  (git--trim-string
   (apply #'git--exec-pipe "commit" msg "-F" "-" args)))

(defun git--reset (&rest args)
  "Execute 'git reset ARGS', return the result string."
  (apply #'git--exec-string "reset" args))

(defun git--config (&rest args)
  "Execute 'git config ARGS', return the result string. Return empty
if git config fails (behaviour if unconfigured as of version 1.7.1)."
  (condition-case nil
      (git--trim-string (apply #'git--exec-string "config" args))
    (error "")))

(defun git--add (files)
  "Execute 'git add' with the sequence FILES."
  (when (stringp files) (setq files (list files)))
  (apply #'git--exec-string "add" files))

(defun git--mv (src dst)
  "Execute git mv for SRC and DST files."
  (git--exec-string "mv" src dst))

(defun git--tag (&rest args)
  "Execute 'git tag ARGS', return the result as string."
  (apply #'git--exec-string "tag" args))

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
  "Execute 'git diff --raw' with 'args' and 'files' at current buffer. This
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

(defun git--symbolic-ref (arg)
  "Execute 'git symbolic-ref ARG' and return the sha1 string, or nil if the
arg is not a symbolic ref."
  (let ((commit (git--exec-string-no-error "symbolic-ref" "-q" arg)))
    (when  (> (length commit) 0)
      (car (split-string commit"\n")))))

(defun git--current-branch ()
  "Execute 'git symbolic-ref'HEAD' and return branch name string. Returns
nil if there is no current branch."
  (let ((branch (git-in-lowest-existing-dir nil (git--symbolic-ref "HEAD"))))
    (when branch
      (if (string-match "^refs/heads/" branch)
          (substring branch (match-end 0))
        branch))))

(defsubst git--log (&rest args)
  "Execute 'git log ARGS' and return result string"
  (apply #'git--exec-string "log" "-z" args))

(defsubst git--last-log-short ()
  "Get the last log, as one line: <short_commit> <short_msg>"
  (git--trim-string (git--log "--max-count=1" "--pretty=oneline"
                              "--abbrev-commit")))

(defsubst git--last-log-message ()
  "Return the last commit message, as a possibly multiline string, with an "
  "ending newline,"
  (git--log "--max-count=1" "--pretty=format:%s%n%b"))

(defun git--get-relative-to-top(filename)
  (file-relative-name filename
                      (git--get-top-dir (file-name-directory filename))))

(defun git--get-top-dir-or-prompt (prompt &optional dir)
  "Returns the top-level git directory above DIR (or default-directory). Prompts
the user with PROMPT if not a git repository, or if DIR is t."
  (or (unless (eq dir t) (ignore-errors (git--get-top-dir dir)))
      (git--get-top-dir (read-directory-name prompt (unless (eq dir t) dir)))))

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
          (let* ((stat (match-string 1))
                 (name (match-string 2))
                 (file-name (directory-file-name name)))
            ;; Files listed with e.g "-o" might be directories
            (push (git--create-fileinfo file-name
                                        (if (equal name file-name) 'blob
                                          'tree)
                                        nil nil nil
                                        (git--interpret-to-state-symbol stat))
                  fileinfo)))))
    (sort fileinfo 'git--fileinfo-lessp)))

(defsubst git--to-type-sym (type)
  "Convert a string type from git to 'blob, 'tree or 'commit (i.e. submodule)"
  (cond ((string= type "blob") 'blob)
        ((string= type "tree") 'tree)
        ((string= type "commit") 'commit)
        (t (error "strange type : %s" type))))

(defun git--ls-tree (&rest args)
  "Execute 'git ls-tree ARGS', return a list of git--fileinfo structs."

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
  "Run 'git merge ARGS', output the return message to the user."
  (message "%s" (git--trim-string (apply #'git--exec-string "merge" args))))

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


;;-----------------------------------------------------------------------------
;; git application
;;-----------------------------------------------------------------------------

(defsubst git--managed-on-git? ()
  "Returns true if we're in a git repository."
  (not (string-match "fatal: Not a git repository"
                     (git--rev-parse "HEAD"))))

(defun git--status-file (file)
  "Return the git status of FILE, as a symbol."
  (let ((fileinfo (git--status-index file)))
    (unless fileinfo (setq fileinfo (git--ls-files file)))
    (when (= 1 (length fileinfo))
      (git--fileinfo->stat (car fileinfo)))))

(defun git--branch-list ()
  "Get branch list, in the order returned by 'git branch'. Returns a cons cell
\(list-of-branches . current-branch), where current-branch may be nil."

  (let ((branches) (current-branch)
        (regexp (concat " *\\([*]\\)? *" git--reg-branch "\n")))

    (with-temp-buffer
      (git-in-lowest-existing-dir
       nil
       (unless (eq 0 (git--exec-buffer "branch" "-l"))
         (error "%s" (git--trim-string (buffer-string)))))
      (goto-char (point-min))

      (while (re-search-forward regexp nil t)
        (let ((branch (match-string 2)))
          (unless (string= branch "(no branch)")
            (push branch branches))
          (when (and (not current-branch) (string= "*" (match-string 1)))
            (setq current-branch branch)))))
    (cons (nreverse branches) current-branch)))

(defun git--cat-file (buffer-name &rest args)
  "Execute 'git cat-file ARGS' and return a new buffer named BUFFER-NAME
with the file content"
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)

      ;; auto mode, for highlighting
      (let ((buffer-file-name buffer-name)) (set-auto-mode))
      (apply #'git--exec-buffer "cat-file" args)

      ;; set buffer readonly & quit
      (setq buffer-read-only t)

      ;; Failed?
      (goto-char (point-min))
      (when (looking-at "^\\([Ff]atal\\|[Ff]ailed\\|[Ee]rror\\):")
        (let ((msg (buffer-string)))
          (kill-buffer nil)
          (setq buffer nil)
          (error "%s" (git--trim-tail msg)))))
    buffer))

(defun git--select-branch (&rest excepts)
  "Prompts the user for a branch name. Offers all the branches for completion,
except EXCEPTS. Returns the user's selection."
  (let ((branches (car (git--branch-list))))
    (git--select-from-user
     "Select branch: "
     (delq nil (mapcar (lambda (b) (unless (member b excepts) b))
                       branches)))))

;; ================================================================================
;; I will revise this code laster this week
;; ================================================================================
(defun git-pull-ff-only ()
  "Interactive git pull. Prompts user for a remote branch, and pulls from it.
  This command will fail if we can not do a ff-only pull from the remote branch."
  (interactive)
  (let ((remote (git--select-remote 
                 (concat "Select remote for pull (local branch:" 
                         (git--current-branch) 
                         "): "))))
    (message (git--pull-ff-only remote))))

;; XXX. should this be implemented list this way? umm..
(defsubst git--select-remote (prompt &rest excepts)
  "Select remote branch interactively."
  (let ((remotes (git--symbolic-commits '("remotes"))))
    (git--select-from-user prompt
                           (delq nil (mapcar (lambda (b) (unless (member b excepts) b))
                                             remotes)))))

(defun git--pull-ff-only (remote)
  "Pull from remote into current branch, but only on a fast-forward pull."
  (let ((split-remote (split-string remote "/"))
        (parse-success-string (lambda (resultstring) ;; Parses success string
                                (let ((lines (split-string resultstring "\n")))
                                  (if (string-equal (nth 2 lines) "Already up-to-date.")
                                      "Already up-to-date."
                                    (let ((revision-change 
                                           (split-string (cadr (split-string (nth 2 lines) )) 
                                                         "\\.\\.")))
                                      (concat "Pulled revisions from " 
                                              (car revision-change) 
                                              " to " 
                                              (cadr revision-change) 
                                              "." (nth (- (length lines) 2) lines))))))))
    (let ((remote-name (car split-remote))
          (remote-branch (car (cdr split-remote))))
      (condition-case err
          (progn
            (funcall parse-success-string 
                     (git--exec-string "pull" "--ff-only" remote-name (concat remote-branch))))
        (error (error-message-string err))))))

(defun git--split-porcelain (resultstring)
  (mapcar (lambda (s) (split-string s "\t")) (split-string resultstring "\n")))

(defun git--n-n-th (i j arr)
  "Given a 2-d list, access the i,j 'th element"
  (nth j (nth i arr)))

(defun git--actual-push (remote-name remote-branch)
  (let ((actual-run-output 
         (git--split-porcelain 
          (git--exec-string "push" "--porcelain" 
                            remote-name 
                            (concat (git--current-branch) ":" remote-branch)))))
    (message (concat "Pushed changes " 
                     (git--n-n-th 1 2 dry-run-output) 
                     " to remote " 
                     remote-name 
                     "/" 
                     remote-branch))))

(defun git--push-ff-only (remote)
  "Pushes from current branch into remote, fast-forward only."
  (let ((split-remote (split-string remote "/")))
    (let ((dry-run-output (git--split-porcelain 
                           (git--exec-string "push" "--dry-run" "--porcelain" 
                                             (car split-remote) 
                                             (concat (git--current-branch) ":" (cadr split-remote))))))
      (let ((newbranch (string-equal (git--n-n-th 1 2 dry-run-output) "[new branch]"))
            (change-diff (git--n-n-th 1 2 dry-run-output))
            (up-to-date (string-equal (git--n-n-th 1 0 dry-run-output) "Everything up-to-date")))
        (cond (newbranch (if (y-or-n-p (concat "Pushing will create branch " 
                                               (cadr split-remote) 
                                               " in remote. Continue? "))
                             (git--actual-push (car split-remote) (cadr split-remote))
                           (message "Did not push.")))
              (up-to-date (message "Remote branch is up to date."))
              ((not newbranch) (git--actual-push (car split-remote) (cadr split-remote)))
              (t (message "Did not push.")))))))
;; --------------------------------------------------------------------------------

(defun git--symbolic-commits (&optional reftypes)
  "Find symbolic names referring to commits, using 'git for-each-ref'.
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

(defun git--select-revision (prompt &optional prepend-choices excepts)
  "Offer the user a list of human-readable revisions to choose from. By default,
it shows branches, tags and remotes; additional choices can be
specified as a list. If EXCEPTS is specified, don't show any choices `equal'
to those in the list, unless they were specified in PREPEND-CHOICES explicitly."
  (git--select-from-user prompt
                         (append
                          prepend-choices
                          (delq nil
                                (mapcar #'(lambda (revision)
                                            (unless (member revision excepts)
                                              revision))
                                        (git--symbolic-commits)))
                          git--revision-history)))

(defun git--maybe-ask-and-commit(after-func)
  "Helper for functions that switch trees. If there are pending
changes, asks the user whether they want to commit, then pops up
a commit buffer (and returns). Once the user has committed (or
immediately, if they chose not to or there are no pending
changes), AFTER-FUNC is called, which should do the tree
switching along with any confirmations. The return value is either the
pending commit buffer or nil if the buffer wasn't needed."
  ;; git status -a tells us if there's anything to commit
  (if (and (eq 0 (git--commit-dryrun-compat nil "-a"))
           (y-or-n-p "Commit your pending changes first? (if not, they will be merged into the new tree) "))
      (with-current-buffer (git-commit-all)
        (add-hook 'git--commit-after-hook after-func t t) ; append, local
        (current-buffer))
    (funcall after-func)
    nil))


;;-----------------------------------------------------------------------------
;; vc-git integration
;;-----------------------------------------------------------------------------

(defun git--update-modeline ()
  "Update the current's buffer modeline state display."
  ;; mark depending on the fileinfo state
  (when (and buffer-file-name (git--in-vc-mode?))
    (git--update-state-mark
     (git--status-file (file-relative-name buffer-file-name)))))

(defalias 'git-history 'git-log)

(defadvice vc-after-save (after git--vc-git-after-save activate)
  "vc-after-save advice for updating status"

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
;; Low-level commit support.
;;-----------------------------------------------------------------------------

(defun git--config-get-author ()
  "Returns the user's name, either from git or from emacs's config."

  (let ((config-user-name (git--config "user.name")))
      (or (and (not (string= config-user-name "")) config-user-name)
          (and (fboundp 'user-full-name) (user-full-name))
          (and (boundp  'user-full-name) user-full-name))))

(defun git--config-get-email ()
  "Returns the user's email, either from git or from emacs's config."
  (let ((config-user-email (git--config "user.email")))
    (or (and (not (string= config-user-email "")) config-user-email)
        (and (fboundp 'user-mail-address) (user-mail-address))
        (and (boundp 'user-mail-address) user-mail-address))))

(defun git--insert-log-header-info (amend)
  "Insert the log header to the buffer. If AMEND, grab the info from the last
commit, like git commit --amend will do once we commit."

  ;; TODO: we should make git always use this info, even if its config
  ;; has changed. Otherwise this is misleading.
  (insert git--log-header-line  "\n"
          "# Branch : " (or (git--current-branch) "<none>")     "\n")
  (if amend
      (insert (git--log "--max-count=1"
                        (concat "--pretty=format:"
                                "# Author : %an%n"
                                "# Email  : %ae%n"
                                "# Date   : %ci%n"
                                "# Amend  : %h%n")))
    (insert
     "# Author : " (git--config-get-author)  "\n"
     "# Email  : " (git--config-get-email)   "\n"
     "# Date   : " (git--today)              "\n")))

;; Internal variables for commit
(defvar git--commit-after-hook nil
  "Hooks to run after comitting (and killing) the commit buffer.")
(defvar git--commit-args nil
  "Args to be passed to the current git commit once done editing.")
(make-variable-buffer-local 'git--commit-args)
(defvar git--commit-targets nil
  "Records the targets parameter of `git-commit' in the current commit buffer")
(make-variable-buffer-local 'git--commit-targets)
(defvar git--commit-last-diff-file-buffer nil
  "Stores last diff buffer launched from a commit buffer.")
(make-variable-buffer-local 'git--commit-targets)
(defvar git--commit-amend nil
  "Records whether the current commit buffer is for a commit --amend")
(make-variable-buffer-local 'git--commit-amend)

(defun git--commit-buffer ()
  "Called when the user commits, in the commit buffer (C-cC-c).
Trim the buffer log, commit runs any after-commit functions."
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
        (message "%s" (apply #'git--commit
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
    (kill-buffer git--commit-log-buffer)

    ;; hooks (e.g. switch branch)
    (run-hooks 'local-git--commit-after-hook 'git--commit-after-hook)))

;;-----------------------------------------------------------------------------
;; Merge support.
;;-----------------------------------------------------------------------------

(defun git--resolve-fill-buffer (template side)
  "Make a buffer showing a SIDE ('local or 'remote) of the conflict
buffer TEMPLATE. Returns the buffer."

  (let* ((filename (file-relative-name (buffer-file-name template)))
         (buffer-name (format "*%s*: %s"
                              (capitalize (symbol-name side)) filename))
         (buffer (get-buffer-create buffer-name))
         (msg "Malformed conflict marker"))

    (with-current-buffer buffer
      (buffer-disable-undo)
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
            ('local (delete-region conflict-sep conflict-end))
            ('remote (delete-region conflict-begin conflict-sep))
            (t (error "Side must be one of 'local or 'remote"))))))
    buffer))

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

(defun git--merge-ask ()
  "Prompts the user for a branch or tag to merge. Returns t if the merge
succeeded, nil if it had conflicts, raises an error if the merge failed for
unknown reasons."
  (let ((branch (git--select-revision "Merge: " nil
                                      (list (git--current-branch))))
        (merge-success t))
    (condition-case err
        (git--merge branch)
      (error
       (setq merge-success nil)
       (let ((err-msg (error-message-string err)))
         ;; Often because of conflicts
         (if (string-match "^CONFLICT" err-msg)
               (message "%s" err-msg)
           ;; otherwise, reraise
           (signal (car err) (cdr err))))))
    merge-success))

(defun git-merge ()
  "Prompts the user for a branch or tag to merge. On success, asks for
buffer revert. On conflicts, pulls up a status buffer"
  (interactive)
  (if (git--merge-ask) (git-after-working-dir-change)
    (git-status ".")))

(defun git--resolve-merge-buffer (&optional success-callback)
  "Implementation of resolving a conflicted buffer, which must be current.
If SUCCESS-CALLBACk is specified, it will be called if the merge is successful
and the user accepts the result."
  (interactive)

  (let* ((result-buffer (current-buffer))
         (filename (file-relative-name buffer-file-name))
         (our-buffer (git--resolve-fill-buffer result-buffer 'local))
         (their-buffer (git--resolve-fill-buffer result-buffer 'remote))
         (base-buffer (git--resolve-fill-base))
         (config (current-window-configuration))
         (ediff-default-variant 'combined)
         (ediff-combination-pattern '("<<<<<<< Local" A "=======" B
                                      ">>>>>>> Remote")))
    ;; Set the major mode of all the buffers based on the current buffer
    (let ((default-major-mode nil))
      (mapc #'set-buffer-major-mode
            (delq nil (list our-buffer their-buffer base-buffer))))

    ;; set merge buffer
    (set-buffer (if base-buffer
                    (ediff-merge-buffers-with-ancestor
                     our-buffer their-buffer base-buffer)
                  (ediff-merge-buffers our-buffer their-buffer)))

    (add-hook
     'ediff-quit-hook
     (lexical-let ((saved-config config)
                   (saved-result-buffer result-buffer)
                   (saved-base-buffer base-buffer)
                   (saved-success-callback success-callback))
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
               (set-window-configuration saved-config))))
     nil t)                             ; hook is prepend, local
    ;; Add the -after function after ediff does its thing.
    (add-hook 'ediff-quit-hook
              (lexical-let ((saved-success-callback success-callback))
                #'(lambda () (git--resolve-merge-after saved-success-callback)))
                t t)

    (message "Please resolve conflicts now, exit ediff when done")))

(defun git--resolve-has-merge-markers ()
  "Returns non-nil if the current buffer has any merge markers, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^<<<<<<< " nil t)))

(defun git--resolve-merge-after (&optional success-callback)
  "Function called after a git-resolve-merge. Called with the
merged buffer current. Checks for remaining merge markers, if
none ask the user whether to accept the merge results"
  (if (git--resolve-has-merge-markers)
      (message "Conflicts remain; resolve manually or hit undo to restore the buffer")
    (if (y-or-n-p "Conflicts resolved, save merge result? ")
        (progn
          (save-buffer)
          (git--add (file-relative-name buffer-file-name))
          (git--update-modeline)
          (when success-callback (funcall success-callback)))
      (message "You can hit undo once to restore the buffer")
      )))

(defun git-merge-next-action ()
  "Auto-pilot function to guide the user through a git merge. If none in
progress, prompts for a revision to merge. If there are unresolved conflicts,
prompts for resolving the next one. If all conflicts have been resolved, pulls
up a commit buffer. The function continues with the above logic until either
the user quits or the merge is successfully committed."
  (interactive)
  ;; First, check for any unmerged files. We must go to the top every time,
  ;; because the default-dir might change on us.
  (let* ((default-directory (git--get-top-dir))
         (unmerged-files (git--ls-unmerged)))
    (if unmerged-files
        (progn
          (switch-to-buffer
           (let ((resolve-next-file
                  (git--select-from-user
                   "Resolve next conflict: "
                   (delq nil (mapcar
                              #'(lambda (stage-and-fi)
                                  (when (eq 2 (car stage-and-fi))
                                    (file-relative-name (git--fileinfo->name
                                                         (cdr stage-and-fi)))))
                              unmerged-files)))))
             ;; find-file-noselect will nicely prompt about refreshing
             (find-file-noselect resolve-next-file)))
          (redisplay t)                 ;force refontification, show buffer
          (if (git--resolve-has-merge-markers)
              ;; tell resolve-merge to schedule us if the resolution succeeded.
              ;; Avoid running another ediff from the ediff hook, though
              (git--resolve-merge-buffer
               #'(lambda()
                   (run-at-time "0 sec" nil 'git-merge-next-action)))
            (if (y-or-n-p "Conflicts seem resolved, save merge result to git? ")
                (progn
                  (save-buffer)
                  (git--add (file-relative-name buffer-file-name))
                  (git--update-modeline)))
            (git-merge-next-action)))

      ;; else branch, no unmerged files remaining
      ;; Perhaps we should commit (staged files only!)
      (if (file-exists-p (expand-file-name ".git/MERGE_HEAD"))
          (git-commit nil nil "Merge finished ")    ; And we're done!
        ;; else branch, no sign of a merge. Ask for another.
        (if (git--merge-ask)
            (progn
              (git-after-working-dir-change)
              (message "Merge successful"))
          (sit-for 1.5)            ; for the user to digest message
          (message "")             ; fixes odd v23 interaction with ediff's msgs
          (git-merge-next-action)) ; start processing conflicts
))))


(defun git-resolve-merge ()
  "Resolve git merge conflicts in the current buffer with ediff."
  (interactive)
  (git--resolve-merge-buffer))


;;-----------------------------------------------------------------------------
;; High-level commit functions.
;;-----------------------------------------------------------------------------

(defconst git--commit-status-font-lock-keywords
  '(("^#\t\\([^:]+\\): +[^ ]+$"
     (1 'git--bold-face))
    ("^# \\(Branch\\|Author\\|Email\\|Date\\|Amend\\) +:" (1 'git--bold-face))
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
    ; Compute rev1, rev2 inputs to diff--many
    (let ((diff-from (if git--commit-amend "HEAD^1" "HEAD"))
          (diff-to nil))
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

(defun git-commit (&optional amend targets prepend-status-msg)
  "Does git commit with a temporary prompt buffer. If AMEND or a prefix argument
is specified, does git commit --amend. TARGETS can be nil (commit staged files)
, t (commit all) or a list of files. If PREPEND-STATUS-MSG is specified,
adds it in front of the help message (Type C-c C-c ...).

Returns the buffer."

  (interactive "P")
  ;; Don't save anything on commit-index
  (when targets (git--maybe-ask-save (if (eq t targets) nil targets)))

  (let ((cur-pos nil)
        (buffer (get-buffer-create git--commit-log-buffer))
        (current-dir default-directory))
    (with-current-buffer buffer
      ;; Tell git--commit-buffer what to do
      (setq git--commit-targets targets
            git--commit-args (append
                              (when amend '("--amend"))
                              (cond ((eq nil targets) '())
                                    ((eq t targets) '("-a"))
                                    ((listp targets) (cons "--" targets))
                                    (t (error "Invalid targets: %S" targets))))
            git--commit-amend amend)

      (local-set-key "\C-c\C-c" 'git--commit-buffer)
      (local-set-key "\C-c\C-q" 'git--quit-buffer)
      (erase-buffer)
      (flyspell-mode 0)               ; disable for the text we insert
      (cd current-dir)                ; if we reused the buffer

      (set (make-local-variable 'font-lock-defaults)
           (list 'git--commit-status-font-lock-keywords t))
      (when global-font-lock-mode (font-lock-mode t))
      ;; insert info
      (git--insert-log-header-info amend)

      ;; real log space
      (insert (propertize git--log-sep-line 'face 'git--log-line-face) "\n")

      (insert "\n")
      ;; If amend, insert last commit msg.
      (when amend (insert (git--last-log-message)))
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

      ;;git commit --dryrun or git status -- give same args as to commit
      (insert git--log-sep-line "\n")
      (git--please-wait "Reading git status"
        (unless (eq 0 (apply #'git--commit-dryrun-compat t git--commit-args))
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
                      (when git--commit-last-diff-file-buffer
                          (kill-buffer git--commit-last-diff-file-buffer))))
                t t)                    ; append, local

      ;; Set cursor to message area
      (goto-char cur-pos)

      (when git--log-flyspell-mode (flyspell-mode t))

      ;; comment hook
      (run-hooks 'git-comment-hook)

      (message "%sType 'C-c C-c' to commit, 'C-c C-q' to cancel"
               (or prepend-status-msg "")))
    (git--pop-to-buffer buffer)
    buffer))

(defun git-commit-file (&optional amend)
  "Runs git commit with the file in the current buffer, or with the selected
files in git-status. Only changes to those files will be committed. If the
current buffer is not in git, it will get added automatically. If AMEND, or
a prefix argument, is specified, does a commit --amend."
  (interactive "P")
  ;; Here and in other functions below we rely on the fact that git-status has
  ;; surely been loaded if the current major mode is git-status.
  (git--if-in-status-mode
      (git-commit amend (git--status-view-marked-or-file))
    (unless buffer-file-name (error "Not a file buffer"))
    (unless (git--in-vc-mode?)
      (git--add (file-relative-name buffer-file-name))
      (vc-find-file-hook))
    (git-commit amend (list (file-relative-name buffer-file-name)))))

(defun git-init (dir)
  "Initialize a git repository."

  (interactive "DGit Repository: ")
  (message "%s" (git--trim-string (git--init dir)))
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
         (t (error "%s is not supported " ext)))))

    (let ((dir (file-name-sans-extension
                (file-name-sans-extension file))))
      ;; move to git repository
      (cd dir)

      ;; init
      (git-init dir)
      (git--add ".")
      (git-commit-all))))

;;-----------------------------------------------------------------------------
;; Miscellaneous high-level functions.
;;-----------------------------------------------------------------------------

(defun git-clone (dir)
  "Clone a repository (prompts for the URL) into the local directory DIR (
prompts if unspecified)."
  
  (interactive "DLocal destination directory: ")

  (let ((repository
         (git--select-from-user "Clone repository: "
                                git--repository-bookmarks
                                git--repository-history
                                "")))
    (with-temp-buffer
      (cd dir)
      (git--clone repository))))

(defun git-reset (commit)
  "Reset the current branch to the given COMMIT (prompts the user if not
specified). Prompts the user whether to reset --hard."

  (interactive
      ;; We might be operating with a detached HEAD.
   (let ((current-branch (git--current-branch)))
     (list (git--select-revision
            (format "Reset %s to: "
                    (if current-branch
                        (concat "branch " (git--bold-face current-branch))
                      "current state"))
            '("HEAD")))))               ; frequent usage
  (let ((saved-head (git--abbrev-commit "HEAD" 10))
        (reset-hard (y-or-n-p
                     "Reset working directory as well (reset --hard)? ")))
    (apply #'git--reset (delq nil
                              (list (when reset-hard "--hard") commit "--")))
    (unwind-protect
        (if reset-hard (git-after-working-dir-change)
          (git--update-all-state-marks)
          (git--if-in-status-mode (git--status-view-refresh)))
      ;; I nearly lost my HEAD to an accidental reset --hard
      (message "You can recover the old HEAD as %s" saved-head))))

(defun git-revert (commit)
  "Revert a commit, prompting the user if unspecified. Does not commit the
revert operation, instead popping up a commit buffer."
  (interactive
   ;; TODO: for this to really make sense we need some SHA1 completion
   (list (git--select-revision "Revert commit: ")))
  ;; Save MERGE_MSG, in case the user is doing multiple reverts.
  (let ((merge-msg-file (expand-file-name ".git/MERGE_MSG" (git--get-top-dir)))
        (actual-revert #'(lambda()
                           (git--trim-string
                            (git--exec-string "revert" "-n" commit)))))
    (let ((output
           (if (file-exists-p merge-msg-file)
               (with-temp-buffer
                 (buffer-disable-undo)
                 (insert-file-contents-literally merge-msg-file)
                 (delete-file merge-msg-file)
                 ;; append the new MERGE_MSG
                 (unwind-protect (funcall actual-revert)
                   (when (file-exists-p merge-msg-file)
                     (goto-char (point-max))
                     (insert "\n")
                     (insert-file-contents-literally merge-msg-file))
                   (write-region (point-min) (point-max) merge-msg-file nil
                                 'dont-notify-user)))
             (funcall actual-revert))))    ; no existing msg, just run it
    (unwind-protect
        (git-after-working-dir-change)
      (git-commit nil nil output)))))

(defcustom gitk-program "gitk"
  "The command used to launch gitk."
  :type '(string)
  :group 'git-emacs)

(defun gitk ()
  "Launch gitk in the current directory."
  (interactive)
  (start-process "gitk" nil gitk-program))

(defun git-checkout (&optional commit confirm-prompt after-checkout-func
                     &rest after-checkout-func-args)
  "Checkout a commit, fully. When COMMIT is nil, shows tags and branches
for selection. May pop up a commit buffer to commit pending
changes; in this case, the function is asynchronous and returns
that commit buffer. If CONFIRM-PROMPT is non-nil, ask for
confirmation, replacing %s in CONFIRM-PROMPT with the commit.
If AFTER-CHECKOUT-FUNC is specified, runs it with AFTER-CHECKOUT-FUNC-ARGS,
once the checkout is complete."
  (interactive)
  (git--maybe-ask-save)
  (git--maybe-ask-and-commit
   (lexical-let ((commit (or commit (git--select-revision "Checkout: ")))
                 (confirm-prompt confirm-prompt)
                 (repo-dir default-directory)
                 (after-checkout-func after-checkout-func)
                 (after-checkout-func-args after-checkout-func-args))
     (lambda()
       (when (or (not confirm-prompt)
                 (y-or-n-p (format confirm-prompt commit)))
         (git--checkout commit)
         (when after-checkout-func
           (apply after-checkout-func after-checkout-func-args))
         (git-after-working-dir-change repo-dir))))))


(defalias 'git-create-branch 'git-checkout-to-new-branch)

(defun git-checkout-to-new-branch (&optional branch suggested-start
                                             after-checkout-func
                                   &rest after-checkout-func-args)
  "Checkout new branch. Unless BRANCH is specified, prompts for the new branch
name. Also prompts for the starting point of the new branch, with either
SUGGESTED-START (if specified) or the current branch as the first suggestion.
If AFTER-CHECKOUT-FUNC is specified, runs it with AFTER-CHECKOUT-FUNC-ARGS,
once the checkout is complete."
  (interactive)
  (git--maybe-ask-save)
  (git--maybe-ask-and-commit
   (lexical-let ((branch branch)
                 (repo-dir default-directory)
                 (after-checkout-func after-checkout-func)
                 (after-checkout-func-args after-checkout-func-args))
     (lambda()
       (let* ((branch (or branch (read-from-minibuffer "Create new branch: ")))
              (suggested-start (or suggested-start
                                   (git--current-branch)))
              ;; put current branch as the first option to be based on.
              (commit (git--select-revision
                       (format "Create %s based on: "
                               (git--bold-face branch))
                       ;; Move the suggestion from the end to the beginning.
                       (when suggested-start (list suggested-start))
                       (when suggested-start (list suggested-start)))))
         (git--checkout "-b" branch commit)
         (when after-checkout-func
           (apply after-checkout-func after-checkout-func-args))
         (git-after-working-dir-change repo-dir))))))

(defun git-delete-branch (&optional branch)
  "Delete BRANCH, prompt the user if unspecified."
  (interactive)
  (unless branch (setq branch (git--select-branch "master")))

  (let ((saved-head (git--abbrev-commit branch 10))) ; safer abbrev
    (git--branch "-D" branch)              ; we've asked already
    (message "You can recover the deleted branch %s as %s"
             (git--bold-face branch) saved-head)))

(defun git-delete-tag (tag)
  "Delete TAG, prompt the user if unspecified."
  (interactive
   (list (git--select-from-user "Delete tag: " (git--tag-list))))
  (let ((saved-tag-target
         (ignore-errors
           (git--trim-string (git--exec-string "rev-parse" "--short" tag)))))
    (git--tag "-d" tag)
    (message "%s tag '%s'; you can recover it as %s"
             (git--bold-face "Deleted") tag saved-tag-target)))

(defvar git--cmd-history nil
  "History variable for commands ran through git-cmd")

(defun git-cmd (str)
  "Runs an arbitrary git command, prompting in interactive mode. Displays
the result as a message."
  (interactive (list (read-string "git >> " nil 'git--cmd-history)))
  ;; The command may or may not affect the working dir or work with files we
  ;; are editing.
  (git--maybe-ask-save)
  (message "%s" (git--trim-tail
                 (apply #'git--exec-string (split-string str))))
  ; let the user digest message, then check for modified files.
  (sit-for 2)
  (git-after-working-dir-change))



(defun git-config-init ()
  "Set initial configuration, it query the logined user information"

  (interactive)

  (let ((name (git--trim-string (git--config "user.name")))
        (email (git--trim-string (git--config "user.email"))))

    (when (or (null name) (string= "" name))
      (setq name (read-from-minibuffer "User Name : "
                                       (git--config-get-author)))

      (git--config "--global" "user.name" name))
    
    (when (or (null email) (string= "" email))
      (setq email (read-from-minibuffer "User Email : "
                                        (git--config-get-email)))

      (git--config "--global" "user.email" email))

    (message "Set user.name(%s) and user.email(%s)" name email)))

(defun git-ignore (file-or-glob)
  "Add FILE-OR-GLOB to .gitignore. Prompts the user if interactive."
  (interactive "sIgnore file or glob: ")
  (with-temp-buffer
    (insert file-or-glob "\n")
    (append-to-file (point-min) (point-max)
                    (expand-file-name ".gitignore" (git--get-top-dir)))))
  
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
         (git-after-working-dir-change repo-dir))))))

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
  (interactive)
  ;; TODO: ido doesn't give good feedback on globs
  (let* ((choices (mapcar #'(lambda (fi)
                              (git--fileinfo->name fi))
                          (git--ls-files "--others" "--exclude-standard")))
         (default-choice  ;; the current file, if it's a choice
           (when buffer-file-name
             (let ((current-file (file-relative-name buffer-file-name)))
               (when (member current-file choices)
                 (message "default: %S" current-file) current-file))))
         (files (git--select-from-user "Add new files (glob) >> " 
                                       choices nil default-choice))
         (matched-files (mapcar #'(lambda (fi) (git--fileinfo->name fi))
                                (git--ls-files "--others" "--exclude-standard"
                                               "--" files))))
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
;; Low-level diff functions
;;-----------------------------------------------------------------------------

(defun git--diff (file rev &optional before-ediff-hook after-ediff-hook)
  "Starts an ediff session between the FILE and its specified revision.
REVISION should not include the filename, e.g. \"HEAD:\". If
BEFORE-EDIFF-HOOK is specified, it is executed as an ediff setup
hook. If AFTER-EDIFF-HOOK is specified, it is executed as an
ediff quit hook. Both hooks run in the ediff context, i.e. with
valid ediff-buffer-A and B variables, among others. If the
versions are identical, error out without executing either type
of hook."
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
    (when (eq 0 (compare-buffer-substrings buf1 nil nil buf2 nil nil))
      (kill-buffer buf2)
      (error "No differences vs. %s"
             (or (car-safe (split-string rev ":" t)) "index")))

    (set-buffer
     (ediff-buffers buf1 buf2
                    (append
                     (when before-ediff-hook (list before-ediff-hook)))))

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
    (git--pop-to-buffer buffer)))

;;-----------------------------------------------------------------------------
;; branch mode
;;-----------------------------------------------------------------------------

(defvar git--branch-mode-map nil)
(defvar git--branch-mode-hook nil
  "Hooks to run after branch mode initialization, after the branches have
been displayed.")

(let ((map (make-keymap)))
  (suppress-keymap map)

  (define-key map "q"     'git--quit-buffer)
  (define-key map "n"     'next-line)
  (define-key map "p"     'previous-line)

  (define-key map "d"     'git--branch-mode-delete)
  (define-key map (kbd "<delete>") 'git--branch-mode-delete)
  (define-key map "c"     'git--branch-mode-create)
  (define-key map "s"     'git--branch-mode-switch)
  (define-key map "\C-m"  'git--branch-mode-switch)
  (define-key map "g"     'git--branch-mode-refresh)

  (setq git--branch-mode-map map))

(easy-menu-define gitemacs-menu-branch git--branch-mode-map
  "Git-Branch"
  `("Git-Branch"
    ["Next Line" next-line t]
    ["Previous Line" previous-line t]
    ["Switch To Branch" git--branch-mode-switch t]
    ["Create New Branch..." git--branch-mode-create t]
    ["Delete Branch" git--branch-mode-delete t]
    ["Refresh" git--branch-mode-refresh t]
    ["Quit" git--quit-buffer t]))


(defun git--branch-mode ()
  "Initialize branch mode buffer."

  (kill-all-local-variables)
  (buffer-disable-undo)

  ;; set major mode
  (setq mode-name "git branch")
  (setq major-mode 'git-branch-mode)

  (use-local-map git--branch-mode-map)

  (setq buffer-read-only t)
  ;; annotations can be long, so request horizontal scrolling
  (setq truncate-lines t)
  ;; beginning of buffer name position
  (setq goal-column 3)
  (setq header-line-format
        (concat (make-string (+ (scroll-bar-columns 'left)
                                (fringe-columns 'left))
                             ? )
                "Branch List"))
  (when (require 'hl-line nil t) (hl-line-mode)))


(defun git--branch-mode-annotate-changes-pending (branch-list)
  "Branch annotator function: display \"changes pending\" next
to the current branch, if applicable. Enabled by default."
  (let ((current-branch (git--current-branch)))
    (when (and (member current-branch branch-list)
               ;; "git commit --dryrun -a" returns ok if pending changes.
               (eq 0 (git--commit-dryrun-compat nil "-a")))
      (list (cons current-branch (git--bold-face "changes pending"))))))


(defvar git-branch-annotator-functions
  (list 'git--branch-mode-annotate-changes-pending)
  "List of functions that provide branch annotations in `git-branch' buffers.
Each function is called with a list of branches and the git-branch-mode
buffer current (i.e., default-directory is in git), and should return an
association list of (branch-name . annotation) for the branches that can
be annotated. The annotation is a string that will be displayed next
to the branch. See `git--branch-mode-annotate-changes-pending' for an
example.")
;; (makunbound 'git-branch-annotator-functions)   <-- C-x C-e for testing

(defvar git--branch-mode-branch-list nil
  "Stores the list of branches, in the order displayed in the branch-mode
buffer")
(make-variable-buffer-local 'git--branch-mode-branch-list)

(defun git--branch-mode-refresh (&optional position-on-current)
  "Displays or refreshes the branch list in the branch-mode buffer.
If POSITION-ON-CURRENT is true, put the cursor on the current branch; otherwise
preserve the cursor position."
  (interactive)

  ;; Set the working dir to the top-level dir. Otherwise it might have gone
  ;; away from under us.
  (setq default-directory (git--get-top-dir))

  ;; find annotations
  (let* ((branch-list-and-current (git--branch-list))
         (branch-list (car branch-list-and-current))
         (current-branch (cdr branch-list-and-current))
         (branch-annotations (make-hash-table :test 'equal))
         (buffer-read-only nil)
         ;; Complex decision on where to leave the cursor
         (line-number-after
          ;; Select either the current branch or the previously selected one.
          ;; Note that the buffer-pos and even line no could change wildly.
          (+ 1 (or (position (if position-on-current current-branch
                               (git-branch-mode-selected t))
                             branch-list :test 'equal)
                   ;; If not found, just stay on the same line or the first.
                   (- (line-number-at-pos) 1)))))
    (erase-buffer)
    (dolist (annotator git-branch-annotator-functions)
      (dolist (branch-annotation
               (ignore-errors (funcall annotator branch-list)))
        (let* ((branch (car branch-annotation))
               (annotation (cdr branch-annotation))
               (lookup (gethash branch branch-annotations)))
            (puthash branch (add-to-list 'lookup annotation t)
                     branch-annotations))))

    ;; Display annotations either 3 chars after the longest branch name,
    ;; or at half window width, whichever is smaller.
    (let ((annotation-column
           (when (> (hash-table-count branch-annotations) 0)
             (min (/ (window-width) 2)
                  (+ goal-column 3
                     (reduce #'max (mapcar #'length branch-list)))))))
      (dolist (branch branch-list)
        (let ((annotations (gethash branch branch-annotations))
              (branch-is-current (string= current-branch branch)))
          (insert (format "%2s "
                          (if branch-is-current (git--bold-face "*") " ")))
          (insert branch)
          (when annotations
            (insert (make-string
                     (max 1 (- annotation-column (current-column)))
                     ?\s))
            (insert "-")
            (dolist (annotation annotations)
              (insert " ")
              (insert annotation)))
          (insert "\n"))))

    ;; reposition
    (ignore-errors (goto-line line-number-after))
    (if (> (line-number-at-pos) (length branch-list))
        (previous-line)
      (next-line 0))   ;; side-effect: moves to goal-column :)

    (setq git--branch-mode-branch-list branch-list)))


(defun git-branch ()
  "Pop up a git branch mode buffer and return it."

  (interactive)

  (let ((buffer (get-buffer-create "*git-branch*"))
        (windows (current-window-configuration))
        ;; Subtle: a pre-existing *git-branch* buffer might have the wrong dir
        (directory default-directory))
    (with-current-buffer buffer
      (setq default-directory directory)
      ;; set branch mode
      (git--branch-mode)
      (git--branch-mode-refresh t)
      )
    ;; pop up
    (git--pop-to-buffer buffer)
    (fit-window-to-buffer)
    (run-hooks 'git--branch-mode-hook)
    buffer))

(defcustom git-branch-buffer-closes-after-action 'on-branch-switch
  "Whether to close the `git-branch' buffer after a user action. May be
t, nil, or the symbol on-branch-switch, which causes the buffer to be closed
only after branch switch actions."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "On branch switch" on-branch-switch))
  :group 'git-emacs)


(defun git--branch-mode-after-action (was-branch-switch &optional buffer)
  "Refreshes or closes the `git-branch' buffer after a user action, depending
on `git-branch-buffer-closes-after-action'. WAS-BRANCH-SWITCH is a boolean,
self-explanatory. BUFFER is the git-branch buffer, which may not be current
anymore; if unspecified, we operate on the current buffer."
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (if (or (eq t git-branch-buffer-closes-after-action)
              (and was-branch-switch
                   (eq 'on-branch-switch
                       git-branch-buffer-closes-after-action)))
          (kill-buffer buffer)
        ;; Just refresh. If this was a branch switch is seems OK to move
        ;; the cursor to the new branch.
        (with-current-buffer buffer
          (git--branch-mode-refresh was-branch-switch))))))


;; Branch mode actions
(defun git-branch-mode-selected (&optional noerror)
  "Returns the branch on the current line in a `git-branch'
buffer. Depending on NOERROR, it errors out or returns nil if
none. It's safe to call this outside a `git-branch' buffer: it will
behave as if there is no curent branch (error or nil)."
  (or (nth (- (line-number-at-pos) 1) git--branch-mode-branch-list)
      (unless noerror (error "No branch selected"))))


(defun git--branch-mode-delete ()
  "Delete the branch that point is on."
  (interactive)
  (let ((branch (git-branch-mode-selected)))
    (when (y-or-n-p (format "%s the branch %s? "
                            (git--bold-face "Delete")
                            (git--bold-face branch)))
      (git-delete-branch branch)
      (git--branch-mode-after-action nil))))


(defun git--branch-mode-switch ()
  "Switch to the branch that point is on."
  (interactive)
  (let ((branch (git-branch-mode-selected))
        (current-branch (git--current-branch)))
    (when (string= branch current-branch)
      (error "Already on branch %s" branch))
    (git-checkout branch
                  (format "Switch from branch %s to %s? "
                          (git--bold-face current-branch)
                          (git--bold-face "%s")) ; checkout replaces this
                  #'git--branch-mode-after-action
                  t (current-buffer))
     ))


(defun git--branch-mode-create ()
  "Create a branch, prompting for the name and the base branch."
  (interactive)
  (git-checkout-to-new-branch nil (git-branch-mode-selected t)
                              #'git--branch-mode-after-action
                              t (current-buffer)))

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
(defvar git-baseline-alist '()
  "Association list of (REPOSITORY-DIR . BASELINE-COMMIT). Both
REPOSITORY-DIR and BASELINE-COMMIT are strings. The BASELINE-COMMIT determines
what to diff against, in this repository, when git-diff-baseline is used; it
is either a string or a function, see also `git-baseline-candidates'.")
;; (makunbound 'git-baseline-commit)  ;;eval to clear variable

(defvar git-baseline-candidates '("git-svn" "origin")
  "List of strings and functions that might be good choices for baseline commits
in git (i.e., the state of an upstream repository). A string item is interpreted
as a git symbolic ref, and it will be used if it exists in the current
repository. A function item will be called with default-directory set to the
root repository, and if it completes successfully and returns non-nil its
string result will be used as the baseline. Normally, the first string or
function that matches will be used, but you can select a baseline manually
by calling `git-baseline' interactively.")

(defun git-baseline (&optional always-prompt-user)
  "Select the baseline commit used in `git-diff-baseline' and friends, for the
current repository. Tries to find the repo in `git-baseline-alist'; if not
found, tries all of `git-baseline-candidates'. If the above attempts fail, or
ALWAYS-PROMPT-USER is specified, or it was called interactively, prompts the
user for a baseline commit, saves it to `git-baseline-alist' and offers to save
that variable in .emacs.
  Returns either a string (git symbolic ref) or a function that returns one."
  (interactive '(t))
  ;; This function is a bit too long. Consider extracting parts that may
  ;; be useful elsewhere.
  (let* ((repo-dir
          ;; either the repo of the current buffer
          (git--get-top-dir-or-prompt "Set baseline for repository: "))
         ;; canonicalize, for storage / lookup
         (canonical-repo-dir
          (expand-file-name (file-name-as-directory repo-dir)))
         (previous-baseline-assoc
          (assoc canonical-repo-dir git-baseline-alist)))
    ;; found among previous associations?
    (if (and (not always-prompt-user) previous-baseline-assoc)
        (if (functionp (cdr previous-baseline-assoc))
            (funcall (cdr previous-baseline-assoc))
          (cdr previous-baseline-assoc))
      ;; prompt for new one, possibly a function
      (let ((default-directory canonical-repo-dir)
            candidate-strings candidate-function-alist)
        (catch 'found-one
          (dolist (candidate git-baseline-candidates)
            (cond ((stringp candidate)
                   (when (ignore-errors (git--rev-parse candidate))
                     (if (not always-prompt-user)
                         (throw 'found-one candidate)
                       (add-to-list 'candidate-strings candidate t))))
                  ((functionp candidate)
                   (let ((result (ignore-errors (funcall candidate))))
                     (when result
                       (if (not always-prompt-user)
                           (throw 'found-one result)
                         (let ((readable-form (format "(%S)" candidate)))
                           (add-to-list 'candidate-strings readable-form t)
                           (add-to-list 'candidate-function-alist
                                        (cons readable-form candidate)))))))
                  (t (error "Invalid git-baseline-candidate: %S" candidate))))
          ;; Maybe prompt user
          (let* ((new-baseline-str
                  ;; Show the viable candidates first, but allow arbitrary revs
                  (git--select-revision "Select baseline commit: "
                                        candidate-strings candidate-strings))
                 (new-baseline (or (cdr-safe
                                    (assoc new-baseline-str
                                           candidate-function-alist))
                               new-baseline-str)))
            ;; store in variable
            (if previous-baseline-assoc
                (setcdr previous-baseline-assoc new-baseline)
              (add-to-list 'git-baseline-alist
                           (cons canonical-repo-dir new-baseline))
              ;; ... which we possibly save in .emacs
              (when (y-or-n-p "Save for future sessions? ")
                (customize-save-variable 'git-baseline-alist
                                         git-baseline-alist)))
            (if (functionp new-baseline) (funcall new-baseline)
              new-baseline)))))))

(defun git-diff-baseline()
  "Diff current buffer against a selectable \"baseline\" commit"
  (interactive)
  (git--require-buffer-in-git)
  (git--diff (git--if-in-status-mode
              (git--status-view-select-filename)
              buffer-file-name)
             (concat (git-baseline) ":")))

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
  (git--diff-many files (git-baseline)))

(defun git-diff-all-other (commit &optional files)
  (interactive
   (list (git--select-revision "Diff against commit: ")))
  (git--diff-many files commit))

;;-----------------------------------------------------------------------------
;; Add interactively
;;-----------------------------------------------------------------------------
(defun git-add-interactively()
  "A friendly replacement for git add -i, using ediff"
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
         (lambda (&rest ignore) "*git grep*"))
        (grep-use-null-device nil))
    (add-hook 'grep-setup-hook git-grep-setup-hook)
    (unwind-protect
         (grep (concat "git grep -n " args))
      (remove-hook 'grep-setup-hook git-grep-setup-hook))))


;;-----------------------------------------------------------------------------
;; Minimalistic stash support.
;;-----------------------------------------------------------------------------
(defconst git--stash-list-font-lock-keywords
  '(("^\\(stash@{\\([^}]*\\)}\\):"
     (1 font-lock-function-name-face prepend)
     (2 font-lock-variable-name-face prepend))
    ("^\\(Branch\\): \\([^(\n]+\\)\\( (\\(changes pending\\))\\| ([^\n]*\\)?\n"
     ;; Actually, too much decoration looks ugly.
     ;;(1 'git--bold-face prepend)
     ;;(2 font-lock-variable-name-face prepend)
     (4 'git--bold-face prepend)
     )))
;; (makunbound 'git--stash-list-font-lock-keywords)

(defun git--prepare-stash-list-buffer (buffer)
  "Prepares and pops the stash list buffer."
  (let ((directory default-directory))
    (with-current-buffer buffer
      (setq default-directory directory) ; in case it was different before
      (setq buffer-read-only t)
      (setq cursor-type nil)
      (buffer-disable-undo)
      (set (make-local-variable 'font-lock-defaults)
           (list 'git--stash-list-font-lock-keywords t))
      (when global-font-lock-mode (font-lock-mode t)))
    (git--pop-to-buffer buffer)))


(defvar git--stash-history nil "History for git-stash")
(defun git-stash (&optional cmd)
  "Simple interface to \"git stash\". Without args, pops up a list of the
available stashes and prompts for the stash command, with a reasonable
suggestion. If CMD is specified, just runs \"git stash cmd\", with the
usual pre / post work: ask for save, ask for refresh."
  (interactive)
  (git--maybe-ask-save)                 ; affects "changes pending"
  (unless cmd
   (let ((stash-list-str (git--exec-string "stash" "list"))
         (buffer (get-buffer-create "*git-stash*"))
         (changes-pending-point))
     (unwind-protect
         (let ((stashes-exist (> (length stash-list-str) 0)))
           (git--prepare-stash-list-buffer buffer)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert "Branch: " (or (git--current-branch) "<none>"))
             (setq changes-pending-point (point))
             (insert "\n\n")
             (if (not stashes-exist) (insert "(no stashes)")
               ;; Display arrow next to first stash.
               (set (make-local-variable 'overlay-arrow-position)
                    (point-marker))
               (insert stash-list-str)))
           (fit-window-to-buffer)
           (message "Checking tree status...")
           (redisplay t)                ; this might take a little bit
           (let ((changes-pending (eq 0 (git--commit-dryrun-compat nil "-a"))))
             (let ((inhibit-read-only t))
               (save-excursion
                 (goto-char changes-pending-point)
                 (insert (if changes-pending " (changes pending)"
                           " (no changes pending)"))))
             (let ((suggested-cmd
                    ;; "save" if there are pending changes, "pop" if there
                    ;; are no pending changes and stashes present, else nothing
                    (cond
                     (changes-pending "save") (stashes-exist "pop") (t ""))))
               (setq cmd (read-string "git stash >> "
                                      suggested-cmd 'git--stash-history)))))
         (kill-buffer buffer))))
  (message "%s" (git--trim-string (git--exec-string "stash" cmd)))
  (redisplay t)
  (sleep-for 1.5)                       ; let the user digest message
  (git-after-working-dir-change))


(provide 'git-emacs)