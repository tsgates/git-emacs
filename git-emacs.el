;;; git-emacs (v.1.0) : yet another git emacs mode for newbies
;;
;; Copyright (C) 2008  TSKim (tsgatesv@gmail.com)
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
;; (add-to-list 'load-path "/home/tsgates/Skills/git/git-emacs")
;; (require 'git-emacs)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; BUG FIXES
;; 2008.03.28 : git-diff just work on git root
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TODO : delete temporary log file
;; TODO : separate branch-mode & status-view-mode to other files
;; TODO : fetching 
;; TODO : regular exp selecting
;; TODO : enhance config! & tag!
;; TODO : save -> status-view update
;; TODO : git-log -> C-u command :=> cmd
;; TODO : remote branch list
;; TODO : status-mode function -> add status prefix
;; TODO : git set config
;; TODO : () -> fording/unfording for detail
;; TODO : show ignored files
;; TODO : locally flyspell
;; TODO : C-x v b -> branch
;; 

(eval-when-compile (require 'cl))

(require 'ewoc)                         ; view
(require 'vc-git)                       ; vc-git advises
(require 'ido)                          ; ido readline
(require 'electric)                     ; branch mode
(require 'time-stamp)                   ; today

(require 'git-blame)                    ; git blame
(require 'git-modeline)                 ; modeline dot

(defalias 'electric-pop-up-window 'Electric-pop-up-window)
(defalias 'electric-command-loop  'Electric-command-loop)

;;-----------------------------------------------------------------------------
;; faces
;;-----------------------------------------------------------------------------

(defgroup git nil
  "A user interface for the git versioning system."
  :group 'tools)

(defmacro git--face (name fore1 prop1 fore2 prop2)
  `(defface ,(intern (concat "git--" (symbol-name name) "-face"))
     '((((class color) (background light)) (:foreground ,fore1 ,@prop1))
       (((class color) (background dark))  (:foreground ,fore2 ,@prop2)))
    ,(concat "git " (symbol-name name) " face in status buffer mode")
    :group 'git))

(git--face mark       "red"    (:bold t) "tomato" (:bold t))
(git--face mark-tree  "blue"   (:bold t) "yellow" (:bold t))
(git--face mark-blob  "black"  () "white" ())
(git--face unknown    "black"  (:bold t) "white"  (:bold t))
(git--face ignored    "gray"   (:bold t) "gray"   (:bold t))
(git--face bold       "tomato" (:bold t) "tomato" (:bold t))
(git--face modified   "tomato" (:bold t) "tomato" (:bold t))
(git--face unmerged   "red"    (:bold t) "tomato" (:bold t))
(git--face uptodate   "gray"   (:bold t) "tomato" (:bold t))
(git--face added      "tomato" (:bold t) "tomato" (:bold t))
(git--face deleted    "red"    (:bold t) "tomato" (:bold t))
(git--face log-line   "gray"   (:bold t :italic t) "gray"(:bold t :italic t))

(defsubst git--bold-face (str) (propertize str 'face 'git--bold-face))

(defconst git--msg-error     (propertize "Error" 'face 'git--bold-face))
(defconst git--msg-critical  (propertize "Critical Error" 'face 'git--bold-face))
(defconst git--msg-failed    (propertize "Failed" 'face 'git--bold-face))

;;-----------------------------------------------------------------------------
;; internal variable
;;-----------------------------------------------------------------------------

(defvar git--commit-log-buffer "*git-log*")
(defvar git--log-flyspell-mode t "enable flyspell-mode when editing log")
(defvar git--repository-bookmarks
  '("~/Skills/git/checkouttest"
    "git://git.kernel.org/pub/scm/git/git.git"
    "git://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux-2.6.git"
    )
  "repository bookmarks")

(defvar git--repository-history nil)
(defvar git--status-mode-hook   nil)
(defvar git--status-mode-map    nil)
(defvar git--status-view        nil)

(defconst git--repository-dir ".git")
(defconst git--status-header-format "     %-2s %-10s %-5s %-5s %s")
(defconst git--status-line-column 30)

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
  "# ----------------------------- log -----------------------------")
(defconst git--log-file-line
  "# ---------------------------- files ----------------------------")
(defconst git--log-header-line
  "# ----------------------------- info ----------------------------")

(defsubst git--status-header ()
  (format (concat " " git--status-header-format)
          "M" "STATUS" "PERM" "SIZE" "FILE"))

;;-----------------------------------------------------------------------------
;; fork git process
;;-----------------------------------------------------------------------------

(defsubst git--exec (cmd outbuf inbuf &rest args)
  "Execute 'git' clumsily"

  (apply #'call-process
         (concat "git-" cmd)            ; cmd
         inbuf                          ; in buffer
         outbuf                         ; out buffer
         nil                            ; display
         args))                         ; args

(defun git--exec-pipe (cmd input &rest args)
  "Execute 'echo input | git cmd args' and return result string"

  (with-output-to-string
    (with-current-buffer standard-output
      (let ((tmp (make-temp-file "git-tmp")))
        (with-temp-buffer
          (insert input)
          (write-file tmp)

          ;; tricky hide write to file message
          (message ""))
        (apply #'git--exec cmd t tmp args)))))

(defsubst git--exec-buffer (cmd &rest args)
  "Execute 'git' within the buffer"
  
  (apply #'git--exec cmd t nil args))

(defsubst git--exec-string (cmd &rest args)
  "Execute 'git' and return result string"

  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'git--exec-buffer cmd args))))

(defsubst git--exec-cmd (cmd)
  "Execute 'git-cmd' with args which comes from user"

  (apply #'git--exec-string
         cmd
         (split-string (read-from-minibuffer (concat ">> git " cmd " ")))))

;;-----------------------------------------------------------------------------
;; utilities
;;-----------------------------------------------------------------------------

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
    (delete-window)
    (kill-buffer buffer)))

(defsubst git--select-from-user (prompt choices)
  "Select from choices"

  (ido-completing-read prompt choices))

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

  (let ((msg (git--exec-string "mv" src dst)))
    (unless (string-match "" (git--trim-string msg))
      (error msg))))

(defun git--rm (file)
  "Execute git-rm for file"

  (let ((msg (git--exec-string "rm" "--quiet" file)))
    (unless (string-match "" (git--trim-string msg))
      (error msg))))

(defun git--tag (&rest args)
  "Execute 'git-tag' with 'args' and return the result as string"

  (apply #'git--exec-string "tag" args))

(defalias 'git-snapshot 'git-tag)
(defun git-tag (name)
  "Make the new git same as 'git-snapshot"

  (interactive "sNew Tag Name >> ")

  (let ((msg (git--trim-string (git--tag name))))
    (if (string= "" msg)
        (message "Success to make %s" (git--bold-face name))
      (error msg))))

(defun git--tag-list ()
  "Get the tag list"

  (split-string (git--tag "-l") "\n" t))

(defsubst git--diff-index (&rest args)
  "Execute 'git-diff' with 'args' at current buffer"
  
  (apply #'git--exec-buffer "diff-index" "-z" "--full-index" args))

(defun git--status-index (&rest files)
  "Execute 'git-status-index' and return list of 'git--fileinfo'"

  ;; update fileinfo -> unmerged index
  (let ((fileinfo nil)
        (unmerged-info (make-hash-table :test 'equal))
        (regexp (git--build-reg git--reg-status  ; matched-1
                                git--reg-eof
                                git--reg-file))) ; matched-2

    (dolist (fi (git--ls-unmerged))
      (puthash (git--fileinfo->name fi)
               (git--fileinfo->stat fi)
               unmerged-info))

    (with-temp-buffer
      (apply #'git--diff-index "--name-status"  "HEAD" "--" files)

      (goto-char (point-min))

      (while (re-search-forward regexp nil t)
        (let ((stat (git--interprete-to-state-symbol (match-string 1)))
              (file (match-string 2)))

          ;; if unmerged file
          (when (gethash file unmerged-info) (setq stat 'unmerged))

          ;; assume all listed elements are 'blob
          (push (git--create-fileinfo file 'blob nil nil nil stat) fileinfo))))

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

(defun git--refresh-desc ()
  "Refresh the git-status-mode header description"
  
  (ewoc-set-hf git--status-view
               (concat (git--bold-face "Directory") " : " default-directory     "\n"
                       (git--bold-face "Branch   ") " : " (git--current-branch) "\n"
                       (git--bold-face "Last Log ") " : " (git--last-log-short) "\n")
               ""))

(defsubst git--rev-parse (&rest args)
  "Execute 'git-rev-parse' with args and return as string"

  (apply #'git--exec-string "rev-parse" args))

(defsubst git--get-top-dir (dir)
  "Get the top directory of the current git repository"
  
  (with-temp-buffer
    (when (stringp dir) (cd dir))

    (let ((cdup (git--rev-parse "--show-cdup")))
      (git--concat-path dir (car (split-string cdup "\n"))))))

(defun git--ls-unmerged ()
  "Get the list of 'git--fileinfo' of the unmerged files"
  
  (let (fileinfo)
    (with-temp-buffer
      (git--exec-buffer "ls-files" "-t" "-u" "-z")
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
                (file (match-string 4)))

            (unless (and fileinfo
                         (string= file (git--fileinfo->name (car (last fileinfo)))))
              (push (git--create-fileinfo file 'blob sha1 perm nil 'unmerged)
                    fileinfo))))))
    (sort fileinfo 'git--fileinfo-lessp)))

(defun git--ls-files (&rest args)
  "Execute 'git-ls-files' with 'args' and return the list of the 'git--fileinfo'"

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
                                        (case (string-to-char stat)
                                          (?H 'uptodate )
                                          (?M 'unmerged )
                                          (?R 'deleted  )
                                          (?C 'modified )
                                          (?K 'killed   )
                                          (?? 'unknown  )
                                          (t nil        )))
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

(defsubst git--status-buffer-name (dir)
  (format "*git-status on %s*" (expand-file-name dir)))

(defsubst git--create-status-buffer (dir)
  (let* ((status-buffer-name (git--status-buffer-name dir))
         (status-buffer (get-buffer status-buffer-name)))
    (or status-buffer (get-buffer-create status-buffer-name))))

(defsubst git--kill-status-buffer (dir)
  (kill-buffer (git--status-buffer-name dir)))

(defsubst git--revert (&rest args)
  (apply #'git--exec-string "revert" args))

(defun git--merge (&rest args)
  (apply #'git--exec-string "merge" args))

(defsubst git--branch (&rest args)
  (apply #'git--exec-string "branch" args))

;;-----------------------------------------------------------------------------
;; status miscellaneous
;;-----------------------------------------------------------------------------

(defsubst git--today ()
  (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S"))

(defsubst git--interprete-to-state-symbol (stat)
  "Interpret git state string to state symbol"

  (case (string-to-char stat)
    (?M 'modified )
    (?? 'unknown  )
    (?A 'added    )
    (?D 'deleted  )
    (?U 'unmerged )
    (?T 'modified )
    (t nil)))

(defsubst git--interprete-state-mode-color (stat)
  "Interpret git state symbol to mode line color"

  (case stat
    ('modified "tomato"      )
    ('unknown  "gray"        )
    ('added    "blue"        )
    ('deleted  "red"         )
    ('unmerged "purple"      )
    ('uptodate "GreenYellow" )
    (t "red")))

;;-----------------------------------------------------------------------------
;; status view & render
;;-----------------------------------------------------------------------------

;; status view on one node
;;            +-> perm     +-> name
;; M STATUS   PERM   SIZE  FILE
;; +-> mark          +-> size
;;   +-> stat
;;                   
;; * Modified 100644 4564 |test.c
;; * New      100644 4564  test.c

(defsubst git--status-node-mark (info)
  "Render status view node mark"

  (propertize (if (git--fileinfo->marked info) "*" " ")
              'face
              'git--mark-face))

(defsubst git--status-node-stat (info)
  "Render status view node state"
  
  (let ((stat (git--fileinfo->stat info)))
    (propertize (capitalize (symbol-name stat))
                'face
                (case stat
                  ('modified 'git--modified-face )
                  ('uptodate 'git--uptodate-face )
                  ('unknown  'git--unknown-face  )
                  ('added    'git--added-face    )
                  ('deleted  'git--deleted-face  )
                  ('unmerged 'git--unmerged-face )
                  (t nil)))))

(defsubst git--status-node-perm (info)
  "Render status view node permission"
  
  (or (git--fileinfo->perm info) "------"))

(defsubst git--status-node-size (info)
  "Render status view node size"

  (let ((size (git--fileinfo->size info)))
    (if (numberp size)
        (number-to-string size)
      "")))
      
(defsubst git--status-node-name (info)
  "Render status view node name"
  
  (let ((name (git--fileinfo->name info))
        (type (git--fileinfo->type info)))

    (setq name (replace-regexp-in-string "[^/]+/" "    " name))
    (propertize name 'face
                (case type
                  ('tree 'git--mark-tree-face)
                  ('blob 'git--mark-blob-face)
                  (t (error "Can't be!"))))))
                  
(defun git--render-file-status (info)
  "Render status view node, call in order
 mark       : 'git--status-node-mark
 state      : 'git--status-node-stat
 permission : 'git--status-node-perm
 size       : 'git--status-node-size
 name       : 'git--status-node-name"
  
  (insert (format git--status-header-format
                  (git--status-node-mark info)
                  (git--status-node-stat info)
                  (git--status-node-perm info)
                  (git--status-node-size info)
                  (git--status-node-name info))))

(defun git--status-mode ()
  "git-status mode for editing state-view for git"
  
  (kill-all-local-variables)
  (buffer-disable-undo)

  ;; set major mode
  (setq mode-name "git status")
  (setq major-mode 'git-status-mode)

  (use-local-map git--status-mode-map)

  (setq buffer-read-only t)
  (setq header-line-format (git--status-header))

  ;; create ewoc for current git-status buffer
  (set (make-local-variable 'git--status-view)
       (ewoc-create 'git--render-file-status "" ""))

  (set (make-local-variable 'revert-buffer-function)
       'git--status-mode-revert-buffer)

  (run-hooks 'git--status-mode-hook))

(defun git--status-mode-revert-buffer (ignore-auto noconfirm)
  "Revert buffer to refresh!"

  ;; TODO refresh status-mode-buffer
  (git--status-new)
  (git--status-view-first-line))

;;-----------------------------------------------------------------------------
;; fileinfo in view
;;-----------------------------------------------------------------------------

(defsubst git--build-reg (&rest args)
  (apply #'concat (add-to-list 'args "\0" t)))

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
  stat     ;; 'unknown/'modified/'uptodate
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

;;-----------------------------------------------------------------------------
;; git-status-view features
;;-----------------------------------------------------------------------------

(defsubst git--clear-status ()
  "Clear the git-status-view"

  (ewoc-filter git--status-view #'(lambda (info) nil))
  (ewoc-refresh git--status-view)
  (let ((buffer-read-only nil)) (erase-buffer)))

(defsubst git--status-tree () (git--ls-tree "HEAD"))

(defsubst git--status-map (node pred)
  "Iterating 'git--status-view' by using 'ewoc-next and return the next node.
The predicate function should get 'node and 'data arguments and it return 't or nil.
If predicate return nil continue to scan, otherwise stop and return the node"
  
  (let ((data nil)
        (cont t))

    (while (and node cont)
      (setq data (ewoc-data node))
      (setq cont (not (funcall pred node data)))
      (setq node (ewoc-next git--status-view node)))

    node))

;; TODO -> binary search
(defun git--status-view-dumb-update-element (fi)
  "Add update 'fi' to 'git--status-view' thoughtlessly!"
  
  (unless (git--status-map (ewoc-nth git--status-view 0)
                           #'(lambda (node data)
                               (when (git--fileinfo-lessp fi data)
                                 (ewoc-enter-before git--status-view node fi))))
    (ewoc-enter-last git--status-view fi)))

(defun git--status-view-update-state (fileinfo)
  "Update the state-view elements in fileinfo"

  (let ((hashed-info (make-hash-table :test 'equal :size (length fileinfo))))
    (dolist (fi fileinfo)
      (puthash (git--fileinfo->name fi) fi hashed-info))

    (ewoc-collect git--status-view
                  #'(lambda (node)
                      (let* ((name (git--fileinfo->name node))
                             (fi (gethash name hashed-info)))
                        (when fi
                          (setf (git--fileinfo->stat node)
                                (git--fileinfo->stat fi))
                          (remhash name hashed-info)))))

    (maphash #'(lambda (k v) (git--status-view-dumb-update-element v)) hashed-info)))

;; TODO : need refactoring
(defun git--status-view-update-expand-tree (fileinfo)
  "Expand the interesting tree nodes containing one of fileinfos"

  (let ((node (ewoc-nth git--status-view 0)))
    
    (dolist (fi fileinfo)
      (let* ((paths (split-string (git--fileinfo->name fi) "/"))
             (matched-name nil))

        (when (< 1 (length paths))

          (setq matched-name (car paths))
          (setq paths (cdr paths))

          (setq node (git--status-map node
                                      (lambda (cur-node data)
                                        (when (and (eq (git--fileinfo->type data) 'tree)
                                                   (string= (git--fileinfo->name data) matched-name))

                                          (git--expand-tree cur-node)
                                          (if paths
                                              (progn
                                                (setq matched-name (concat matched-name "/" (car paths)))
                                                (setq paths (cdr paths))
                                                nil)
                                            t))))))))))
                                          

(defun git--status-view-update ()
  "Friendly update view function"
  
  (let ((fileinfo (git--status-index)))
    (git--status-view-update-expand-tree fileinfo)
    (git--status-view-update-state fileinfo)))

(defun git--status-new ()
  "Create new status-view buffer in current buffer"

  (git--clear-status)
  (git--refresh-desc)

  ;; add new file infos
  (dolist (info (git--status-tree)) (ewoc-enter-last git--status-view info))

  ;; add modified/renamed etc file infos
  (git--status-view-update)

  ;; add unknown file
  (let ((fileinfo (git--ls-files "-o" "--exclude-per-directory=.gitignore"
                                 "--exclude-from=.git/info/exclude")))
    
    (git--status-view-update-expand-tree fileinfo)

    (let ((iter (ewoc-nth git--status-view 0)))
      (dolist (fi fileinfo)
        (setq iter (git--status-map iter (lambda (node data)
                                           (when (git--fileinfo-lessp fi data)
                                             (ewoc-enter-before git--status-view node fi))))))))
  (git--status-refresh))

(defsubst git--status-delete (node)
  
  (let ((buffer-read-only nil)) 
    (ewoc-delete git--status-view node)))

(defsubst git--status-refresh ()
  (let ((pos (point)))
    (ewoc-refresh git--status-view)
    (goto-char pos)))

(defun git--status-delete-afer-regex (node regex)
  (while node
    (let ((next-node (ewoc-next git--status-view node))
          (node-data (ewoc-data node)))

      (if (string-match regex (git--fileinfo->name node-data))
          (git--status-delete node)
        ;; finish if not matched
        (setq next-node nil))

      (setq node next-node)))
  (git--status-refresh))


;;-----------------------------------------------------------------------------
;; key/menu map
;;-----------------------------------------------------------------------------

(unless git--status-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)

    (define-key map "n" 'git--status-view-next-line)
    (define-key map "p" 'git--status-view-prev-line)
    (define-key map "N" 'git--status-view-next-meaningfull-line)
    (define-key map "P" 'git--status-view-prev-meaningfull-line)
    (define-key map "m" 'git--status-view-mark-and-next)
    (define-key map "u" 'git--status-view-unmark-and-next)
    (define-key map " " 'git--status-view-toggle-and-next)
    (define-key map "q" 'git--status-view-quit)
    (define-key map "<" 'git--status-view-first-line)
    (define-key map ">" 'git--status-view-last-line)

    (define-key map "e" 'git--status-view-expand-tree-toggle)
    (define-key map "v" 'git--status-view-view-file)
    (define-key map "o" 'git--status-view-open-file)
    (define-key map "=" 'git--status-view-diff-file)
    (define-key map "b" 'git--status-view-switch-branch)
    (define-key map "!" 'git--status-view-resolve-merge)
    (define-key map "." 'git--status-view-git-cmd)
    (define-key map "k" 'git--status-view-gitk)
    (define-key map "g" 'git--status-view-refresh)
    (define-key map "a" 'git--status-view-add)
    (define-key map "i" 'git--status-view-add-ignore)
    (define-key map "r" 'git--status-view-rename)
    (define-key map "?" 'git--status-view-blame)
    (define-key map "d" 'git--status-view-rm)
    (define-key map "*" 'git--status-view-mark-reg)
    (define-key map "s" 'git--status-view-summary)

    ;; ok for commiting
    (define-key map "c" 'git-commit-all)
    
    (define-key map "\C-m" 'git--status-view-do-propriate)

    (setq git--status-mode-map map)))

;;-----------------------------------------------------------------------------
;; status view tree expanding
;;-----------------------------------------------------------------------------

(defun git--expand-tree (node)
  "Expand 'node' in 'git--status-view', but node->type should be 'tree"

  (let* ((data (ewoc-data node))
         (name (git--fileinfo->name data))
         (type (git--fileinfo->type data))
         (fileinfo (git--ls-tree (git--fileinfo->sha1 data))))

    (unless (eq type 'tree) (error "type should be 'tree"))

    (unless (git--fileinfo->expanded data)

      (dolist (fi fileinfo)
        (let ((fi-name (git--fileinfo->name fi)))
          ;; update fileinfo name as "path/name"
          (setf (git--fileinfo->name fi)
                (git--concat-path-only name fi-name))

          ;; update lessp by force
          (setf (git--fileinfo->lessp fi) 2)

          (setq node (ewoc-enter-after git--status-view node fi))))
    
      (setf (git--fileinfo->expanded data) t))))

(defun git--shrink-tree (node)
  "Shrink 'node' in 'git--status-view', but node->type should be 'tree"
  
  (let* ((data (ewoc-data node))
         (type (git--fileinfo->type data))
         (name (git--fileinfo->name data)))

    (unless (eq type 'tree) (error "type should be 'tree"))
  
    (when (git--fileinfo->expanded data)
      ;; make regexp "node->name/"
      (git--status-delete-afer-regex (ewoc-next git--status-view node)
                                     (file-name-as-directory name))
      (setf (git--fileinfo->expanded data) nil))))


(defun git--status-view-expand-tree-toggle ()
  "Expand if tree is not expanded otherwise close the tree"

  (interactive)

  (let* ((node (ewoc-locate git--status-view))
         (node-info (ewoc-data node)))
    (when (and node node-info
               (eq (git--fileinfo->type node-info) 'tree))
      (if (git--fileinfo->expanded node-info)
          (git--shrink-tree node)
        (git--expand-tree node)))))

;;-----------------------------------------------------------------------------
;; status view moving
;;-----------------------------------------------------------------------------

(defun git--status-view-forward-line (n)
  "Move to forward on the status view item"

  (interactive "p")
  
  (let ((dir (/ n (abs n))))
    (forward-line n)

    (while (or (looking-at "^[\n\t ]+$")
               (looking-at "^[^ ]"))
      (forward-line dir)))

  (move-to-column git--status-line-column))

(defun git--status-view-first-line ()
  "Move to the first item"
  
  (interactive)
  (goto-char (point-min))
  (git--status-view-forward-line 1))

(defun git--status-view-last-line ()
  "Move to the last item"
  
  (interactive)
  (goto-char (point-max))
  (git--status-view-forward-line -1))

(defun git--forward-meaningfull-line (move)
  "Implementation of forward meaningful line"

  (let ((start-node (ewoc-locate git--status-view)))
    (funcall move 1)

    (while (and (eq 'uptodate
                    (git--fileinfo->stat (ewoc-data (ewoc-locate git--status-view))))
                (not (eq start-node (ewoc-locate git--status-view))))
      (funcall move 1))))

(defun git--status-view-next-line (&optional n)
  "Move to the next line"

  (interactive "p")

  (if (eql (ewoc-locate git--status-view)
           (ewoc-nth git--status-view -1))
      (git--status-view-first-line)
    (git--status-view-forward-line 1)))

(defun git--status-view-next-meaningfull-line ()
  "Move to the meaningful next line"

  (interactive)
  (git--forward-meaningfull-line 'git--status-view-next-line))

(defun git--status-view-prev-line (&optional n)
  "Move to the previous line"
  
  (interactive "p")

  (if (eql (ewoc-locate git--status-view)
           (ewoc-nth git--status-view 0))
      (git--status-view-last-line)
    (git--status-view-forward-line -1)))

(defun git--status-view-prev-meaningfull-line ()
  "Move the the meaningful previous line"
  
  (interactive)
  (git--forward-meaningfull-line 'git--status-view-prev-line))

;;-----------------------------------------------------------------------------
;; status view marking
;;-----------------------------------------------------------------------------

(defun git--mark-line (marked)
  "Implementation of marking"

  (let ((node (ewoc-locate git--status-view)))
    (setf (git--fileinfo->marked (ewoc-data node)) marked)
    (ewoc-invalidate git--status-view node)))

(defun git--status-view-mark-and-next ()
  "Mark and go to the next line"

  (interactive)
  (git--mark-line t)
  (git--status-view-next-line))

(defun git--status-view-unmark-and-next ()
  "Unmark and go to the next line"
  
  (interactive)
  (git--mark-line nil)
  (git--status-view-next-line))

(defun git--toggle-line ()
  "Implementation of toggle line"
  
  (let* ((node (ewoc-locate git--status-view))
         (data (ewoc-data node))
         (mark (git--fileinfo->marked data)))
    (setf (git--fileinfo->marked data) (not mark))
    (ewoc-invalidate git--status-view node)))

(defun git--status-view-toggle-and-next ()
  "Toggle the mark and go to next line"

  (interactive)
  (git--toggle-line)
  (git--status-view-next-line))

;;-----------------------------------------------------------------------------
;; status view independent command
;;-----------------------------------------------------------------------------

(defun git--status-view-quit ()
  "Quit"

  (interactive)
  (kill-buffer (current-buffer)))

(defun git--status-view-switch-branch ()
  "Switch branch"

  (interactive)
  (call-interactively 'git-switch-branch))

(defun git--status-view-git-cmd ()
  "Direct git command"

  (interactive)
  (call-interactively 'git-cmd))

(defun git--status-view-gitk ()
  "Launch gitk"

  (interactive)
  (call-interactively 'gitk))

(defun git--status-view-refresh ()
  "Refresh view"

  (interactive)
  (revert-buffer))

(defun git--status-view-mark-reg (reg)
  "Mark with regular expression"

  (interactive "sRegexp >> ")
  (ewoc-collect git--status-view
                #'(lambda (data)
                    (when (string-match reg (git--fileinfo->name data))
                      (setf (git--fileinfo->marked data) t))))

  (ewoc-refresh git--status-view)
  (git--status-view-first-line)
  (git--status-view-next-meaningfull-line))

(defun git--status-view-summary ()
  "To the summary mode with occur"
  
  (interactive)
  (occur "[\t* ]+\\(Deleted\\|Modified\\|Unknown\\|Added\\)")
  
  (message "Move with 'next-error and 'previous-error"))

;;-----------------------------------------------------------------------------
;; status view for one selected file
;;-----------------------------------------------------------------------------

(defsubst git--status-view-select-filename ()
  "Return current filename of view item"

  (git--fileinfo->name (ewoc-data (ewoc-locate git--status-view))))

(defsubst git--status-view-select-type ()
  "Return current type of view item"

  (git--fileinfo->type (ewoc-data (ewoc-locate git--status-view))))

(defun git--status-view-view-file ()
  "View the selected file"

  (interactive)
  (view-file (git--status-view-select-filename)))

(defun git--status-view-open-file ()
  "Open the selected file"

  (interactive)
  (find-file (git--status-view-select-filename)))

(defun git--status-view-diff-file ()
  "Diff the selected file"

  (interactive)
  (git-diff-cmd (git--status-view-select-filename)))

(defun git--status-view-resolve-merge ()
  "Resolve the conflict if necessary"
  
  (interactive)

  (let ((file (git--status-view-select-filename)))
    (if (eq 'unmerged (git--status-file file))
        (progn
          (find-file (git--status-view-select-filename))
          (git--resolve-merge-buffer (current-buffer)))
      (error "Selected file is not unmerged state"))))

(defun git--status-view-do-propriate ()
  "If 'tree selected -> expand or un-expand otherwise open it"

  (interactive)

  (case (git--status-view-select-type)
    ('tree (git--status-view-expand-tree-toggle))
    ('blob (git--status-view-open-file))
    (t (error "Not supported type"))))

(defun git--status-view-blame ()
  "Open the file as blame-mode"

  (interactive)

  (when (eq (git--status-view-select-type) 'blob)
    (find-file (git--status-view-select-filename))
    (git-blame-mode t)))

;;-----------------------------------------------------------------------------
;; status view for all marked files or selected
;;-----------------------------------------------------------------------------

(defsubst git--status-view-marked-files ()
  "Scrap the all marked files"

  (let (files)
    (ewoc-collect git--status-view
                  #'(lambda (node)
                      (when (git--fileinfo->marked node)
                        (push (git--fileinfo->name node) files))))
    files))

(defsubst git--status-view-makred-or-file ()
  "If not marked -> rename for current file"

  (let ((files (git--status-view-marked-files)))
    (when (null files)
      (setq files (list (git--status-view-select-filename))))
    files))

(defun git--status-view-rm ()
  "Delete the whole marked files"

  (interactive)

  (let* ((files (git--status-view-makred-or-file))
         (msg (format "total %s files including '%s'"
                      (length files)
                      (file-name-nondirectory (car files)))))
    
    (unless (y-or-n-p (format "Really want to %s %s?"
                              (git--bold-face "delete")
                              msg))
      (error "Aborted deletion"))
                      
    (dolist (file files)
      (git--rm file)))

  (revert-buffer))

(defun git--status-view-rename ()
  "Renamed the whole marked files"

  (interactive)

  (let ((files (git--status-view-makred-or-file)))
    (dolist (src files)
      (let ((msg (format "%s '%s' to >> " (git--bold-face "Rename") src)))
        (git--mv src (file-relative-name (read-from-minibuffer msg src))))))

  (revert-buffer))
  
(defun git--status-view-add ()
  "Add the selected files"

  (interactive)
  (git--add (git--status-view-makred-or-file))
  (revert-buffer))

(defun git--status-view-add-ignore ()
  "Add the selected file to .gitignore"

  (interactive)

  (let ((files (git--status-view-marked-files)))
    (unless files (list (read-from-minibuffer "Add Ignore >> ")))

    (dolist (file files)
      (git-ignore file)))

  (revert-buffer))

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

  (let ((branchs (git--branch-list)))
    (git--select-from-user "Select Branch : "
                           (remove-if (lambda (b) (member b excepts)) branchs))))
                         
(defsubst git--select-tag ()
  "Select the tag"

  (git--select-from-user "Select Tag : " (git--tag-list)))

(defsubst git--select-revision ()
  "Select the revision"
  
  (git--select-from-user "Select : " (append (git--branch-list)
                                             (git--tag-list))))

(defvar git--switch-branch-auto-msg nil "confirm the auto-generated message")

(defun git--switch-branch (branch)
  "Implementation of switch-branch"
  
  (let* ((current (git--current-branch))
         (msg (format "Switch from '%s' to '%s'" current branch)))

    (unless git--switch-branch-auto-msg
      (setq msg (read-from-minibuffer "Commit Log >> " msg)))

    ;; commit with a automatically generated msg
    (git--commit msg "-a")

    ;; switch to different branch
    (git-checkout branch)))

;;-----------------------------------------------------------------------------
;; vc-git integration
;;-----------------------------------------------------------------------------

(defun git--update-modeline ()
  "Update modeline state dot mark properly"
  
  ;; mark depending on the fileinfo state
  (when (and buffer-file-name (git--in-vc-mode?))
    (git--update-state-mark
     (git--interprete-state-mode-color
      (git--status-file (file-relative-name buffer-file-name))))))

;; simple highlighting for log view
(font-lock-add-keywords 'vc-git-log-view-mode
                        '(("^\\([Aa]uthor\\|[Cc]ommit\\|[Dd]ate\\)"
                           1 font-lock-keyword-face prepend)))

(defun git-log ()
  "Launch the git log view for the file you opened"

  (interactive)

  (if (git--in-vc-mode?)
      (progn
        ;; call vc-log
        (call-interactively 'vc-print-log)

        ;; close window with key 'q'
        (local-set-key "q" 'git--quit-buffer))
    (git-log-all)))
  
(defvar git--log-view-buffer "*git-log-view*")

(defun git-log-all ()
  "Launch the git log view for the whole project"
  
  ;; vc-git-log-view-mode
  (interactive)

  (let ((buffer (get-buffer-create git--log-view-buffer)))
    (with-current-buffer buffer
      (let ((buffer-read-only nil)) (erase-buffer))

      (local-set-key "q" 'git--quit-buffer)

      (vc-git-log-view-mode)

      (save-excursion 
        (git--rev-list "--pretty=full" "HEAD"))

      (message "Please 'q' to quit"))
    (pop-to-buffer buffer)))

(defalias 'git-history 'git-log-all)

(defsubst git--in-vc-mode? ()
  "Check see if in vc-git is under vc-git"
  
  (and vc-mode (string-match "^ Git" (substring-no-properties vc-mode))))

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

  (insert (propertize git--log-header-line 'face 'git--log-line-face)  "\n"
          (git--bold-face "# Branch  : ") (git--current-branch)        "\n"
          (git--bold-face "# Author  : ") (git--config-get-author)     "\n"
          (git--bold-face "# Email   : ") (git--config-get-email)      "\n"
          (git--bold-face "# Date    : ") (git--today)                 "\n"))

(defun git--insert-log-files-status ()
  "Insert log file status to the buffer"
  
  (insert (propertize git--log-file-line 'face 'git--log-line-face) "\n")

  (dolist (fi (git--status-index))
    (insert (format "#  %-15s : %s\n"
                    (git--status-node-stat fi)
                    (git--fileinfo->name fi)))))

(defun git--insert-log-status ()
  "Insert log status to the buffer"
  
  (insert (propertize git--log-sep-line 'face 'git--log-line-face) "\n")
  (git--exec-buffer "status"))

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
        (message (git--commit (git--trim-string (buffer-substring begin end)) "-a")))))

  ;; close window
  (delete-window)
  (kill-buffer git--commit-log-buffer)

  ;; update
  (git--update-modeline))

(defun git--resolve-fill-buffer (template side)
  "Make the new buffer based on the conflicted template on each side(working and checkedin)"

  (let* ((filename (file-relative-name (buffer-file-name template)))
         (buffer-name (concat "*" filename ": " (capitalize (symbol-name side)) "*"))
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
            ('workfile (delete-region conflict-sep conflict-end))
            ('checked-in (delete-region conflict-begin conflict-sep))
            (t (error "Side argument have to be one of 'workfile or 'checked-in"))))))
    buffer-name))

(defun git-merge ()
  "Git merge"

  (interactive)

  (let ((branch (git--select-branch (git--current-branch))))
    (git--merge branch)
    (git-status ".")))

(defvar git--resolve-window-config)
(defvar git--resolve-buffer)

(defun git--resolve-merge-buffer (result-buffer)
  "Implementation of resolving conflicted buffer"

  (setq result-buffer (current-buffer))
  
  (interactive)
  (let* ((filename (file-relative-name buffer-file-name))
         (your-buffer (git--resolve-fill-buffer result-buffer 'workfile))
         (other-buffer (git--resolve-fill-buffer result-buffer 'checked-in))
         (config (current-window-configuration))
         (ediff-default-variant 'default-B))

    ;; set merge buffer first
    (set-buffer (ediff-merge-buffers your-buffer other-buffer))

    (set (make-local-variable 'git--resolve-buffer) result-buffer)
    (set (make-local-variable 'git--resolve-window-config) config)
    (set (make-local-variable 'ediff-quit-hook)
         #'(lambda ()
             (let ((buffer-A ediff-buffer-A)
                   (buffer-B ediff-buffer-B)
                   (buffer-C ediff-buffer-C)
                   (windows git--resolve-window-config)
                   (result git--resolve-buffer))
               (ediff-cleanup-mess)
               (set-buffer result)
               (erase-buffer)
               (insert-buffer-substring buffer-C)
               (kill-buffer buffer-A)
               (kill-buffer buffer-B)
               (kill-buffer buffer-C)
               (set-window-configuration windows)
               (message "Conflict resolution finished, you may save the buffer"))))
    (message "Please resolve conflicts now, exit ediff when done")))

(defun git-resolve-merge ()
  "Resolve merge for the current buffer"
  
  (interactive)
  (git--resolve-merge-buffer (current-buffer)))

(defun git-commit-all ()
  "git commit -a like commit command"

  (interactive)
  
  (let ((cur-pos nil)
        (buffer (get-buffer-create git--commit-log-buffer)))
    (with-current-buffer buffer
      (local-set-key "\C-c\C-c" 'git--commit-buffer)
      (erase-buffer)

      ;; insert info
      (git--insert-log-header-info)
      (git--insert-log-files-status)

      ;; real log space
      (insert (propertize git--log-sep-line 'face 'git--log-line-face) "\n")

      (insert "\n")
      (setq cur-pos (point))
      (insert "\n\n")

      ;; git status
      (git--insert-log-status)
      
      ;; set cursor 
      (goto-char cur-pos)

      ;; flyspell-mode
      (when git--log-flyspell-mode (flyspell-mode t))

      ;; hello~
      (message "Please 'C-cC-c' to commit"))
    (pop-to-buffer buffer)))

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
  (let ((repository (ido-completing-read "Repository : "
                                         git--repository-bookmarks
                                         nil
                                         nil
                                         ""
                                         git--repository-history)))
    (with-temp-buffer
      (cd dir)
      (git--clone repository))))

(defun git-reset-hard (&rest args)
  "Reset hard"

  (interactive)
  (message (git--trim-string (git--reset "--hard" (git--select-tag)))))

(defun git-revert ()
  "Revert to other revision"

  (interactive)
  (message (git--trim-string (git--revert (git--select-revision))))

  ;; revert buffer
  (revert-buffer))

(defun gitk ()
  "Launch gitk in emacs"

  (interactive)
  (start-process "gitk" nil "gitk"))
    
(defun git-checkout (&optional rev)
  "Checkout from 'tag' & 'branch' list when 'rev' is null"

  (interactive)
  (unless rev (setq rev (git--select-revision)))

  ;; TODO : sophisticated message control
  (message (git--trim-string (git--checkout rev))))

(defalias 'git-create-branch 'git-checkout-to-new-branch)

(defun git-checkout-to-new-branch (branch)
  "Checkout to new list based on tag"

  (interactive "sNew Branch : ")
  (let* ((tag (git--select-revision))
         (msg (git--checkout "-b" branch tag)))
    (if (string-match "^Switched" msg)
        (message "%s to the new branch '%s'"
                 (git--bold-face "Switched")
                 (git--bold-face branch))
      (message "%s on creating '%s' from '%s'"
               git--msg-critical
               (git--bold-face branch)
               (git--bold-face tag))))

  ;; refresh buffer content
  (revert-buffer))


(defun git-delete-branch (&optional branch)
  "Delete branch after selecting branch"

  (interactive)

  ;; select branch if not assigned
  (unless branch (setq branch (git--select-branch "master")))
  
  (let* ((msg (git--branch "-d" branch)))
    (if (string-match "^Deleted" msg)
        (message "%s '%s' branch" (git--bold-face "Deleted") branch)
      (message "%s on %s '%s' branch in '%s' branch"
               git--msg-critical
               (git--bold-face "deleting")
               (git--bold-face branch)
               (git--current-branch)))))

(defun git-delete-tag ()
  "Delete tag after selecting tag"
  
  (interactive)
  (let ((tag (git--select-tag)))
    (if (string-match "^Deleted" (git--tag "-d" tag))
        (message "%s '%s' Tag" (git--bold-face "Deleted") tag)
      (message "%s on %s '%s' Tag"
               git--msg-critical
               (git--bold-face "deleting")
               tag))))

(defun git-status (dir)
  "Launch git-status mode at the directory if it is under 'git'"

  (interactive "DSelect directory: ")

  (setq dir (git--get-top-dir dir))
  (if (file-directory-p (git--expand-to-repository-dir dir))
      (progn
        (switch-to-buffer (git--create-status-buffer dir))
        (cd dir)
        (git--status-mode)
        (git--status-new)
        (git--status-view-first-line))
    ;; (add-hook 'after-save-hook 'git-update-saved-file)))
    (message "%s is not a git working tree." dir)))

(defun git-regression ()
  "Regression tests on git-emacs, but have to enhance it!"

  (interactive)

  ;; git exec
  (assert (string= "\n" (git--exec-string "rev-parse" "--show-cdup")))
  (assert (string= (expand-file-name "./") (git--get-top-dir ".")))

  ;; create status buffer
  (assert (string= (buffer-name (git--create-status-buffer "."))
                   (git--status-buffer-name ".")))

  ;; open status buffer
  (assert (string= (buffer-name (git--create-status-buffer "."))
                   (git--status-buffer-name ".")))

  (git--kill-status-buffer ".")

  ;; testing
  (assert (null (string-match "asdf/" "asdf"))))

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

(defvar git--diff-buffer nil "locally saved buffer for ediffing")
(defvar git--diff-window nil "locally saved windows for ediffing")

(defun git--diff (file rev &rest args)
  "Implementation of git-diff, it should be called with file and revision"

  (setq abspath (expand-file-name file))
  
  (let* ((buf1 (find-file-noselect file))
	 (buf2 nil)
	 (config (current-window-configuration)))
  
    ;; build buf2
    (with-temp-buffer 
      (let ((abspath (expand-file-name file))
	    (filename nil))

	;; get relative to git root dir
	(cd (git--get-top-dir (file-name-directory file)))
	(setq rev (concat rev (file-relative-name abspath)))
	(setq buf2 (git--cat-file rev "blob" rev))))

    ;; set ediff type
    (setq ediff-split-window-function 'split-window-horizontally)
    (set-buffer (ediff-buffers buf1 buf2))
 
    (set (make-local-variable 'git--diff-buffer) buf2)
    (set (make-local-variable 'git--diff-window) config)
    (set (make-local-variable 'ediff-quit-hook)
         #'(lambda ()
             (let ((buffer git--diff-buffer)
                   (window git--diff-window))
               
               (ediff-cleanup-mess)               
               (set-buffer buffer)
               (kill-buffer buffer)
               (set-window-configuration window))))))

(defun git-diff-head (file)
  "Simple diffing with the previous HEAD"
  
  (interactive "fSelect Diff Target : ")
  (git--diff file "HEAD:"))

(defun git-diff (file)
  "Diffing with the target file and revision user selected"

  (interactive "fSelect Diff Target : ")

  (let ((prompt (format "git diff [rev]:%s >> " (file-relative-name file))))
    (git--diff file (concat (read-from-minibuffer prompt "HEAD") ":"))))

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

(defun git-ignore (ignored-opt)
  "Add ignore file"
  
  (interactive "sIgnore Option : ")

  (with-temp-buffer
    (insert ignored-opt "\n")
    (append-to-file (point-min) (point-max) ".gitignore")))
  
(defun git-switch-branch ()
  "Git switch branch, user have to select the branch which you will move on"
  
  (interactive)
  (git--switch-branch (git--select-branch (git--current-branch)))
  (revert-buffer))

;;-----------------------------------------------------------------------------
;; branch mode
;;-----------------------------------------------------------------------------

(defvar git--branch-mode-map nil)
(defvar git--branch-mode-hook nil)

(unless git--branch-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)

    (define-key map "q"     'git--branch-mode-quit)
    (define-key map "n"     'next-line)
    (define-key map "p"     'previous-line)

    (define-key map "d"     'git--branch-mode-delete)
    (define-key map "c"     'git--branch-mode-checkout)
    (define-key map "s"     'git--branch-mode-switch)
    (define-key map "\C-m"  'git--branch-mode-switch)

    (setq git--branch-mode-map map)))

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

(defun git--branch-mode-checkout ()
  "Git branch mode checkout"

  (interactive)
  (git--branch-mode-throw 'checkout))

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
        (nbranchs 0))

    (with-current-buffer buffer
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
      (git--branch-mode-interprete selected-branch))))

(defun git--branch-mode-interprete (selected-branch)
  "git-branch command interpreter,
if 'delete -> call 'git-delete-branch
if 'switch -> call 'git-switch-branch
if 'checkout -> call git-checkout-to-new-branch"

  (when selected-branch
    (let ((command (car selected-branch))
          (branch (cdr selected-branch)))
    (case command
      ('delete
       (when (y-or-n-p (format "Would you like to %s the branch, %s? "
                               (git--bold-face "delete")
                               (git--bold-face branch)))
         (git-delete-branch branch)))
      ('switch
       (when (y-or-n-p (format "Would you like to %s from %s to %s branch? "
                               (git--bold-face "switch")
                               (git--bold-face (git--current-branch))
                               (git--bold-face branch)))
         (git--switch-branch branch)
         (revert-buffer)))
      ('checkout (call-interactively 'git-checkout-to-new-branch))))))

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

(provide 'git-emacs)