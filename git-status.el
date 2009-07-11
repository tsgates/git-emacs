;; See git-emacs.el for license and versioning

(require 'git-emacs)                    ; main module
(require 'ewoc)                         ; view

;;-----------------------------------------------------------------------------
;; Variables and utility functions
;;-----------------------------------------------------------------------------

(defvar git-status-mode-hook  nil
  "Hooks to run upon entering git-status-mode")
(defvar git-status-mode-map nil
  "Keymap for git-status-mode")
(defvar git--status-view nil
  "EWOC for git-status-mode")


(defconst git--status-header-format "     %-2s %-10s %-5s %4s  %s")
(defconst git--status-line-column 32)

(defsubst git--status-header ()
  ;; Put spaces above the scrollbar and the fringe
  (format
   (concat (make-string (+ (scroll-bar-columns 'left) (fringe-columns 'left))
                        ? )
           git--status-header-format)
   "M" "STATUS" "PERM" "SIZE" "FILE"))

(defun git--refresh-desc ()
  "Refresh the git-status-mode header description"
  
  (ewoc-set-hf git--status-view
               (concat (git--bold-face "Directory") " : " default-directory     "\n"
                       (git--bold-face "Branch   ") " : " (git--current-branch) "\n"
                       (git--bold-face "Last Log ") " : " (git--last-log-short) "\n")
               ""))

(defsubst git--status-buffer-name (dir)
  (format "*git-status on %s*" (abbreviate-file-name (expand-file-name dir))))

(defsubst git--create-status-buffer (dir)
  (let* ((status-buffer-name (git--status-buffer-name dir))
         (status-buffer (get-buffer status-buffer-name)))
    (or status-buffer (get-buffer-create status-buffer-name))))

(defsubst git--kill-status-buffer (dir)
  (kill-buffer (git--status-buffer-name dir)))

;;-----------------------------------------------------------------------------
;; faces
;;-----------------------------------------------------------------------------

(defmacro git--face (name fore1 prop1 fore2 prop2)
  `(defface ,(intern (concat "git--" (symbol-name name) "-face"))
     '((((class color) (background light)) (:foreground ,fore1 ,@prop1))
       (((class color) (background dark))  (:foreground ,fore2 ,@prop2)))
    ,(concat "git " (symbol-name name) " face in status buffer mode")
    :group 'git))

(git--face mark       "red"    (:bold t) "tomato"  (:bold t))
(git--face mark-tree  "blue"   (:bold t) "yellow"  (:bold t))
(git--face mark-blob  "black"  () "white" ())
(git--face mark-submodule "cyan" ()      "cyan"    ())
(git--face unknown    "black"  (:bold t) "white"   (:bold t))
(git--face ignored    "gray"   (:bold t) "gray"    (:bold t))
(git--face modified   "tomato" (:bold t) "tomato"  (:bold t))
(git--face unmerged   "red"    (:bold t) "magenta" (:bold t))
(git--face uptodate   "gray"   (:bold t) "green"   ())
(git--face added      "tomato" (:bold t) "cyan"    (:bold t))
(git--face deleted    "red"    (:bold t) "red"     (:bold t))
(git--face staged     "yellow" (:bold t) "yellow"  (:bold t))
(git--face log-line   "gray"   (:bold t :italic t) "gray"(:bold t :italic t))


;;-----------------------------------------------------------------------------
;; status view rendering
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
                  ('staged   'git--staged-face   )
                  (t nil)))))

(defsubst git--status-node-perm (info)
  "Render status view node permission"
  
  (or (git--fileinfo->perm info) "------"))

(defun git--status-human-readable-size (size)
  "Given a size in bytes, returns a string size of at most four chars, similar
to ls -sh; e.g. 29152 -> 28K."
  (if (< size 1024)
      (format "%d" size)
    (let ((suffixes "KMGT") (i 0))
      (while (and (< i (length suffixes))
                  (>= size 1000))       ; 1023K would be 5 chars
        (setq size (/ size 1024.0))
        (incf i))
      (format (if (< size 10) "%.1f%c" "%.0f%c")
              size (elt suffixes (- i 1))))))

(defsubst git--status-node-size (info)
  "Render status view node size"

  (let ((size (git--fileinfo->size info)))
    (if (not size) ""
      (git--status-human-readable-size size))))
      
(defsubst git--status-node-name (info)
  "Render status view node name"
  
  (let ((name (git--fileinfo->name info))
        (type (git--fileinfo->type info)))

    (setq name (replace-regexp-in-string "[^/]+/" "    " name))
    (format
     (if (eq type 'commit) "%s  [submodule>]" "%s")
     (propertize name 'face
                 (case type
                   ('tree 'git--mark-tree-face)
                   ('blob 'git--mark-blob-face)
                   ('commit 'git--mark-submodule-face)
                   (t (error "Can't be!")))))))
                  
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

;;-----------------------------------------------------------------------------
;; status mode definition
;;-----------------------------------------------------------------------------

(defun git-status-mode ()
  "Major mode for viewing and editing the state of a git directory."
  
  (kill-all-local-variables)
  (buffer-disable-undo)

  ;; set major mode
  (setq mode-name "git status")
  (setq major-mode 'git-status-mode)

  (use-local-map git-status-mode-map)

  (setq buffer-read-only t)
  (setq header-line-format (git--status-header))

  ;; create ewoc for current git-status buffer
  (set (make-local-variable 'git--status-view)
       (ewoc-create 'git--render-file-status "" ""))

  (set (make-local-variable 'revert-buffer-function)
       'git-status-mode-revert-buffer)

  (run-hooks 'git-status-mode-hook))

(defun git-status-mode-revert-buffer (ignore-auto noconfirm)
  "Refresh status information."

  ;; TODO refresh status-mode-buffer
  (git--status-new)
  (git--status-view-first-line))


;; autoloaded entry point
(defun git-status (dir)
  "Launch git-status-mode on the specified directory."

  (interactive (list (git--get-top-dir-or-prompt "Select directory: ")))

  (setq dir (git--get-top-dir dir))
  (if (file-directory-p (git--expand-to-repository-dir dir))
      (progn
        (switch-to-buffer (git--create-status-buffer dir))
        (cd dir)
        (git-status-mode)
        (git--please-wait "Reading git status"
                          (git--status-new))
        (git--status-view-first-line))
    ;; (add-hook 'after-save-hook 'git-update-saved-file)))
    (message "%s is not a git working tree." dir)))

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

(defsubst git--status-refresh ()
  (let ((pos (point)))
    (ewoc-refresh git--status-view)
    (goto-char pos)))

(defun git--status-add-size (fileinfo)
  "Fill in the size field of a fileinfo"
  (let ((attrs (file-attributes (git--fileinfo->name fileinfo))))
    (when (and attrs (not (first attrs)))
      (setf (git--fileinfo->size fileinfo) (elt attrs 7)))))
    
(defun git--status-new ()
  "Create new status-view buffer in current buffer"

  (git--clear-status)
  (git--refresh-desc)

  ;; add new file infos
  (dolist (info (git--status-tree))
    (git--status-add-size info)
    (ewoc-enter-last git--status-view info))

  ;; add modified/renamed etc file infos
  (git--status-view-update)

  ;; add unknown file
  (let ((fileinfo (git--ls-files "-o" "--exclude-standard")))
    (git--status-view-update-expand-tree fileinfo)

    (let ((iter (ewoc-nth git--status-view 0)))
      (dolist (fi fileinfo)
        (git--status-add-size fi)
        (setq iter (git--status-map iter (lambda (node data)
                                           (when (git--fileinfo-lessp fi data)
                                             (ewoc-enter-before git--status-view node fi))))))))
  (git--status-refresh))

(defsubst git--status-delete (node)
  
  (let ((buffer-read-only nil)) 
    (ewoc-delete git--status-view node)))

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

(let ((map (make-keymap)))
  (suppress-keymap map)

  (define-key map "n" 'git--status-view-next-line)
  (define-key map "p" 'git--status-view-prev-line)
  (define-key map "N" 'git--status-view-next-meaningful-line)
  (define-key map "P" 'git--status-view-prev-meaningful-line)
  (define-key map "l" 'git-log)
  (define-key map "m" 'git--status-view-mark-and-next)
  (define-key map "u" 'git--status-view-unmark-and-next)
  (define-key map " " 'git--status-view-toggle-and-next)
  (define-key map "q" 'git--status-view-quit)
  (define-key map "<" 'git--status-view-first-line)
  (define-key map ">" 'git--status-view-last-line)

  (define-key map "e" 'git--status-view-expand-tree-toggle)
  (define-key map "v" 'git--status-view-view-file)
  (define-key map "o" 'git--status-view-open-file)
  ;; Use the sub-maps from git-global-keys for diffs.
  (define-key map "d" (copy-keymap git--diff-buffer-map))
  (define-key map "D" (copy-keymap git--diff-all-map))
  (define-key map "b" 'git--status-view-switch-branch)
  (define-key map "!" 'git--status-view-resolve-merge)
  (define-key map "." 'git--status-view-git-cmd)
  (define-key map "k" 'git--status-view-gitk)
  (define-key map "L" 'git-log-files)
  (define-key map "g" 'git--status-view-refresh)
  (define-key map "a" 'git--status-view-add)
  (define-key map "i" 'git--status-view-add-ignore)
  (define-key map "r" 'git--status-view-rename)
  (define-key map "?" 'git--status-view-blame)
  (define-key map (kbd "<delete>") 'git--status-view-rm)
  (define-key map "*" 'git--status-view-mark-reg)
  (define-key map "s" 'git--status-view-summary)
  (define-key map "z" 'git-branch)

  (define-key map "c" (copy-keymap git--commit-map))
  (define-key map "R" 'git-reset)

  (define-key map "\C-m" 'git--status-view-do-propriate)

  (setq git-status-mode-map map))

(easy-menu-define gitemacs-menu git-status-mode-map
  "Git"
  `("Emacs-Git"
    ["Refresh" git--status-view-refresh t]
    ["First Line" git--status-view-first-line t]
    ["Last Line" git--status-view-last-line t]
    ["Next Line" git--status-view-next-line t]
    ["Previous Line" git--status-view-prev-line t]
    ["Next Meaningful Line" git--status-view-next-meaningful-line t]
    ["Previous Meaningful Line" git--status-view-prev-meaningful-line t]
    ["Expand Tree" git--status-view-expand-tree-toggle]
    "----" 
    ["Add File" git--status-view-add t]
    ["Ignore File" git--status-view-add-ignore t]
    ["Rename File" git--status-view-rename t]
    ["Open File" git--status-view-open-file t]
    ["View File" git--status-view-view-file t]
    ("Diff File against"
     ;; We want the short keys to appear here rather than the global keys
      ["HEAD" git-diff-head :keys "d RET" :active t]
      ["Index" git-diff-index :keys "d i" :active t]
      ["Baseline" git-diff-baseline :keys "d b" :active t]
      ["Other..." git-diff-other :keys "d o" :active t])
    ("Diff Repository against"
     ["HEAD" git-diff-all-head :keys "D RET" :active t]
     ["Index" git-diff-all-index :keys "D i" :active t]
     ["Baseline" git-diff-all-baseline :keys "D b" :active t]
     ["Other..." git-diff-all-other :keys "D o" :active t])
    ["Delete File" git--status-view-rm]
    ["View Summary" git--status-view-summary t]
    ["Log for Selected File(s)" git-log-files :keys "L" :active t]
    ["Mark" git--status-view-mark-and-next t]
    ["Unmark" git--status-view-unmark-and-next t]
    "----"
    ["Branch Mode" git-branch t]
    ["Switch to Branch..." git--status-view-switch-branch t]      
    ("Commit"
     ["All Changes" git-commit-all :keys "c RET" :active t]
     ["Index" git-commit :keys "c i" :active t]
     ["Selected File(s)" git-commit-file :keys "c f" :active t])
    ["Reset..." git-reset :keys "R" :active t]
    ["Resolve Merge" git--status-view-resolve-merge t]
    ["Merge..." git-merge t]
    ["Revert" git-revert t]
    ["Log for Project" git-log t]
    "----"
    ["Git Command" git--status-view-git-cmd t]
    ["GitK" git--status-view-gitk t]
    "----"
    ["Quit" git--status-view-quit t]))


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

          (git--status-add-size fi)

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

(defun git--forward-meaningful-line (move)
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

(defun git--status-view-next-meaningful-line ()
  "Move to the meaningful next line"

  (interactive)
  (git--forward-meaningful-line 'git--status-view-next-line))

(defun git--status-view-prev-line (&optional n)
  "Move to the previous line"
  
  (interactive "p")

  (if (eql (ewoc-locate git--status-view)
           (ewoc-nth git--status-view 0))
      (git--status-view-last-line)
    (git--status-view-forward-line -1)))

(defun git--status-view-prev-meaningful-line ()
  "Move the the meaningful previous line"
  
  (interactive)
  (git--forward-meaningful-line 'git--status-view-prev-line))

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
  (git--please-wait "Reading git status" (revert-buffer)))

(defun git--status-view-mark-reg (reg)
  "Mark with regular expression"

  (interactive "sRegexp >> ")
  (ewoc-collect git--status-view
                #'(lambda (data)
                    (when (string-match reg (git--fileinfo->name data))
                      (setf (git--fileinfo->marked data) t))))

  (ewoc-refresh git--status-view)
  (git--status-view-first-line)
  (git--status-view-next-meaningful-line))

(defun git--status-view-summary ()
  "To the summary mode with occur"
  
  (interactive)
  (occur "[\t* ]+\\(Deleted\\|Modified\\|Unknown\\|Added\\|Staged\\)")
  
  (message "Move with 'next-error and 'previous-error"))

;;-----------------------------------------------------------------------------
;; status view for one selected file
;;-----------------------------------------------------------------------------

(defsubst git--status-view-select-filename ()
  "Return current filename of view item"

  (let ((filename (git--fileinfo->name (ewoc-data (ewoc-locate git--status-view)))))
    (when (file-directory-p filename)
      (error "Execute on file"))
    filename))

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

(defun git--status-view-descend-submodule ()
  "Opens a status view on the selected submodule"
  (let ((submodule-dir (git--fileinfo->name
                        (ewoc-data (ewoc-locate git--status-view)))))
    (git-status submodule-dir)
    (message "Viewing submodule \"%s\", close buffer to return"
             submodule-dir)))

(defun git--status-view-resolve-merge ()
  "Resolve the conflict if necessary"
  
  (interactive)

  (let ((file (git--status-view-select-filename)))
    (if (eq 'unmerged (git--status-file file))
        (progn
          (find-file (git--status-view-select-filename))
          (git--resolve-merge-buffer))
      (error "Selected file is not unmerged state"))))

(defun git--status-view-do-propriate ()
  "If 'tree selected -> expand or un-expand otherwise open it"

  (interactive)

  (case (git--status-view-select-type)
    ('tree (git--status-view-expand-tree-toggle))
    ('blob (git--status-view-open-file))
    ('commit (git--status-view-descend-submodule))
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
  "Return a list of the marked files. Usually,
`git--status-view-marked-or-file' is what you want instead."

  (let (files)
    (ewoc-collect git--status-view
                  #'(lambda (node)
                      (when (git--fileinfo->marked node)
                        (push (git--fileinfo->name node) files))))
    files))

(defsubst git--status-view-marked-or-file ()
  "Return a list of the marked files, or if none, the file on the
current line. You can think of this as the \"selected files\"."

  (let ((files (git--status-view-marked-files)))
    (when (null files)
      (setq files (list (git--status-view-select-filename))))
    files))

(defun git--status-view-rm ()
  "Delete the selected files."

  (interactive)

  (let* ((files (git--status-view-marked-or-file))
         ;; We can't afford to use stale fileinfos here, the warnings
         ;; are crucial.
         (fresh-fileinfos (append (apply #'git--status-index files)
                                  (apply #'git--ls-files "-o" "--" files)))
         (untracked-files nil) (pending-files nil))
    (dolist (fi fresh-fileinfos)
      (let ((stat (git--fileinfo->stat fi)) (name (git--fileinfo->name fi)))
        (if (member stat '(unknown ignored)) ;although ignored aren't really vis
            (push name untracked-files)
          (unless (eq stat 'uptodate) (push name pending-files)))))
    ;; We really have to be careful about this -- elaborate warning message
    (let* ((untracked-warn (git--bold-face "untracked"))
           (pending-warn (concat "with " (git--bold-face "pending changes")))
           (status-warning
            (cond
             ((eq (length files) (length untracked-files)) untracked-warn)
             ((eq (length files) (length pending-files)) pending-warn)
             (t (git--join
                 (delq nil
                       (list (when untracked-files
                               (format "%d %s"
                                       (length untracked-files) untracked-warn))
                             (when pending-files
                               (format "%d %s"
                                       (length pending-files) pending-warn))))
                 ", "))))
           (status-warning-include (if (> (length status-warning) 0)
                                      (format " (%s)" status-warning)
                                     ""))
           (msg (if (eq 1 (length files))
                    (format "%s%s" (first files) status-warning-include)
                  (format "%s files%s" (length files) status-warning-include))))
      (unless (y-or-n-p (format "Really %s %s? "
                                (git--bold-face "delete")
                                msg))
        (error "Aborted deletion"))

      ;; do git rm -f on all the tracked files
      (let ((tracked-files
             (delq nil (mapcar #'(lambda(file)
                                   (unless (member file untracked-files) file))
                               files)))
            (num-deleted 0))
        (when tracked-files
          (apply #'git--exec-string "rm" "-f" "--" tracked-files))
        (incf num-deleted (length tracked-files))
        ;; Remove other files directly
        (unwind-protect
            (dolist (file untracked-files)
              (delete-file file)
              (incf num-deleted))
          (message "Deleted %d files" num-deleted)))))
                                        
  (revert-buffer))

(defun git--status-view-rename ()
  "Rename the selected files."

  (interactive)

  (let ((files (git--status-view-marked-or-file)))
    (dolist (src files)
      (let ((msg (format "%s '%s' to >> " (git--bold-face "Rename") src)))
        (git--mv src (file-relative-name (read-from-minibuffer msg src))))))

  (revert-buffer))
  
(defun git--status-view-add ()
  "Add the selected files."

  (interactive)
  (git--add (git--status-view-marked-or-file))
  (revert-buffer))

(defun git--status-view-add-ignore ()
  "Add the selected file to .gitignore"

  (interactive)

  (let ((files (git--status-view-marked-or-file)))
    (unless files (list (read-from-minibuffer "Add Ignore >> ")))

    (dolist (file files)
      (git-ignore file)))

  (revert-buffer))

;;-----------------------------------------------------------------------------

(provide 'git-status)
