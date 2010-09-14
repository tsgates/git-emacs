;; Git status buffer support, part of git-emacs.
;;
;; See git-emacs.el for license and versioning.

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
  (ewoc-set-hf
   git--status-view
   (concat (git--bold-face "Directory") " : " default-directory     "\n"
           (git--bold-face "Branch   ") " : "
           (or (git--current-branch) "<none>") "\n"
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
    :group 'git-emacs-faces))

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
                   (t (error "Unknown node type: %S" type)))))))
                  
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

  (let* ((current-node (ewoc-locate git--status-view))
         (current-fi (when current-node (ewoc-data current-node)))
         new-current)
    (git--please-wait "Reading git status" (git--status-new))
    (when current-fi
      (git--status-view-update-expand-tree (list current-fi) t)
      (setq new-current
            (git--status-map (ewoc-nth git--status-view 0)
                             (lambda (node data)
                               (or (string= (git--fileinfo->name data)
                                            (git--fileinfo->name current-fi))
                                   (git--fileinfo-lessp current-fi data))))))
    (if (not new-current)
        (git--status-view-first-line)
      (ewoc-goto-node git--status-view new-current)
      (move-to-column git--status-line-column))))
      

;; autoloaded entry point
(defun git-status (dir)
  "Launch git-status-mode on the specified directory. With a prefix
argument (C-u), always prompts."
  (interactive (list (git--get-top-dir-or-prompt
                      "Select directory: " (when current-prefix-arg t))))

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
  "Iterate over git--status-view nodes by using 'ewoc-next. Stops
when PRED returns t and returns that node. The predicate function
should get 'node and 'data arguments and it should return t or
nil.  If predicate returned nil we continue to scan, otherwise
stop and return the node."
  (let ((data nil)
        (cont t))

    (while (and node cont)
      (setq data (ewoc-data node))
      (setq cont (not (funcall pred node data)))
      (when cont (setq node (ewoc-next git--status-view node))))

    node))

(defun git--status-view-dumb-update-element (fi)
  "Add updated fileinfo FI to `git--status-view'. Slow, right now."
  
  (unless (git--status-map (ewoc-nth git--status-view 0)
                           #'(lambda (node data)
                               (when (git--fileinfo-lessp fi data)
                                 (ewoc-enter-before git--status-view node fi))))
    (ewoc-enter-last git--status-view fi)))

(defun git--status-view-update-state (fileinfos)
  "Update the state of the status view nodes corresponding to  FILEINFOS."

  (let ((hashed-info (make-hash-table :test 'equal :size (length fileinfos))))
    (dolist (fi fileinfos)
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

(defun git--status-view-update-expand-tree (fileinfos
                                            &optional dont-add-unknown-dirs)
  "Expand the tree nodes containing one of FILEINFOS, which must be sorted.
Does not add unknown files within the expanded dirs, that must be
an additional merge step. If DONT-ADD-UNKNOWN-DIRS is specified,
does not add additional directories to accommodate fileinfos that
are very deep (used when repositioning mark on refresh)."
  (let ((node (ewoc-nth git--status-view 0))
        (last-path-expanded '()))
    (dolist (fi fileinfos)
      (let* ((components (nbutlast
                          (split-string (git--fileinfo->name fi) "/")))
             (paths-to-expand components) ; advancing pointer inside components
             (matched-name nil) (cont-iteration t))

        (while (and paths-to-expand last-path-expanded
                    (string= (car paths-to-expand) (car last-path-expanded)))
          (setq paths-to-expand (cdr paths-to-expand))
          (setq last-path-expanded (cdr last-path-expanded)))

        (setq last-path-expanded components)
        
        ;; Paths inside root or an expanded path are already handled.
        (when paths-to-expand
          (let ((remaining-paths (cdr paths-to-expand)))
            (setcdr paths-to-expand nil)          ; splice off beginning path
            (setq matched-name (git--join components "/"))
            (setcdr paths-to-expand remaining-paths) ; relink last-path-expanded
            (setq paths-to-expand remaining-paths))
          
          (while cont-iteration
            (let ((data (ewoc-data node)) (found-it nil))
              (if (and (git--fileinfo-is-dir data)
                       (string= (git--fileinfo->name data) matched-name))
                  (progn
                    (unless (git--fileinfo->expanded data)
                      (git--expand-tree node t))
                    (setq found-it t))
                ;; Have we passed our insertion point? This can happen when
                ;; merging unknown files in unknown subdirs.
                (when (git--fileinfo-lessp fi data)
                  (if dont-add-unknown-dirs
                      (setq cont-iteration nil)
                    ;; Add the subdir we were looking for here. Don't advance.
                    (setq node (ewoc-enter-before
                                git--status-view node
                                (git--create-fileinfo
                                 matched-name 'tree nil nil nil 'unknown)))
                    ;; This new node is being expanded as we speak.
                    (setf (git--fileinfo->expanded (ewoc-data node)) t)
                    (setq found-it t))))
              (if found-it
                  ;; Do we need to expand even lower?
                  (if paths-to-expand
                    (progn
                      (setq matched-name
                            (concat matched-name "/"
                                    (car paths-to-expand)))
                      (setq paths-to-expand (cdr paths-to-expand))
                      ;; Continue iteration from next node
                      (setq node (ewoc-next git--status-view node)))
                    (setq cont-iteration nil))   ;; No, stop at this node.
                (setq node (ewoc-next git--status-view node))) ;; advance
              ))
          ;; This was very useful while debugging. Please leave it in.
          ;; (message "node: %s paths-to-expand %S matched-name %S fi-name %S" (when node (git--fileinfo->name (ewoc-data node))) paths-to-expand matched-name (git--fileinfo->name fi))
          )))))
                                          

(defun git--status-view-update ()
  "Update the state of all changed files."
  
  (let ((fileinfos (sort (git--status-index) #'git--fileinfo-lessp)))
    (git--status-view-update-expand-tree fileinfos)
    (git--status-view-update-state fileinfos)))

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

    ;; Use the file sorting to merge into the list.
    (let ((iter (ewoc-nth git--status-view 0)))
      (dolist (fi fileinfo)
        (git--status-add-size fi)
        ;; Find the lowest node that's larger, or enter at the end.
        (let (enter-before)
          (git--status-map
           iter
           (lambda (node data)
             (when (git--fileinfo-lessp fi data)
               (setq enter-before node))))
          (if enter-before
              (setq iter (ewoc-enter-before git--status-view enter-before fi))
            (setq iter (ewoc-enter-last git--status-view fi)))))))

  (git--status-refresh))

(defsubst git--status-delete (node)
  
  (let ((buffer-read-only nil)) 
    (ewoc-delete git--status-view node)))

(defun git--status-delete-after-regex (node regex)
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
  (define-key map "b" 'git-switch-branch)
  (define-key map "!" 'git--status-view-resolve-merge)
  (define-key map "." 'git-cmd)
  (define-key map "k" 'gitk)
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

  (define-key map "\C-m" 'git--status-view-open-or-expand)

  (setq git-status-mode-map map))

(easy-menu-define gitemacs-menu git-status-mode-map
  "Git"
  `("Git-Emacs"
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
    ["Switch to Branch..." git-switch-branch t]      
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
    ["Git Command" git-cmd t]
    ["GitK" gitk t]
    "----"
    ["Quit" git--status-view-quit t]))


;;-----------------------------------------------------------------------------
;; status view tree expanding
;;-----------------------------------------------------------------------------

(defun git--expand-tree (node &optional dont-add-unknown)
  "Expand 'node' in 'git--status-view'.  node->type should be 'tree. If
DONT-ADD-UNKOWN is true, does not add unknown files (if we're about to merge
them)."

  (let* ((data (ewoc-data node))
         (name (git--fileinfo->name data))
         (type (git--fileinfo->type data))
         (tree-sha1 (git--fileinfo->sha1 data))
         ;; We need some duplicate removal later on. Hashtable? not now.
         (known-subdirs '())
         (massage-fileinfo
          (lambda (fi)
            (let ((subfname (git--fileinfo->name fi)))
              (setf (git--fileinfo->name fi)
                    (git--concat-path-only name subfname))
              (when (git--fileinfo-is-dir fi) (push subfname known-subdirs)))
            fi))
         ;; The node may or may not be in git (e.g. unknown files onl)
         (fileinfos
          (sort (append
                 (when tree-sha1
                   (let ((fileinfos (git--ls-tree tree-sha1)))
                     (mapc massage-fileinfo fileinfos) ; modify them
                     fileinfos))
                 ;; Add unknown files, but just at the top-level. Note
                 ;; that git would give them to us *with* name, if we
                 ;; didn't cd.
                 (unless dont-add-unknown
                   (let ((unknown-files
                          (let ((default-directory
                                  (concat default-directory "/"
                                          (file-name-as-directory name))))
                            (git--ls-files "-o" "--exclude-standard")))
                         (filtered-unknown '()))
                     (dolist (fi unknown-files)
                       (let* ((subfname (git--fileinfo->name fi))
                              (components (split-string subfname "/" t)))
                         (if (eq 1 (length components))
                             (push (funcall massage-fileinfo fi)
                                   filtered-unknown)
                           ;; insert just the first component, if not seen
                           (unless (member (car components) known-subdirs)
                             (push
                              (funcall massage-fileinfo
                                       (git--create-fileinfo
                                        (car components)
                                        'tree nil nil nil 'unknown))
                              filtered-unknown)))))
                     filtered-unknown)))
                 #'git--fileinfo-lessp)))

    (unless (eq type 'tree) (error "type should be 'tree"))

    (unless (git--fileinfo->expanded data)

      (dolist (fi fileinfos)
        (git--status-add-size fi)
        (setq node (ewoc-enter-after git--status-view node fi)))
    
      (setf (git--fileinfo->expanded data) t))))

(defun git--shrink-tree (node)
  "Shrink 'node' in 'git--status-view'. node->type should be 'tree"
  (let* ((data (ewoc-data node))
         (name (git--fileinfo->name data)))
    (unless (git--fileinfo-is-dir data) (error "type should be 'tree"))
    (when (git--fileinfo->expanded data)
      ;; make regexp "node->name/"
      (git--status-delete-after-regex
       (ewoc-next git--status-view node)
       (concat "^" (regexp-quote (file-name-as-directory name))))
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
  "Move forward by N lines in the status view."

  (interactive "p")
  
  (let ((dir (/ n (abs n))))
    (forward-line n)

    (while (or (looking-at "^[\n\t ]+$")
               (looking-at "^[^ ]"))
      (forward-line dir)))

  (move-to-column git--status-line-column))

(defun git--status-view-first-line ()
  "Move to the first item in the status view."
  
  (interactive)
  (goto-char (point-min))
  (git--status-view-forward-line 1))

(defun git--status-view-last-line ()
  "Move to the last item in the status view."
  
  (interactive)
  (goto-char (point-max))
  (git--status-view-forward-line -1))

(defun git--forward-meaningful-line (move)
  "Call MOVE until we end up on a meaningful line (i.e. one with updates)."

  (let ((start-node (ewoc-locate git--status-view)))
    (funcall move 1)

    (while (and (eq 'uptodate
                    (git--fileinfo->stat (ewoc-data (ewoc-locate git--status-view))))
                (not (eq start-node (ewoc-locate git--status-view))))
      (funcall move 1))))

(defun git--status-view-next-line (&optional n)
  "Move to the next line in the status view."
  (interactive "p")
  (if (eql (ewoc-locate git--status-view)
           (ewoc-nth git--status-view -1))
      (git--status-view-first-line)
    (git--status-view-forward-line 1)))

(defun git--status-view-next-meaningful-line ()
  "Move to the next meaningful line in the status view."
  (interactive)
  (git--forward-meaningful-line 'git--status-view-next-line))

(defun git--status-view-prev-line (&optional n)
  "Move to the previous line in the status view."
  (interactive "p")
  (if (eql (ewoc-locate git--status-view)
           (ewoc-nth git--status-view 0))
      (git--status-view-last-line)
    (git--status-view-forward-line -1)))

(defun git--status-view-prev-meaningful-line ()
  "Move to the previous meaningful line in the status view."
  (interactive)
  (git--forward-meaningful-line 'git--status-view-prev-line))

;;-----------------------------------------------------------------------------
;; Marking.
;;-----------------------------------------------------------------------------

(defun git--mark-line (marked)
  "Sets the mark flag of the current line to MARK. Updates the view."

  (let ((node (ewoc-locate git--status-view)))
    (setf (git--fileinfo->marked (ewoc-data node)) marked)
    (ewoc-invalidate git--status-view node)))

(defun git--status-view-mark-and-next ()
  "Mark and go to the next line."
  (interactive)
  (git--mark-line t)
  (git--status-view-next-line))

(defun git--status-view-unmark-and-next ()
  "Unmark and go to the next line."
  (interactive)
  (git--mark-line nil)
  (git--status-view-next-line))

(defun git--toggle-line ()
  "Toggles the marked state of the current line."
  (let* ((node (ewoc-locate git--status-view))
         (data (ewoc-data node))
         (mark (git--fileinfo->marked data)))
    (setf (git--fileinfo->marked data) (not mark))
    (ewoc-invalidate git--status-view node)))

(defun git--status-view-toggle-and-next ()
  "Toggle the marked state of the current line and move to the next."
  (interactive)
  (git--toggle-line)
  (git--status-view-next-line))

;;-----------------------------------------------------------------------------
;; Commands
;;-----------------------------------------------------------------------------

(defun git--status-view-quit ()
  "Quit the git status buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun git--status-view-refresh ()
  "Refresh git status buffer."
  (interactive)
  (git--please-wait "Reading git status" (revert-buffer)))

(defun git--status-view-mark-reg (reg)
  "Prompt for a regular expression, mark the files that match."

  (interactive "sRegexp >> ")
  (ewoc-collect git--status-view
                #'(lambda (data)
                    (when (string-match reg (git--fileinfo->name data))
                      (setf (git--fileinfo->marked data) t))))

  (ewoc-refresh git--status-view)
  (git--status-view-first-line)
  (git--status-view-next-meaningful-line))

(defun git--status-view-summary ()
  "Pops up an 'occur' summary of the changed files."
  (interactive)
  (occur "[\t* ]+\\(Deleted\\|Modified\\|Unknown\\|Added\\|Staged\\)")
  (message "Move with 'next-error and 'previous-error"))

;;-----------------------------------------------------------------------------
;; Operations on single files.
;;-----------------------------------------------------------------------------

(defsubst git--status-view-select-filename ()
  "Return the filename of the current status view item."
  (let ((filename (git--fileinfo->name (ewoc-data (ewoc-locate git--status-view)))))
    (when (file-directory-p filename)
      (error "Not a file"))
    filename))

(defsubst git--status-view-select-type ()
  "Return the type of the current view item."
  (git--fileinfo->type (ewoc-data (ewoc-locate git--status-view))))

(defun git--status-view-view-file ()
  "View the selected file."
  (interactive)
  (view-file (git--status-view-select-filename)))

(defun git--status-view-open-file ()
  "Open the selected file."
  (interactive)
  (find-file (git--status-view-select-filename)))

(defun git--status-view-descend-submodule ()
  "Opens a status view on the selected submodule."
  (let ((submodule-dir (git--fileinfo->name
                        (ewoc-data (ewoc-locate git--status-view)))))
    (git-status submodule-dir)
    (message "Viewing submodule \"%s\", close buffer to return"
             submodule-dir)))

(defun git--status-view-resolve-merge ()
  "Resolve merge conflicts in the currently selected file (must be unmerged)."
  (interactive)
  (let ((file (git--status-view-select-filename)))
    (if (eq 'unmerged (git--status-file file))
        (progn
          (find-file (git--status-view-select-filename))
          (git--resolve-merge-buffer))
      (error "File is not unmerged"))))

(defun git--status-view-open-or-expand ()
  "Open or expands the current file / directory / submodule."
  (interactive)
  (case (git--status-view-select-type)
    ('tree (git--status-view-expand-tree-toggle))
    ('blob (git--status-view-open-file))
    ('commit (git--status-view-descend-submodule))
    (t (error "Not supported type"))))

(defun git--status-view-blame ()
  "Open the current file and enable blame mode."
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
  "Rename the selected file(s)."
  (interactive)
  (let ((files (git--status-view-marked-or-file)))
    (dolist (src files)
      (let ((msg (format "%s '%s' to >> " (git--bold-face "Rename") src)))
        (git--mv src (file-relative-name (read-from-minibuffer msg src))))))
  (revert-buffer))
  
(defun git--status-view-add ()
  "Add the selected file(s) to the index."
  (interactive)
  (git--add (git--status-view-marked-or-file))
  (revert-buffer))

(defun git--status-view-add-ignore ()
  "Add the selected file(s) to .gitignore"
  (interactive)
  (let ((files (git--status-view-marked-or-file)))
    (dolist (file files)
      (git-ignore file)))
  (revert-buffer))

;;-----------------------------------------------------------------------------

(provide 'git-status)
