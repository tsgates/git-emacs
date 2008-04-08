;;-----------------------------------------------------------------------------

;; commit reel index viewer
;; 
;; 1) get commits topologically ordered
;; 2) diff each commits
;;

(defsubst git--reel-buffer-name (dir)
  (format "*git-reel on %s*" (expand-file-name dir)))

(defsubst git--create-reel-buffer (dir)
  (let* ((reel-buffer-name (git--reel-buffer-name dir))
         (reel-buffer (get-buffer reel-buffer-name)))
    (or reel-buffer (get-buffer-create reel-buffer-name))))

(defun git-reel (dir)
  "Launch git-status mode at the directory if it is under 'git'"

  (interactive "DSelect directory: ")

  ;; (setq dir (git--get-top-dir dir))
  (if (file-directory-p (git--expand-to-repository-dir dir))
      (save-excursion
        (switch-to-buffer (git--create-reel-buffer dir))
        (setq default-directory dir)
        (git--reel-mode)
        (git--reel-new))
    (message "%s is not a git working tree." dir)))

(defun git--reel-new ()
  (let ((buffer-read-only nil)) (erase-buffer))

  (dolist (info (git--get-reel))
    (ewoc-enter-last git--reel-view info))
  (ewoc-refresh git--reel-view))

(defun git--render-type (info)
  (let ((type (git--reel-info->type info)))
    (propertize (symbol-name type) 'face
                (case type
                  ('tree 'git--mark-tree-face)
                  ('blob 'git--mark-blob-face)
                  ('commit 'git--bold-face)
                  (t 'git--ignored-face)))))

(defun git--render-etc (info)
  (let ((etc (git--reel-info->etc info)))
    (if (stringp etc) etc "")))

(defun git--render-reel (info)
  (insert (format "  % 11d % 7s  %11s %7s %s"
                  (git--reel-info->offset info)
                  (propertize (int-to-string (git--reel-info->size info)) 'face 'git--bold-face)
                  (concat (substring (git--reel-info->sha1 info) 0 8) "...")
                  (git--render-type info)
                  (git--render-etc info))))

(defvar git--reel-mode-map nil)

(unless git--reel-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)


    (setq git--reel-mode-map map)))

(defun git--reel-mode ()
  (kill-all-local-variables)
  (buffer-disable-undo)

  (use-local-map git--reel-mode-map)

  ;; set major mode
  (setq mode-name "git reel")
  (setq major-mode 'git-reel-mode)

  (setq buffer-read-only t)
  (setq header-line-format
        (format "V  %11s %7s  %-11s %7s %s" "offset" "size" "sha1" "type" "info"))

  (set (make-local-variable 'git--reel-view)
       (ewoc-create 'git--render-reel "" "")))

(defstruct (git--commit-info
            (:copier nil)
            (:constructor git--create-commit-info (commit tree parents date))
            (:conc-name git--commit-info->))
  commit                                ; commit sha1
  tree                                  ; tree sha1
  parents                               ; list of parents
  date)                                 ; date of commit

(defvar git--reel-index-buffer "*git-reel-index*")

(defun git--topo-commits ()
  "Sort commits following RFC rule"
  
  (let ((buffer (get-buffer-create git--reel-index-buffer))
        (regexp (concat "commit " git--reg-sha1 "\n"   ; matched-1
                        "date \\([0-9]+\\)\n"          ; matched-2
                        "tree " git--reg-sha1 "\n"     ; matched-3
                        "parents \\([0-9a-f ]*\\)\n")) ; matched-4
        (dir default-directory)
        (commits nil)
        (sorted nil)
        (roots nil))

    (with-current-buffer buffer
      (setq default-directory dir)
      (let ((buffer-read-only nil)) (erase-buffer))

      ;; get topologically sorted commit object list
      (git--rev-list "--topo-order"
                     "--pretty=format:date %ct%ntree %T%nparents %P"
                     "HEAD")

      ;; parse commit/tree/parents -> commits
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let ((commit (match-string 1)) 
              (date (string-to-int (match-string 2)))
              (tree (match-string 3))
              (parents (split-string (match-string 4))))

          ;; FIXME : for each root, sort, but already sorted
          ;; if no parent -> one of roots
          (unless parents (push commit roots))
          
          (push (git--create-commit-info commit tree parents date) commits))))

    ;; sort by commit time not restricting topological order
    (dolist (commit commits sorted)

      ;; 
      ;; to find the position of c2
      ;;
      ;; ^ c4 <- sorted
      ;; | c3 <- prev-commit
      ;; | c1 <- iter-commit
      ;;
      
      (let ((parents (git--commit-info->parents commit))
            (prev-commit nil)
            (iter-commit sorted))

        ;; insert commit
        (while (not (or (null iter-commit)                                            ; if there is nothing to be remained
                        (member (git--commit-info->commit (car iter-commit)) parents) ; if cur-commit is my parent
                        (> (git--commit-info->date commit)                            ; or commit time is earlier
                           (git--commit-info->date (car iter-commit)))))

          ;; TODO if commit dates are same -> check sha1
          
          (push (car iter-commit) prev-commit)
          (pop iter-commit))
          
        (setq sorted (nconc (reverse prev-commit) (list commit) iter-commit))))))

;; display topologically sorted commits to buffer
(defun git--topo-commits-only ()
  (let ((buffer (get-buffer-create "*topo*")))
    (switch-to-buffer buffer)
    
    (let ((commits (git--topo-commits)))
      (message "%s" (length commits))

      (dolist (commit commits)
        (insert (format "C:%s\nP:%s\nT:%s\n"
                        (git--commit-info->commit commit)
                        (git--commit-info->parents commit)
                        (git--commit-info->date commit)))))))

(defun git--get-object-size (sha1)
  (string-to-int (git--exec-string "cat-file" "-s" sha1)))

(defun git--get-reel ()
  ;; for each commits
  (let ((prev-commit-tree nil)          ; for iterating
        (reels nil)                     ; reels result
        (offset 0)                      ; offset of whole block
        (commit-cnt 0)                  ; number of processed commit
        (commit-len 0)                  ; whole commits
        (commits (git--topo-commits)))  ; get topologically sorted commits

    (setq commit-len (length commits))

    ;; check see if huge to process
    (when (and (< 100 commit-len)
               (not (y-or-n-p (format "Want to process huge commits(%d)?" commit-len))))
      (error "Ok, aborted"))

    ;; for each commit
    ;;  1) construct diff between previous commit
    ;;    1-1) for each different objects, process offset/size
    ;;  2) add tree object of the commit
    ;;  3) add commit object itself
    
    (dolist (commit commits)
      (let* ((size 0)
             (cur-commit-tree (git--commit-info->tree commit))
             (cur-commit-sha1 (git--commit-info->commit commit))
             ;; 1) construct diff between previous commit
             (tree (git--diff-tree-as-reel prev-commit-tree cur-commit-tree)))

        (message "%s/%s (%f%%) Complete"
                 commit-cnt commit-len (/ (* commit-cnt 100) commit-len))

        ;; 1-1) for each different objects, process offset/size
        (dolist (item tree)
          ;; TODO : lessened the over-burdened forks to get a size of the object
          ;;        make a separate c utility
          (setq size (git--get-object-size (git--reel-info->sha1 item)))

          ;; update reel info
          (setf (git--reel-info->size item) size)
          (setf (git--reel-info->offset item) offset)

          ;; update offset
          (setq offset (+ offset size)))

        (setq tree (reverse tree))
        (setq reels (nconc tree reels))

        ;; 2) add tree object of the commit
        (setq size (git--get-object-size cur-commit-tree))
        (add-to-list 'reels (git--create-reel-info cur-commit-tree
                                                   'tree
                                                   ""
                                                   offset
                                                   size))
        (setq offset (+ offset size))

        ;; 3) add commit object itself
        (setq size (git--get-object-size cur-commit-sha1))
        (add-to-list 'reels (git--create-reel-info cur-commit-sha1
                                                   'commit
                                                   (int-to-string (git--commit-info->date commit))
                                                   offset
                                                   size))
        (setq offset (+ offset size))

        ;; save previous commit
        (setq prev-commit-tree cur-commit-tree)

        ;; commit process count
        (incf commit-cnt)))

    (message "100%% Complete")

    reels))

(defun git--diff-tree (&rest args)
  (apply #'git--exec-buffer "diff-tree" "-z" args))

(defstruct (git--reel-info
            (:copier nil)
            (:constructor git--create-reel-info (sha1 type &optional etc offset size))
            (:conc-name git--reel-info->))
  sha1                                  ; object's sha1 
  type                                  ; type in {tree,blob,commit}
  offset                                ; offset in block
  size                                  ; size of current object
  etc)                                  ; extra information (ex, blob -> filename, commit -> date)

;; diff in tree2's perspective
(defun git--reel-diff-tree (tree1 tree2)
  "get commit reel from difference between tree1 and tree2 by executing git-diff-tree"

  (let ((reels nil)
        (regexp (git--build-reg ":"
                                git--reg-perm git--reg-space     ; matched-1
                                git--reg-perm git--reg-space     ; matched-2
                                git--reg-sha1 git--reg-space     ; matched-3
                                git--reg-sha1 git--reg-space     ; matched-4
                                git--reg-status git--reg-eof     ; matched-5
                                git--reg-file)))                 ; matched-6

    (with-temp-buffer
      (git--diff-tree "-t" tree1 tree2)
      (goto-char (point-min))

      (while (re-search-forward regexp nil t)
        (let ((sha1 (match-string 4))
              (file (match-string 6))
              (type (match-string 2)))

          ;; check if type is tree
          (setq type (if (string= type "040000") 'tree 'blob))
          (push (git--create-reel-info sha1 type file) reels))))

    (setq reels (git--sort-reel reels))))

(defsubst git--sort-reel (reels)

  ;; sort reversely by directory first, and then by sha1
  (sort reels (lambda (r1 r2)
                 (let ((r1-name (git--reel-info->etc r1))
                       (r2-name (git--reel-info->etc r2)))
                   (if (string= (file-name-directory r1-name) (file-name-directory r2-name))
                       (not (string< (git--reel-info->sha1 r1) (git--reel-info->sha1 r2)))
                     (not (string< r1-name r2-name)))))))

(defsubst git--sort-reel-by-sha1 (reels)

  ;; sort by sha1
  (sort reels (lambda (r1 r2) (string< (git--reel-info->sha1 r1)
                                       (git--reel-info->sha1 r2)))))

;; make ls-tree to reel
(defun git--reel-from-tree (tree prefix)
  "get commit reel by executing ls-tree"

  (let (reels sorted)
    (with-temp-buffer
      (git--exec-buffer "ls-tree" "-t" "-z" tree)
      (goto-char (point-min))

      (let ((regexp (git--build-reg git--reg-perm    ; matched-1
                                    git--reg-space
                                    git--reg-type    ; matched-2
                                    git--reg-space
                                    git--reg-sha1    ; matched-3
                                    git--reg-tab
                                    git--reg-file))) ; matched-4

        ;; for each matched item
        (while (re-search-forward regexp nil t)
          (let ((type (git--to-type-sym (match-string 2)))
                (file (concat prefix (match-string 4)))
                (sha1 (match-string 3)))

            (push (git--create-reel-info sha1 type file) reels)))))

    ;; sort by sha1
    (setq reels (git--sort-reel-by-sha1 reels))
    
    ;; newly construct the reels
    (dolist (reel reels sorted)
      (push reel sorted)

      (let ((type (git--reel-info->type reel))
            (sha1 (git--reel-info->sha1 reel))
            (file (git--reel-info->etc reel)))
        
        (when (eq type 'tree)
          (setq sorted (nconc (git--reel-from-tree sha1 (concat file "/")) sorted)))))))

(defun git--diff-tree-as-reel (tree1 tree2)
  "get commit reel from tree1 to tree2, if one of them is null, get from ls-tree"

  (if (and (stringp tree1) (stringp tree2))
      (git--reel-diff-tree tree1 tree2)
    (cond ((stringp tree1) (git--reel-from-tree tree1 ""))
          ((stringp tree2) (git--reel-from-tree tree2 ""))
          (t nil))))

(provide 'git-reel)