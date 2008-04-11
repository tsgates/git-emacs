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

;; 
;; git-reel -> git--reel-mode
;;          -> git--reel-new
;; 
(defun git-reel (dir)
  "Launch git-status mode at the directory if it is under 'git'"

  (interactive "DSelect directory: ")

  ;; under git
  (if (file-directory-p (git--expand-to-repository-dir dir))
      (save-excursion
        (switch-to-buffer (git--create-reel-buffer dir))
        (setq default-directory dir)
        (git--reel-mode)
        (git--reel-new)
        (goto-char (point-min)))
    (message "%s is not a git working tree." dir)))

(defun git--reel-new ()
  "Newly create the ewoc view"
  
  (let ((buffer-read-only nil)) (erase-buffer))

  (dolist (info (git--get-reel))
    (ewoc-enter-last git--reel-view info))
  (ewoc-refresh git--reel-view))

(defun git--render-type (info)
  "Render the type column of each item in ewoc list"

  (let ((type (git--reel-info->type info)))
    (propertize (symbol-name type) 'face
                (case type
                  ('tree 'git--mark-tree-face)
                  ('blob 'git--mark-blob-face)
                  ('commit 'git--bold-face)
                  (t 'git--ignored-face)))))

(defun git--render-etc (info)
  "Render the extra information column of each item"
  
  (let ((etc (git--reel-info->etc info)))
    (if (stringp etc) etc "")))

;; 
;; git--render-reel -> git--reel-type
;;                  -> git--reel-etc
;;                  
(defun git--render-reel (info)
  "Render the each item"

  (insert (format "  % 11d % 7s  %11s %7s %s"
                  (git--reel-info->offset info)
                  (propertize (int-to-string (git--reel-info->size info)) 'face 'git--bold-face)
                  (concat (substring (git--reel-info->sha1 info) 0 8) "...")
                  (git--render-type info)
                  (git--render-etc info))))

;; mode map of git-reel, but, remain it on TODO list
(defvar git--reel-mode-map nil)

;; TODO : add convenient key bindings
(unless git--reel-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)

    (define-key map "r" 'git-reel-play)
    (define-key map "q" '(lambda () (interactive) (kill-buffer nil)))
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)

    (setq git--reel-mode-map map)))

(defun git--reel-mode ()
  "Git reel commit mode"
  
  (kill-all-local-variables)
  (buffer-disable-undo)

  (use-local-map git--reel-mode-map)

  ;; set major mode
  (setq mode-name "git reel")
  (setq major-mode 'git-reel-mode)

  (setq buffer-read-only t)

  ;; set ewoc
  (setq header-line-format
        (format "V  %11s %7s  %-11s %7s %s" "offset" "size" "sha1" "type" "info"))

  (set (make-local-variable 'git--reel-view)
       (ewoc-create 'git--render-reel "" "")))

;; commit info from the git-rev-list
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
    ;;
    ;; sha1 of commit(month/day)
    ;;
    ;; C0(4/3) -> C1(4/4) -> C2(4/6) -> C3(4/7)->C4(4/8)
    ;;          \-> C5(4/5)->C6(4/6)
    ;;
    ;; maybe the result of –topo-order option
    ;; C0/C1/C2/C3/C4/C5/C6
    ;;
    ;; reflecting the date of commitment and sha1
    ;; C0/C1/C5/C2/C6/C3/C4       (C1(4/4) < C5(4/5) and C2 < C6)

    ;; so,
    ;; 
    ;; 1. from c0 to c6 (simple)
    ;; [C0], [C0/C1], ... , [C0/C2/C3/C4], 
    ;;
    ;; 2. for C5
    ;; [C0/C1/C2/C3/C4]
    ;;     |  |   | +-> C4(4/8) > C5(4/5)
    ;;     |  |   +-> C3(4/7) > C5(4/5)
    ;;     |  +-> C2(4/6) > C5(4/5)
    ;;     +-> C1(4/4) < C5(4/5)
    ;; 
    ;; => [C0/C1/C5/C2/C3/C4]
    ;;
    ;; 3. for C6
    ;; [C0/C1/C5/C2/C3/C4]
    ;;           |
    ;;           +-> C2 < C6 (sha1)
    ;; 
    ;; => [C0/C1/C5/C2/C6/C3/C4]

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

        ;; insert commit to proper position
        ;; 
        ;;   1. if there is nothing to be remained
        ;;   2. if cur-commit is my parent
        ;;   3. or commit time is earlier
        
        (while (not (or (null iter-commit)                                            ; 1
                        (member (git--commit-info->commit (car iter-commit)) parents) ; 2
                        (> (git--commit-info->date commit)                            ; 3
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

(defvar git--commit-reel-lookup nil)
(defvar git--commit-reel-offset nil)

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

    ;; FIXME : to right order (need a discussion)

    (dolist (commit (reverse commits))
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

    ;; init commit lookup table in order to search block
    (setq git--commit-reel-lookup nil)
    (setq git--commit-reel-offset offset)
    
    ;; flipping the offset & making commit lookup table
    (dolist (reel reels)
      (setf (git--reel-info->offset reel)
            (- offset
               (git--reel-info->offset reel)
               (git--reel-info->size reel)))

      ;; TODO : better to binary search on whole reel or commit list
      (when (eq (git--reel-info->type reel) 'commit)
        (push (cons (git--reel-info->offset reel)
                    (git--reel-info->sha1 reel))
              git--commit-reel-lookup)))

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

;; TODO : able to optimize, extensively
(defun git-reel-play (block-size block-num)
  (interactive "nBlock Size >> \nnBlock Number >> ")

  (let* ((offset git--commit-reel-offset)
         (num-of-blocks (+ (/ offset block-size)
                           (if (= (% offset block-size) 0) 0 1)))
         (reel-offset (* block-num block-size))
         (commit-reel nil)              ; the commit starting in the block
         (excluded-commit nil))         ; the last commit out of block

    (when (or (< num-of-blocks block-num)
              (< offset reel-offset))
      (error (format "Wrong block size(%d) and num(%d)" block-size block-num)))

    ;; search the proper commit reel
    (dolist (commit (reverse git--commit-reel-lookup))
      (let ((block-beg reel-offset)
            (block-end (+ reel-offset block-size))
            (block-iter (car commit)))

        (if (and (<= block-beg block-iter) (< block-iter block-end))
            (add-to-list 'commit-reel commit)
          (when (and (<= block-end block-iter) (null excluded-commit))
            (setq excluded-commit commit)))))

    (message "git-rev-list --objects-edge %s %s"
             (mapconcat 'cdr commit-reel " ")
             (if excluded-commit
                 (concat "^" (cdr excluded-commit))
               ""))))
  
(provide 'git-reel)