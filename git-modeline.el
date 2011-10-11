;; Mode line decoration support, part of git-emacs.
;;
;; See git-emacs.el for license and versioning.
;; 
;; ref. "test-runner-image.el" posted at
;; "http://nschum.de/src/emacs/test-runner/"

(require 'git-emacs)

;; Modeline decoration customization
(defcustom git-state-modeline-decoration
  'git-state-decoration-large-dot
  "How to indicate the status of files in the modeline. The value
must be a function that takes a single arg: a symbol denoting file status,
e.g. 'unmerged. The return value of the function will be added at the beginning
of mode-line-format."
  :type '(choice (function-item :tag "Small colored dot"
                                git-state-decoration-small-dot)
                 (function-item :tag "Large colored dot"
                                git-state-decoration-large-dot)
                 (function-item :tag "Status letter"
                                git-state-decoration-letter)
                 (function-item :tag "Colored status letter"
                                git-state-decoration-colored-letter)
                 (const :tag "No decoration" nil)
                 (function :tag "Other"))
  :group 'git-emacs
)

(defun git--interpret-state-mode-color (stat)
  "Return a mode line status color appropriate for STAT (a state symbol)."
  (case stat
    ('modified "tomato"      )
    ('unknown  "gray"        )
    ('added    "blue"        )
    ('deleted  "red"         )
    ('unmerged "purple"      )
    ('uptodate "GreenYellow" )
    ('staged   "yellow"      )
    (t "red")))


;; Modeline decoration options
(defun git-state-decoration-small-dot(stat)
  (git--state-mark-modeline-dot
   (git--interpret-state-mode-color stat) stat
"/* XPM */
static char * data[] = {
\"14 7 3 1\",
\" 	c None\",
\"+	c #202020\",
\".	c %s\",
\"      +++     \",
\"     +...+    \",
\"    +.....+   \",
\"    +.....+   \",
\"    +.....+   \",
\"     +...+    \",
\"      +++     \"};"))

(defun git-state-decoration-large-dot(stat)
  (git--state-mark-modeline-dot
   (git--interpret-state-mode-color stat) stat
"/* XPM */
static char * data[] = {
\"18 13 3 1\",
\" 	c None\",
\"+	c #000000\",
\".	c %s\",
\"                  \",
\"       +++++      \",
\"      +.....+     \",
\"     +.......+    \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"    +.........+   \",
\"     +.......+    \",
\"      +.....+     \",
\"       +++++      \",
\"                  \"};"))

(defun git--interpret-state-mode-letter(stat)
   (case stat
     ('modified "M")
     ('unknown  "?")
     ('added    "A")
     ('deleted  "D")
     ('unmerged "!")
     ('uptodate "U")
     ('staged   "S")
     (t "")))

(defsubst git--state-mark-tooltip(stat)
  (format "File status in git: %s" stat))

(defun git-state-decoration-letter(stat)
  (propertize
   (concat (git--interpret-state-mode-letter stat) " ")
   'help-echo (git--state-mark-tooltip stat)))

(defun git-state-decoration-colored-letter(stat)
  (propertize
   (concat 
    (propertize 
     (git--interpret-state-mode-letter stat)
     'face (list ':foreground (git--interpret-state-mode-color stat)))
    " ")
   'help-echo (git--state-mark-tooltip stat)))

;; Modeline decoration implementation
(defvar git--state-mark-modeline t)     ; marker for our entry in mode-line-fmt

(defun git--state-mark-modeline-dot (color stat img)
  (propertize "    "
              'help-echo (git--state-mark-tooltip stat)
              'display
              `(image :type xpm
                      :data ,(format img color)
                      :ascent center)))

(defun git--state-decoration-dispatch(stat)
  (if (functionp git-state-modeline-decoration)
      (funcall git-state-modeline-decoration stat)))

(defun git--install-state-mark-modeline (stat)
  (push `(git--state-mark-modeline
          ,(git--state-decoration-dispatch stat))
        mode-line-format)
  )

(defun git--uninstall-state-mark-modeline ()
  (setq mode-line-format
        (delq nil (mapcar #'(lambda (mode)
                              (unless (eq (car-safe mode)
                                          'git--state-mark-modeline)
                                mode))
                   mode-line-format)))
  )

;; autoload entry point
(defun git--update-state-mark (stat)
  (git--uninstall-state-mark-modeline)
  (git--install-state-mark-modeline stat))

;; autoload entry point
(defun git--update-all-state-marks (&optional repo-or-filelist)
  "Updates the state marks of all the buffers visiting the REPO-OR-FILELIST,
which is a repository dir or a list of files. This is more efficient than
doing update--state-mark for each buffer."
  
  (git--uninstall-state-mark-modeline)
  (let ((buffers (git--find-buffers repo-or-filelist)))
    (when (and buffers git-state-modeline-decoration)
      ;; Use a hash table to find buffers after status-index and ls-files.
      ;; There could be many, and we're doing all these ops with no user
      ;; intervention. The hash table is filename -> (buffer . stat).
      (let ((file-index (make-hash-table :test #'equal :size (length buffers)))
            (default-directory
              (git--get-top-dir
                (if repo-or-filelist
                    (file-name-directory (first repo-or-filelist))
                  default-directory)))
            (all-relative-names nil))
        (dolist (buffer buffers)
          (let ((relative-name
                 (file-relative-name (buffer-file-name buffer)
                                     default-directory)))
            (puthash relative-name (cons buffer nil) file-index)
            (push relative-name all-relative-names)))
        ;; Execute status-index to find out the changed files
        (dolist (fi (apply #'git--status-index all-relative-names))
          (setcdr (gethash (git--fileinfo->name fi) file-index)
                  (git--fileinfo->stat fi)))
        ;; The remaining files are probably unchanged, do ls-files
        (let (remaining-files)
          (maphash #'(lambda (filename buffer-stat)
                       (unless (cdr buffer-stat)
                         (push filename remaining-files)))
                   file-index)
          (when remaining-files
            (dolist (fi (apply #'git--ls-files remaining-files))
              (setcdr (gethash (git--fileinfo->name fi) file-index)
                      (git--fileinfo->stat fi)))))
        ;; Now set all stats
        (maphash #'(lambda (filename buffer-stat)
                     (when (cdr buffer-stat)
                       (with-current-buffer (car buffer-stat)
                         (git--update-state-mark (cdr buffer-stat)))))
                 file-index)))))
      
;; example on state-modeline-mark
;; 
;;(git--install-state-mark-modeline 'modified)
;; (git--uninstall-state-mark-modeline)
;; (git--update-all-state-marks)

(provide 'git-modeline)
