;; 
;; ref. "test-runner-image.el" posted at
;; "http://nschum.de/src/emacs/test-runner/"
;;

(require 'git-emacs)

;; Modeline decoration customization
(defcustom git-state-modeline-decoration
  (if window-system 'git-state-decoration-large-dot
                    'git-state-decoration-colored-letter)
  "How to indicate the status of files in the modeline. The value
must be a function that takes a single arg: a symbol denoting file status,
e.g. 'unmerged. The return value of the function will be added at the beginning
of mode-line-format."
  :type '(radio (function-item :tag "Small colored dot"
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

;; Modeline decoration options
(defun git-state-decoration-small-dot(stat)
  (git--state-mark-modeline-dot
   (git--interpret-state-mode-color stat)
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
   (git--interpret-state-mode-color stat)
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

(defsubst git--interpret-state-mode-letter(stat)
   (case stat
     ('modified "M")
     ('unknown  "?")
     ('added    "A")
     ('deleted  "D")
     ('unmerged "!")
     ('uptodate "U")
     ('staged   "S")
     (t "")))

(defun git-state-decoration-letter(stat)
  (propertize
   (concat (git--interpret-state-mode-letter stat) " ")
   'help-echo 'git--state-mark-tooltip))

(defun git-state-decoration-colored-letter(stat)
  (propertize
   (concat 
    (propertize 
     (git--interpret-state-mode-letter stat)
     'face (list ':foreground (git--interpret-state-mode-color stat)))
    " ")
   'help-echo 'git--state-mark-tooltip))

;; Modeline decoration implementation
(defvar git--state-mark-modeline t)     ; marker for our entry in mode-line-fmt
(defvar git--state-mark-tooltip nil)    ; modeline tooltip display

(defun git--state-mark-modeline-dot (color img)
  (propertize "    "
              'help-echo 'git--state-mark-tooltip
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
  (force-mode-line-update t))

(defun git--uninstall-state-mark-modeline ()
  (setq mode-line-format
        (delq nil (mapcar #'(lambda (mode)
                              (unless (eq (car-safe mode)
                                          'git--state-mark-modeline)
                                mode))
                   mode-line-format)))
  (force-mode-line-update t))

(defun git--update-state-mark-tooltip (tooltip)
  (setq git--state-mark-tooltip tooltip))

;; autoload entry point
(defun git--update-state-mark (stat)
  (git--uninstall-state-mark-modeline)
  (git--install-state-mark-modeline stat))

;; 
;; example on state-modeline-mark
;; 
;;(git--install-state-mark-modeline 'modified)
;; (git--uninstall-state-mark-modeline)
;; (setq git--state-mark-tooltip "testsetset")

(provide 'git-modeline)

