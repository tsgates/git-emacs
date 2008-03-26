;; 
;; ref. "test-runner-image.el" posted at
;; "http://nschum.de/src/emacs/test-runner/"
;;

(provide 'git-modeline)

(defvar git--state-mark-modeline t)     ; modeline mark display or not
(defvar git--state-mark-tooltip nil)    ; modeline tooltip display

(defun git--state-mark-modeline-dot (color)
  (propertize "    "
              'help-echo 'git--state-mark-tooltip
              'display
              `(image :type xpm
                      :data ,(format "/* XPM */
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
\"                  \"};"
                                     color)
                      :ascent center)))

(defun git--install-state-mark-modeline (color)
  (push `(git--state-mark-modeline
          ,(git--state-mark-modeline-dot color))
        mode-line-format)
  (force-mode-line-update t))

(defun git--uninstall-state-mark-modeline ()
  (setq mode-line-format
        (remove-if #'(lambda (mode) (eq (car-safe mode)
                                        'git--state-mark-modeline))
                   mode-line-format))
  (force-mode-line-update t))

(defun git--update-state-mark-tooltip (tooltip)
  (setq git--state-mark-tooltip tooltip))

(defun git--update-state-mark (color)
  (git--uninstall-state-mark-modeline)
  (git--install-state-mark-modeline color))

;; 
;; example on state-modeline-mark
;; 
;; (git--install-state-mark-modeline "red")
;; (git--uninstall-state-mark-modeline)
;; (setq git--state-mark-tooltip "testsetset")