<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN">
<!-- Created by htmlize-1.34 in css mode. -->
<html>
  <head>
    <title>git-blame.el</title>
    <style type="text/css">
    <!--
      body {
        color: #000000;
        background-color: #ffffff;
      }
      .builtin {
        /* font-lock-builtin-face */
        color: #da70d6;
      }
      .comment {
        /* font-lock-comment-face */
        color: #b22222;
      }
      .comment-delimiter {
        /* font-lock-comment-delimiter-face */
        color: #b22222;
      }
      .constant {
        /* font-lock-constant-face */
        color: #5f9ea0;
      }
      .doc {
        /* font-lock-doc-face */
        color: #bc8f8f;
      }
      .function-name {
        /* font-lock-function-name-face */
        color: #0000ff;
      }
      .keyword {
        /* font-lock-keyword-face */
        color: #a020f0;
      }
      .regexp-grouping-backslash {
        /* font-lock-regexp-grouping-backslash */
        font-weight: bold;
      }
      .regexp-grouping-construct {
        /* font-lock-regexp-grouping-construct */
        font-weight: bold;
      }
      .string {
        /* font-lock-string-face */
        color: #bc8f8f;
      }
      .type {
        /* font-lock-type-face */
        color: #228b22;
      }
      .variable-name {
        /* font-lock-variable-name-face */
        color: #b8860b;
      }
      .warning {
        /* font-lock-warning-face */
        color: #ff0000;
        font-weight: bold;
      }

      a {
        color: inherit;
        background-color: inherit;
        font: inherit;
        text-decoration: inherit;
      }
      a:hover {
        text-decoration: underline;
      }
    -->
    </style>
  </head>
  <body>
    <pre>
<span class="comment-delimiter">;;; </span><span class="comment">git-blame.el --- Minor mode for incremental blame for Git  -*- coding: utf-8 -*-
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">Copyright (C) 2007  David K&#229;gedal
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">Authors:    David K&#229;gedal &lt;<a href="mailto:davidk&#64;lysator.liu.se">davidk&#64;lysator.liu.se</a>&gt;
</span><span class="comment-delimiter">;; </span><span class="comment">Created:    31 Jan 2007
</span><span class="comment-delimiter">;; </span><span class="comment">Message-ID: &lt;<a href="mailto:87iren2vqx.fsf&#64;morpheus.local">87iren2vqx.fsf&#64;morpheus.local</a>&gt;
</span><span class="comment-delimiter">;; </span><span class="comment">License:    GPL
</span><span class="comment-delimiter">;; </span><span class="comment">Keywords:   git, version control, release management
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">Compatibility: Emacs21, Emacs22 and EmacsCVS
</span><span class="comment-delimiter">;;                </span><span class="comment">Git 1.5 and up
</span>
<span class="comment-delimiter">;; </span><span class="comment">This file is *NOT* part of GNU Emacs.
</span><span class="comment-delimiter">;; </span><span class="comment">This file is distributed under the same terms as GNU Emacs.
</span>
<span class="comment-delimiter">;; </span><span class="comment">This program is free software; you can redistribute it and/or
</span><span class="comment-delimiter">;; </span><span class="comment">modify it under the terms of the GNU General Public License as
</span><span class="comment-delimiter">;; </span><span class="comment">published by the Free Software Foundation; either version 2 of
</span><span class="comment-delimiter">;; </span><span class="comment">the License, or (at your option) any later version.
</span>
<span class="comment-delimiter">;; </span><span class="comment">This program is distributed in the hope that it will be
</span><span class="comment-delimiter">;; </span><span class="comment">useful, but WITHOUT ANY WARRANTY; without even the implied
</span><span class="comment-delimiter">;; </span><span class="comment">warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
</span><span class="comment-delimiter">;; </span><span class="comment">PURPOSE.  See the GNU General Public License for more details.
</span>
<span class="comment-delimiter">;; </span><span class="comment">You should have received a copy of the GNU General Public
</span><span class="comment-delimiter">;; </span><span class="comment">License along with this program; if not, write to the Free
</span><span class="comment-delimiter">;; </span><span class="comment">Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
</span><span class="comment-delimiter">;; </span><span class="comment">MA 02111-1307 USA
</span>
<span class="comment-delimiter">;; </span><span class="comment">http://www.fsf.org/copyleft/gpl.html
</span>

<span class="comment-delimiter">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span><span class="comment">
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;;; </span><span class="comment">Commentary:
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">Here is an Emacs implementation of incremental git-blame.  When you
</span><span class="comment-delimiter">;; </span><span class="comment">turn it on while viewing a file, the editor buffer will be updated by
</span><span class="comment-delimiter">;; </span><span class="comment">setting the background of individual lines to a color that reflects
</span><span class="comment-delimiter">;; </span><span class="comment">which commit it comes from.  And when you move around the buffer, a
</span><span class="comment-delimiter">;; </span><span class="comment">one-line summary will be shown in the echo area.
</span>
<span class="comment-delimiter">;;; </span><span class="comment">Installation:
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">To use this package, put it somewhere in `</span><span class="comment"><span class="constant">load-path</span></span><span class="comment">' (or add
</span><span class="comment-delimiter">;; </span><span class="comment">directory with git-blame.el to `</span><span class="comment"><span class="constant">load-path</span></span><span class="comment">'), and add the following
</span><span class="comment-delimiter">;; </span><span class="comment">line to your .emacs:
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;;    </span><span class="comment">(require 'git-blame)
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">If you do not want to load this package before it is necessary, you
</span><span class="comment-delimiter">;; </span><span class="comment">can make use of the `</span><span class="comment"><span class="constant">autoload</span></span><span class="comment">' feature, e.g. by adding to your .emacs
</span><span class="comment-delimiter">;; </span><span class="comment">the following lines
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;;    </span><span class="comment">(autoload 'git-blame-mode "git-blame"
</span><span class="comment-delimiter">;;              </span><span class="comment">"Minor mode for incremental blame for Git." t)
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">Then first use of `M-x git-blame-mode' would load the package.
</span>
<span class="comment-delimiter">;;; </span><span class="comment">Compatibility:
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">It requires GNU Emacs 21 or later and Git 1.5.0 and up
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">If you'are using Emacs 20, try changing this:
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;;            </span><span class="comment">(overlay-put ovl 'face (list :background
</span><span class="comment-delimiter">;;                                         </span><span class="comment">(cdr (assq 'color (cddddr info)))))
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;; </span><span class="comment">to
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;;            </span><span class="comment">(overlay-put ovl 'face (cons 'background-color
</span><span class="comment-delimiter">;;                                         </span><span class="comment">(cdr (assq 'color (cddddr info)))))
</span>

<span class="comment-delimiter">;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;</span><span class="comment">
</span><span class="comment-delimiter">;;</span><span class="comment">
</span><span class="comment-delimiter">;;; </span><span class="comment">Code:
</span>
(<span class="keyword">eval-when-compile</span> (<span class="keyword">require</span> '<span class="constant">cl</span>))                 <span class="comment-delimiter">; </span><span class="comment">to use `</span><span class="comment"><span class="constant">push</span></span><span class="comment">', `</span><span class="comment"><span class="constant">pop</span></span><span class="comment">'
</span>

(<span class="keyword">defun</span> <span class="function-name">git-blame-color-scale</span> (<span class="type">&amp;rest</span> elements)
  <span class="doc">"Given a list, returns a list of triples formed with each
elements of the list.

a b =&gt; bbb bba bab baa abb aba aaa aab"</span>
  (<span class="keyword">let</span> (result)
    (<span class="keyword">dolist</span> (a elements)
      (<span class="keyword">dolist</span> (b elements)
        (<span class="keyword">dolist</span> (c elements)
          (setq result (cons (format <span class="string">"#%s%s%s"</span> a b c) result)))))
    result))

<span class="comment-delimiter">;; </span><span class="comment">(git-blame-color-scale "0c" "04" "24" "1c" "2c" "34" "14" "3c") =&gt;
</span><span class="comment-delimiter">;; </span><span class="comment">("#3c3c3c" "#3c3c14" "#3c3c34" "#3c3c2c" "#3c3c1c" "#3c3c24"
</span><span class="comment-delimiter">;; </span><span class="comment">"#3c3c04" "#3c3c0c" "#3c143c" "#3c1414" "#3c1434" "#3c142c" ...)
</span>
(<span class="keyword">defmacro</span> <span class="function-name">git-blame-random-pop</span> (l)
  <span class="doc">"Select a random element from L and returns it. Also remove
selected element from l."</span>
  <span class="comment-delimiter">;; </span><span class="comment">only works on lists with unique elements
</span>  `(<span class="keyword">let</span> ((e (elt ,l (random (length ,l)))))
     (setq ,l (remove e ,l))
     e))

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-dark-colors</span>
  (git-blame-color-scale <span class="string">"0c"</span> <span class="string">"04"</span> <span class="string">"24"</span> <span class="string">"1c"</span> <span class="string">"2c"</span> <span class="string">"34"</span> <span class="string">"14"</span> <span class="string">"3c"</span>)
  <span class="doc">"*List of colors (format #RGB) to use in a dark environment.

To check out the list, evaluate (list-colors-display git-blame-dark-colors)."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-light-colors</span>
  (git-blame-color-scale <span class="string">"c4"</span> <span class="string">"d4"</span> <span class="string">"cc"</span> <span class="string">"dc"</span> <span class="string">"f4"</span> <span class="string">"e4"</span> <span class="string">"fc"</span> <span class="string">"ec"</span>)
  <span class="doc">"*List of colors (format #RGB) to use in a light environment.

To check out the list, evaluate (list-colors-display git-blame-light-colors)."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-colors</span> '()
  <span class="doc">"Colors used by git-blame. The list is built once when activating git-blame
minor mode."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-ancient-color</span> <span class="string">"dark green"</span>
  <span class="doc">"*Color to be used for ancient commit."</span>)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-autoupdate</span> t
  <span class="doc">"*Automatically update the blame display while editing"</span>)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-proc</span> nil
  <span class="doc">"The running git-blame process"</span>)
(make-variable-buffer-local 'git-blame-proc)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-overlays</span> nil
  <span class="doc">"The git-blame overlays used in the current buffer."</span>)
(make-variable-buffer-local 'git-blame-overlays)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-cache</span> nil
  <span class="doc">"A cache of git-blame information for the current buffer"</span>)
(make-variable-buffer-local 'git-blame-cache)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-idle-timer</span> nil
  <span class="doc">"An idle timer that updates the blame"</span>)
(make-variable-buffer-local 'git-blame-cache)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-update-queue</span> nil
  <span class="doc">"A queue of update requests"</span>)
(make-variable-buffer-local 'git-blame-update-queue)

<span class="comment-delimiter">;; </span><span class="comment"><span class="warning">FIXME</span></span><span class="comment">: docstrings
</span>(<span class="keyword">defvar</span> <span class="variable-name">git-blame-file</span> nil)
(<span class="keyword">defvar</span> <span class="variable-name">git-blame-current</span> nil)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-mode</span> nil)
(make-variable-buffer-local 'git-blame-mode)

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-mode-line-string</span> <span class="string">" blame"</span>
  <span class="doc">"String to display on the mode line when git-blame is active."</span>)

(or (assq 'git-blame-mode minor-mode-alist)
    (setq minor-mode-alist
      (cons '(git-blame-mode git-blame-mode-line-string) minor-mode-alist)))

<span class="comment-delimiter">;;;</span><span class="comment">###</span><span class="comment"><span class="warning">autoload</span></span><span class="comment">
</span>(<span class="keyword">defun</span> <span class="function-name">git-blame-mode</span> (<span class="type">&amp;optional</span> arg)
  <span class="doc">"Toggle minor mode for displaying Git blame

With prefix ARG, turn the mode on if ARG is positive."</span>
  (interactive <span class="string">"P"</span>)
  (<span class="keyword">cond</span>
   ((null arg)
    (<span class="keyword">if</span> git-blame-mode (git-blame-mode-off) (git-blame-mode-on)))
   ((&gt; (prefix-numeric-value arg) 0) (git-blame-mode-on))
   (t (git-blame-mode-off))))

(<span class="keyword">defun</span> <span class="function-name">git-blame-mode-on</span> ()
  <span class="doc">"Turn on git-blame mode.

See also function `</span><span class="doc"><span class="constant">git-blame-mode</span></span><span class="doc">'."</span>
  (make-local-variable 'git-blame-colors)
  (<span class="keyword">if</span> git-blame-autoupdate
      (add-hook 'after-change-functions 'git-blame-after-change nil t)
    (remove-hook 'after-change-functions 'git-blame-after-change t))
  (git-blame-cleanup)
  (<span class="keyword">let</span> ((bgmode (cdr (assoc 'background-mode (frame-parameters)))))
    (<span class="keyword">if</span> (eq bgmode 'dark)
    (setq git-blame-colors git-blame-dark-colors)
      (setq git-blame-colors git-blame-light-colors)))
  (setq git-blame-cache (make-hash-table <span class="builtin">:test</span> 'equal))
  (setq git-blame-mode t)
  (git-blame-run))

(<span class="keyword">defun</span> <span class="function-name">git-blame-mode-off</span> ()
  <span class="doc">"Turn off git-blame mode.

See also function `</span><span class="doc"><span class="constant">git-blame-mode</span></span><span class="doc">'."</span>
  (git-blame-cleanup)
  (<span class="keyword">if</span> git-blame-idle-timer (cancel-timer git-blame-idle-timer))
  (setq git-blame-mode nil))

<span class="comment-delimiter">;;;</span><span class="comment">###</span><span class="comment"><span class="warning">autoload</span></span><span class="comment">
</span>(<span class="keyword">defun</span> <span class="function-name">git-reblame</span> ()
  <span class="doc">"Recalculate all blame information in the current buffer"</span>
  (interactive)
  (<span class="keyword">unless</span> git-blame-mode
    (<span class="warning">error</span> <span class="string">"Git-blame is not active"</span>))

  (git-blame-cleanup)
  (git-blame-run))

(<span class="keyword">defun</span> <span class="function-name">git-blame-run</span> (<span class="type">&amp;optional</span> startline endline)
  (<span class="keyword">if</span> git-blame-proc
      <span class="comment-delimiter">;; </span><span class="comment">Should maybe queue up a new run here
</span>      (message <span class="string">"Already running git blame"</span>)
    (<span class="keyword">let</span> ((display-buf (current-buffer))
          (blame-buf (get-buffer-create
                      (concat <span class="string">" git blame for "</span> (buffer-name))))
          (args '(<span class="string">"--incremental"</span> <span class="string">"--contents"</span> <span class="string">"-"</span>)))
      (<span class="keyword">if</span> startline
          (setq args (append args
                             (list <span class="string">"-L"</span> (format <span class="string">"%d,%d"</span> startline endline)))))
      (setq args (append args
                         (list (file-name-nondirectory buffer-file-name))))
      (setq git-blame-proc
            (apply 'start-process
                   <span class="string">"git-blame"</span> blame-buf
                   <span class="string">"git"</span> <span class="string">"blame"</span>
                   args))
      (<span class="keyword">with-current-buffer</span> blame-buf
        (erase-buffer)
        (make-local-variable 'git-blame-file)
        (make-local-variable 'git-blame-current)
        (setq git-blame-file display-buf)
        (setq git-blame-current nil))
      (set-process-filter git-blame-proc 'git-blame-filter)
      (set-process-sentinel git-blame-proc 'git-blame-sentinel)
      (process-send-region git-blame-proc (point-min) (point-max))
      (process-send-eof git-blame-proc))))

(<span class="keyword">defun</span> <span class="function-name">remove-git-blame-text-properties</span> (start end)
  (<span class="keyword">let</span> ((modified (buffer-modified-p))
        (inhibit-read-only t))
    (remove-text-properties start end '(point-entered nil))
    (set-buffer-modified-p modified)))

(<span class="keyword">defun</span> <span class="function-name">git-blame-cleanup</span> ()
  <span class="doc">"Remove all blame properties"</span>
    (mapcar 'delete-overlay git-blame-overlays)
    (setq git-blame-overlays nil)
    (remove-git-blame-text-properties (point-min) (point-max)))

(<span class="keyword">defun</span> <span class="function-name">git-blame-update-region</span> (start end)
  <span class="doc">"Rerun blame to get updates between START and END"</span>
  (<span class="keyword">let</span> ((overlays (overlays-in start end)))
    (<span class="keyword">while</span> overlays
      (<span class="keyword">let</span> ((overlay (pop overlays)))
        (<span class="keyword">if</span> (&lt; (overlay-start overlay) start)
            (setq start (overlay-start overlay)))
        (<span class="keyword">if</span> (&gt; (overlay-end overlay) end)
            (setq end (overlay-end overlay)))
        (setq git-blame-overlays (delete overlay git-blame-overlays))
        (delete-overlay overlay))))
  (remove-git-blame-text-properties start end)
  <span class="comment-delimiter">;; </span><span class="comment">We can be sure that start and end are at line breaks
</span>  (git-blame-run (1+ (count-lines (point-min) start))
                 (count-lines (point-min) end)))

(<span class="keyword">defun</span> <span class="function-name">git-blame-sentinel</span> (proc status)
  (<span class="keyword">with-current-buffer</span> (process-buffer proc)
    (<span class="keyword">with-current-buffer</span> git-blame-file
      (setq git-blame-proc nil)
      (<span class="keyword">if</span> git-blame-update-queue
          (git-blame-delayed-update))))
  <span class="comment-delimiter">;;</span><span class="comment">(kill-buffer (process-buffer proc))
</span>  <span class="comment-delimiter">;;</span><span class="comment">(message "git blame finished")
</span>  )

(<span class="keyword">defvar</span> <span class="variable-name">in-blame-filter</span> nil)

(<span class="keyword">defun</span> <span class="function-name">git-blame-filter</span> (proc str)
  (<span class="keyword">save-excursion</span>
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    (insert-before-markers str)
    (goto-char 0)
    (<span class="keyword">unless</span> in-blame-filter
      (<span class="keyword">let</span> ((more t)
            (in-blame-filter t))
        (<span class="keyword">while</span> more
          (setq more (git-blame-parse)))))))

(<span class="keyword">defun</span> <span class="function-name">git-blame-parse</span> ()
  (<span class="keyword">cond</span> ((looking-at <span class="string">"</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">(</span></span><span class="string">[0-9a-f]\\{40\\}</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">)</span></span><span class="string"> </span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">(</span></span><span class="string">[0-9]+</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">)</span></span><span class="string"> </span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">(</span></span><span class="string">[0-9]+</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">)</span></span><span class="string"> </span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">(</span></span><span class="string">[0-9]+</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">)</span></span><span class="string">\n"</span>)
         (<span class="keyword">let</span> ((hash (match-string 1))
               (src-line (string-to-number (match-string 2)))
               (res-line (string-to-number (match-string 3)))
               (num-lines (string-to-number (match-string 4))))
           (setq git-blame-current
                 (<span class="keyword">if</span> (string= hash <span class="string">"0000000000000000000000000000000000000000"</span>)
                     nil
                   (git-blame-new-commit
                    hash src-line res-line num-lines))))
         (delete-region (point) (match-end 0))
         t)
        ((looking-at <span class="string">"filename </span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">(</span></span><span class="string">.+</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">)</span></span><span class="string">\n"</span>)
         (<span class="keyword">let</span> ((filename (match-string 1)))
           (git-blame-add-info <span class="string">"filename"</span> filename))
         (delete-region (point) (match-end 0))
         t)
        ((looking-at <span class="string">"</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">(</span></span><span class="string">[a-z-]+</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">)</span></span><span class="string"> </span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">(</span></span><span class="string">.+</span><span class="string"><span class="regexp-grouping-backslash">\\</span></span><span class="string"><span class="regexp-grouping-construct">)</span></span><span class="string">\n"</span>)
         (<span class="keyword">let</span> ((key (match-string 1))
               (value (match-string 2)))
           (git-blame-add-info key value))
         (delete-region (point) (match-end 0))
         t)
        ((looking-at <span class="string">"boundary\n"</span>)
         (setq git-blame-current nil)
         (delete-region (point) (match-end 0))
         t)
        (t
         nil)))

(<span class="keyword">defun</span> <span class="function-name">git-blame-new-commit</span> (hash src-line res-line num-lines)
  (<span class="keyword">save-excursion</span>
    (set-buffer git-blame-file)
    (<span class="keyword">let</span> ((info (gethash hash git-blame-cache))
          (inhibit-point-motion-hooks t)
          (inhibit-modification-hooks t))
      (<span class="keyword">when</span> (not info)
    <span class="comment-delimiter">;; </span><span class="comment">Assign a random color to each new commit info
</span>    <span class="comment-delimiter">;; </span><span class="comment">Take care not to select the same color multiple times
</span>    (<span class="keyword">let</span> ((color (<span class="keyword">if</span> git-blame-colors
             (git-blame-random-pop git-blame-colors)
               git-blame-ancient-color)))
          (setq info (list hash src-line res-line num-lines
                           (git-describe-commit hash)
                           (cons 'color color))))
        (puthash hash info git-blame-cache))
      (goto-line res-line)
      (<span class="keyword">while</span> (&gt; num-lines 0)
        (<span class="keyword">if</span> (get-text-property (point) 'git-blame)
            (forward-line)
          (<span class="keyword">let*</span> ((start (point))
                 (end (<span class="keyword">progn</span> (forward-line 1) (point)))
                 (ovl (make-overlay start end)))
            (push ovl git-blame-overlays)
            (overlay-put ovl 'git-blame info)
            (overlay-put ovl 'help-echo hash)
            (overlay-put ovl 'face (list <span class="builtin">:background</span>
                                         (cdr (assq 'color (nthcdr 5 info)))))
            <span class="comment-delimiter">;; </span><span class="comment">the point-entered property doesn't seem to work in overlays
</span>            <span class="comment-delimiter">;;</span><span class="comment">(overlay-put ovl 'point-entered
</span>            <span class="comment-delimiter">;;             </span><span class="comment">`(lambda (x y) (git-blame-identify ,hash)))
</span>            (<span class="keyword">let</span> ((modified (buffer-modified-p)))
              (put-text-property (<span class="keyword">if</span> (= start 1) start (1- start)) (1- end)
                                 'point-entered
                                 `(<span class="keyword">lambda</span> (x y) (git-blame-identify ,hash)))
              (set-buffer-modified-p modified))))
        (setq num-lines (1- num-lines))))))

(<span class="keyword">defun</span> <span class="function-name">git-blame-add-info</span> (key value)
  (<span class="keyword">if</span> git-blame-current
      (nconc git-blame-current (list (cons (intern key) value)))))

(<span class="keyword">defun</span> <span class="function-name">git-blame-current-commit</span> ()
  (<span class="keyword">let</span> ((info (get-char-property (point) 'git-blame)))
    (<span class="keyword">if</span> info
        (car info)
      (<span class="warning">error</span> <span class="string">"No commit info"</span>))))

(<span class="keyword">defun</span> <span class="function-name">git-describe-commit</span> (hash)
  (<span class="keyword">with-temp-buffer</span>
    (call-process <span class="string">"git"</span> nil t nil
                  <span class="string">"log"</span> <span class="string">"-1"</span> <span class="string">"--pretty=oneline"</span>
                  hash)
    (buffer-substring (point-min) (1- (point-max)))))

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-last-identification</span> nil)
(make-variable-buffer-local 'git-blame-last-identification)
(<span class="keyword">defun</span> <span class="function-name">git-blame-identify</span> (<span class="type">&amp;optional</span> hash)
  (interactive)
  (<span class="keyword">let</span> ((info (gethash (or hash (git-blame-current-commit)) git-blame-cache)))
    (<span class="keyword">when</span> (and info (not (eq info git-blame-last-identification)))
      (message <span class="string">"%s"</span> (nth 4 info))
      (setq git-blame-last-identification info))))

<span class="comment-delimiter">;; </span><span class="comment">(defun git-blame-after-save ()
</span><span class="comment-delimiter">;;   </span><span class="comment">(when git-blame-mode
</span><span class="comment-delimiter">;;     </span><span class="comment">(git-blame-cleanup)
</span><span class="comment-delimiter">;;     </span><span class="comment">(git-blame-run)))
</span><span class="comment-delimiter">;; </span><span class="comment">(add-hook 'after-save-hook 'git-blame-after-save)
</span>
(<span class="keyword">defun</span> <span class="function-name">git-blame-after-change</span> (start end length)
  (<span class="keyword">when</span> git-blame-mode
    (git-blame-enq-update start end)))

(<span class="keyword">defvar</span> <span class="variable-name">git-blame-last-update</span> nil)
(make-variable-buffer-local 'git-blame-last-update)
(<span class="keyword">defun</span> <span class="function-name">git-blame-enq-update</span> (start end)
  <span class="doc">"Mark the region between START and END as needing blame update"</span>
  <span class="comment-delimiter">;; </span><span class="comment">Try to be smart and avoid multiple callouts for sequential
</span>  <span class="comment-delimiter">;; </span><span class="comment">editing
</span>  (<span class="keyword">cond</span> ((and git-blame-last-update
              (= start (cdr git-blame-last-update)))
         (setcdr git-blame-last-update end))
        ((and git-blame-last-update
              (= end (car git-blame-last-update)))
         (setcar git-blame-last-update start))
        (t
         (setq git-blame-last-update (cons start end))
         (setq git-blame-update-queue (nconc git-blame-update-queue
                                             (list git-blame-last-update)))))
  (<span class="keyword">unless</span> (or git-blame-proc git-blame-idle-timer)
    (setq git-blame-idle-timer
          (run-with-idle-timer 0.5 nil 'git-blame-delayed-update))))

(<span class="keyword">defun</span> <span class="function-name">git-blame-delayed-update</span> ()
  (setq git-blame-idle-timer nil)
  (<span class="keyword">if</span> git-blame-update-queue
      (<span class="keyword">let</span> ((first (pop git-blame-update-queue))
            (inhibit-point-motion-hooks t))
        (git-blame-update-region (car first) (cdr first)))))

(<span class="keyword">provide</span> '<span class="constant">git-blame</span>)

<span class="comment-delimiter">;;; </span><span class="comment">git-blame.el ends here
</span></pre>
  </body>
</html>
