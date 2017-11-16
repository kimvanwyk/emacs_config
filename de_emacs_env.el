;; customisations to the general emacs environment itself

;; (if (require-if-exists 'package) 
;;     (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;   )

(require-if-exists 'package)
;; Package management with Marmalade
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; stop confirmation required for opening largish files.
(setq large-file-warning-threshold nil)
;; Set winner mode on, to track window configs if needed
(winner-mode 1)

;; Prevent a trailing newline from beingwthout oneally (and silently) written to files wthout one
(setq require-final-newline nil)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Hippie Expansion stuff. Set hippie-expansion to C-tab
;;(require-if-exists 'hippie-exp)
(global-set-key (kbd "C-<tab>") 'hippie-expand)
;; Add python completion to avoid altering hippie-exp.el itself.
;;(add-to-list 'hippie-expand-try-functions-list 'try-complete-py-complete-symbol)

;; Syntax highlighting on:
(global-font-lock-mode 1)
(defconst font-lock-maximum-decoration t)

;; - Icicles
;; (add-to-list 'load-path "~/emacs/lisp/addons/icicles")
;; Icicles Stuff:
;;(require-if-exists 'icicles)
;; Add the current line number to the mode bar
(line-number-mode t)
 
;; Add the current column number to the mode bar
(column-number-mode t)

;; Enable highlighting when marking a region
(setq transient-mark-mode t)

(global-set-key "\C-cl" 'goto-line)
(global-set-key "\C-Z" 'undo)

;; Reload .emacs file by typing: Mx reload.
(defun reload () "Reloads .emacs interactively."
  (interactive)
  (load "~/.emacs"))

(display-time-mode t)
(setq display-time-24hr-format t)

;;http://www.emacsblog.org/2007/04/09/highlight-the-current-line/
(global-hl-line-mode 0)
;; To customize the background color
(set-face-background 'hl-line "#040")  ;; Emacs 22 Only

;; find file at point
(ffap-bindings)

;; enable ido-mode
(ido-mode 1)


;; Window navigation
(when (fboundp 'windmove-default-keybindings)
      (windmove-default-keybindings))
;; Emacs server support
(server-start)


;; Bind f3 to repeat our last keyboard macro
(global-set-key [f3] 'call-last-kbd-macro)

;; Make various default-unavailable commands permanently available
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Disable insert key on keyboard
(global-set-key (quote [insert]) nil)

;; Bindings for replace string and replace regex
(global-set-key "\C-crs" 'replace-string)
(global-set-key "\C-crr" 'replace-regexp)
;; Binding to delete-region
(global-set-key "\C-cd" 'delete-region)

(defun ridm () 
  "Remove intrusive CTRL-Ms from the buffer" 
  (interactive) 
  (save-excursion 
    (goto-char (point-min)) 
    (replace-string "\C-m\C-j" "\C-j")))


;; Resize to 3/4 - 1/4 instead of 1/2-1/2
(defun three-quarters-window () 
  "Resizes current window big"
  (interactive)
  (let ((size (- (truncate (* .75 (frame-height))) (window-height)))) 
    (if (> size 0) 
        (enlarge-window size)))) 
(global-set-key "\C-x7" 'three-quarters-window)

;; Toggle window dedication
(defun toggle-window-dedicated ()
"Toggle whether the current active window is dedicated or not"
(interactive)
(message
(if (let (window (get-buffer-window (current-buffer)))
   (set-window-dedicated-p window 
        (not (window-dedicated-p window))))
"Window '%s' is dedicated"
"Window '%s' is normal")
(current-buffer)))

(global-set-key [pause] 'toggle-window-dedicated)

(if (require-if-exists 'browse-kill-ring)
    (browse-kill-ring-default-keybindings))

;; Advise killing function to operate on current line if nothing is selected
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(defun dont-kill-emacs ()
  "Print a message when the shortcut to kill emacs is pressed, listing the command to run instead"
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
  
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

;; uniquify buffer names
(if (require-if-exists 'uniquify) 
    (progn
    (setq 
     uniquify-buffer-name-style 'post-forward
     uniquify-strip-common-suffix nil)
  (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
  (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special
                                        ; buffers (or Gnus mail
                                        ; buffers)
  ))

;; Unbind the suspend-frame key to prevent accidental emacs suspension
(global-unset-key "\C-x\C-z")

; An unfill region command
(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs, 
  indented text (quotes,code) and lines starting with an asterix (lists) intact."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

(require-if-exists 'hide-lines)

(setq apropos-do-all t)
(setq confirm-nonexistent-file-or-buffer nil)

;;Add a function to evaluate sexp's in place and provide the result. From emacs.wordpress.com
;;bound to C-c e - which was unused
(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)

;; A function to be used programatically to replace all occurences of OLD in a supplied string with NEW
(defun replace-in-string (strmatch old new)
(while (string-match old strmatch)
  (setq strmatch (replace-match new t t strmatch)))
strmatch)

;; Add regex-IDE
(load "regex-tool.el" t)

;; Bring in extra-edit stuff
(require-if-exists 'extraedit)
(require-if-exists 'htmlize)

;; only show battery mode if the system name reflects the term "laptop"
(if (string-match ".*?-laptop.*" system-name)
    (display-battery-mode t)
  (display-battery-mode nil))

  ;; Color Theme
  (if (require-if-exists 'color-theme "color-theme/")
      (progn
        (color-theme-initialize)
        (color-theme-dark-laptop)
        ;; (load-file "color-theme/color-theme-chocolate-rain.el")
        ;; (color-theme-chocolate-rain)
        ))


(put 'set-goal-column 'disabled nil)

;; Ignore uninteresting files in tab completion when finding files to visit in buffers.
;; Via http://stackoverflow.com/questions/1731634/dont-show-uninteresting-files-in-emacs-completion-window/1732081#1732081
;; (defadvice completion--file-name-table (after 
;;                                         ignoring-backups-f-n-completion 
;;                                         activate)
;;   "filter out results when the have completion-ignored-extensions"
;;   (let ((res ad-return-value))
;; (if (and (listp res)
;; 	 (stringp (car res))
;; 	 (cdr res))                 ; length > 1, don't ignore sole match
;;     (setq ad-return-value
;;               (completion-pcm--filename-try-filter res)))))

(defun os-aware-buffer-file-name () 
  " Return a string with the current buffer-file-name, with path
seperators appropriate to the current OS"
  (setq fn (buffer-file-name))
  (if (equal window-system 'w32)
      (replace-in-string fn "/" "\\")
    fn)
)

;; Disable pinging of files that look like URL's
(setq ffap-machine-p-known "reject")

;; Advice for rectangle open and close, to add optional better whitespace handling
;; Courtesy Jon McKeown
(defadvice open-rectangle (around saner-open-rect (start end &optional fill ragged))
  "Blank out the region-rectangle, shifting text right.

Default behaviour is to insert whitespace at the left edge of the rectangle,
moving text in the region to the column after the right edge.

Interactively, a - argument does the whitespace insertion at the first
whitespace after existing text. A prefix argument will fill short lines
with whitespace. Both options can be applied.

When called from a program the rectangle's corners are START and END, and
FILL and RAGGED have the effect of a prefix and - argument respectively."
  (interactive "*r\nP")
  (when (called-interactively-p)
    (if (< (prefix-numeric-value fill) 0) (setq ragged t))
    (if (eq fill '-) (setq fill nil)))
  (apply-on-rectangle 'open-rectangle-line start end fill ragged)
  (goto-char start))

(defadvice open-rectangle-line (around
                                sane-open-rect-line
                                (startcol endcol fill ragged))
  (let ((endpos (+ (point-at-bol) endcol)))
    (when (= (move-to-column startcol (if fill t 'coerce)) startcol)
      (unless (and (not fill)
                   (= (point) (point-at-eol)))
        (if ragged (skip-syntax-forward "^ " endpos))
        (indent-to endcol)))))

(defadvice delete-whitespace-rectangle-line (around
                                             sane-del-ws-rect-line
                                             (startcol endcol fill ragged))
  (let ((endpos (+ (point-at-bol) endcol)))
    (when (= (move-to-column startcol (if fill t 'coerce)) startcol)
      (unless (= (point) (point-at-eol))
        (if ragged (skip-syntax-forward "^ " endpos))
        (delete-region (point) (progn
                                 (skip-syntax-forward " " endpos)
                                 (point)))))))

(defadvice delete-whitespace-rectangle (around
                                        sane-del-ws-rect
                                        (start end &optional fill ragged))
  "Delete continuous whitespace within the rectangle.
Default behaviour operates only on lines containing whitespace in the
left column of the rectangle, and deletes continuous whitespace up to
the right column.

Interactively, a - argument causes the deletion to apply on every line
and start at the first whitespace after the left column. A prefix
argument will space-fill short lines as far as the left column. Both can
be applied.

When called from a program the rectangle's corners are START and END, and
FILL and RAGGED have the effect of prefix and - arguments respectively."
  (interactive "*r\nP")
  (when (called-interactively-p)
    (if (< (prefix-numeric-value fill) 0) (setq ragged t))
    (if (eq fill '-) (setq fill nil)))
  (apply-on-rectangle 'delete-whitespace-rectangle-line start end fill ragged))

;; Insert quoted characters in hex
(setq read-quoted-char-radix 16)

;; Underline entire line with dashes
(fset 'kvw-underline-line
   [?\C-a ?\C-  ?\C-e ?\M-w return ?\C-y ?\C-  ?\C-a ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?r ?e ?g ?e ?x ?p return ?. return ?- return return ?\C-x ?\C-s])

(require-if-exists 'table)
(provide 'de_emacs_env)

