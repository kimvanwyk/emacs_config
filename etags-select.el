;;; etags-select.el --- Select from multiple tags

;; Copyright (C) 2007  Scott Frazer

;; Author: Scott Frazer <frazer.scott@gmail.com>
;; Maintainer: Scott Frazer <frazer.scott@gmail.com>
;; Created: 07 Jun 2007
;; Version: 1.2
;; Keywords: etags tags tag select

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Open a buffer with file/lines of exact-match tags shown.  Select one by
;; going to a line and pressing return.  pop-tag-mark still works with this
;; code.
;;
;; If there is only one match, you can skip opening the selection window by
;; setting a custom variable.  This means you could substitute the key binding
;; for find-tag-at-point with etags-select-find-tag-at-point, although it
;; won't play well with tags-loop-continue.  On the other hand, if you like
;; the behavior of tags-loop-continue you probably don't need this code.
;;
;; I use this:
;; (global-set-key "\M-?" 'etags-select-find-tag-at-point)
;;
;; Contributers of ideas and/or code:
;; David Engster
;;
;;; Change log:
;;
;; 13 Jun 2007 -- Need to regexp-quote the searched-for string

;;; Code:

(require 'custom)
(require 'etags)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom stuff

;;;###autoload
(defgroup etags-select-mode nil
  "*etags select mode."
  :group 'etags)

;;;###autoload
(defcustom etags-select-no-select-for-one-match t
  "*If non-nil, don't open the selection window if there is only one
matching tag."
  :group 'etags-select-mode
  :type '(boolean))

;;;###autoload
(defcustom etags-select-mode-hook nil
  "*List of functions to call on entry to etags-select-mode mode."
  :group 'etags-select-mode
  :type 'hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables

(defvar etags-select-buffer-name "*etags-select*"
  "etags-select buffer name.")

(defvar etags-select-mode-font-lock-keywords nil
  "etags-select font-lock-keywords.")

(defvar etags-select-starting-buffer nil
  "etags-select buffer tag is being found from.")

(defconst etags-select-non-tag-regexp "\\(\\s-*$\\|In:\\|Finding tag:\\)"
  "etags-select non-tag regex.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions

(if (string-match "XEmacs" emacs-version)
    (fset 'etags-select-match-string 'match-string)
  (fset 'etags-select-match-string 'match-string-no-properties))

;; I use Emacs, but with a hacked version of XEmacs' etags.el, thus this variable

(defvar etags-select-use-xemacs-etags-p (fboundp 'get-tag-table-buffer)
  "Use XEmacs etags?")

(defun etags-select-insert-all-exact-matches (tagname tag-file)
  (let ((tag-table-buffer (if etags-select-use-xemacs-etags-p
                              (get-tag-table-buffer tag-file)
                            (visit-tags-table-buffer tag-file)
                            (get-file-buffer tag-file)))
        (tag-file-path (file-name-directory tag-file))
        (tags-found 0)
        tag-line filename current-filename)
    (set-buffer tag-table-buffer)
    (goto-char (point-min))
    (while (re-search-forward (concat "\^?" tagname "\^A") nil t)
      (setq tags-found (1+ tags-found))
      (beginning-of-line)
      (re-search-forward "\\(.*\\)\^?")
      (setq tag-line (etags-select-match-string 1))
      (end-of-line)
      (save-excursion
        (re-search-backward "\f")
        (re-search-forward "^\\(.*?\\),")
        (setq filename (concat tag-file-path (etags-select-match-string 1))))
      (set-buffer etags-select-buffer-name)
      (when (not (string= filename current-filename))
        (insert "\nIn: " filename "\n")
        (setq current-filename filename))
      (insert tag-line "\n")
      (set-buffer tag-table-buffer))
    tags-found))

;;;###autoload
(defun etags-select-find-tag-at-point ()
  "Do a find-tag-at-point, and display all exact matches.  If only one match is
found, see the `etags-select-no-select-for-one-match' variable to decide what
to do."
  (interactive)
  (let ((tagname (find-tag-default))
        (tag-files (if etags-select-use-xemacs-etags-p
                       (buffer-tag-table-list)
                     (mapcar 'tags-expand-table-name tags-table-list)))
        (tags-found 0))
    (setq etags-select-starting-buffer (buffer-name))
    (get-buffer-create etags-select-buffer-name)
    (set-buffer etags-select-buffer-name)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Finding tag: " tagname "\n")
    (mapcar (lambda (tag-file)
              (setq tags-found (+ tags-found (etags-select-insert-all-exact-matches tagname tag-file))))
            tag-files)
    (cond ((= tags-found 0)
           (message (concat "No exact match for tag \"" tagname "\""))
           (ding))
          ((and (= tags-found 1) etags-select-no-select-for-one-match)
           (set-buffer etags-select-buffer-name)
           (goto-char (point-min))
           (etags-select-next-tag)
           (etags-select-goto-tag))
          (t
           (set-buffer etags-select-buffer-name)
           (goto-char (point-min))
           (etags-select-next-tag)
           (set-buffer-modified-p nil)
           (setq buffer-read-only t)
           (display-buffer etags-select-buffer-name)
           (pop-to-buffer etags-select-buffer-name)
           (force-mode-line-update)
           (etags-select-mode tagname)))))

(defun etags-select-goto-tag (&optional arg)
  "Goto the file/line of the tag under the cursor.  Use the C-u prefix to
prevent the etags-select window from closing."
  (interactive "P")
  (let (tagname tag-point text-to-search-for filename filename-point (search-count 1))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "Finding tag: \\(.*\\)$")
      (setq tagname (etags-select-match-string 1)))
    (beginning-of-line)
    (if (looking-at etags-select-non-tag-regexp)
        (message "Please put the cursor on a line with the tag.")
      (setq tag-point (point))
      (setq overlay-arrow-position (point-marker))
      (re-search-forward "\\s-*\\(.*\\)\\s-*$")
      (setq text-to-search-for (concat "^\\s-*" (regexp-quote (etags-select-match-string 1))))
      (goto-char tag-point)
      (re-search-backward "^In: \\(.*\\)$")
      (setq filename (etags-select-match-string 1))
      (setq filename-point (point))
      (goto-char tag-point)
      (while (re-search-backward text-to-search-for filename-point t)
        (setq search-count (1+ search-count)))
      (goto-char tag-point)
      (if arg
          (let ((window (get-buffer-window etags-select-starting-buffer)))
            (when window (select-window window)))
        (let ((window (get-buffer-window etags-select-buffer-name)))
          (when window (delete-window window)))
        (kill-buffer etags-select-buffer-name))
      (display-buffer etags-select-starting-buffer)
      (set-buffer etags-select-starting-buffer)
      (if etags-select-use-xemacs-etags-p
          (push-tag-mark)
        (ring-insert find-tag-marker-ring (point-marker)))
      (find-file filename)
      (goto-char (point-min))
      (while (> search-count 0)
        (unless (re-search-forward text-to-search-for nil t)
          (message "TAGS file out of date ... stopping at closest match")
          (setq search-count 1))
        (setq search-count (1- search-count)))
      (beginning-of-line)
      (re-search-forward tagname)
      (goto-char (match-beginning 0)))))

(defun etags-select-next-tag ()
  (interactive)
  (beginning-of-line)
  (when (not (eobp))
    (forward-line))
  (while (and (looking-at etags-select-non-tag-regexp) (not (eobp)))
    (forward-line))
  (when (eobp)
    (ding)))

(defun etags-select-previous-tag ()
  (interactive)
  (beginning-of-line)
  (when (not (bobp))
    (forward-line -1))
  (while (and (looking-at etags-select-non-tag-regexp) (not (bobp)))
    (forward-line -1))
  (when (bobp)
    (ding)))

(defun etags-select-quit ()
  (interactive)
  (kill-buffer nil)
  (delete-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap

(defvar etags-select-mode-map nil "'etags-select-mode' keymap.")
(if (not etags-select-mode-map)
    (let ((map (make-keymap)))
      (define-key map [(return)] 'etags-select-goto-tag)
      (define-key map [(down)] 'etags-select-next-tag)
      (define-key map [(up)] 'etags-select-previous-tag)
      (define-key map [(q)] 'etags-select-quit)
      (setq etags-select-mode-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode startup

(defun etags-select-mode (tagname)
  "etags-select-mode is a mode for browsing through tags.\n\n
\\{etags-select-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'etags-select-mode)
  (setq mode-name "etags-select")
  (set-syntax-table text-mode-syntax-table)
  (use-local-map etags-select-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq etags-select-mode-font-lock-keywords
        (list (list "^\\(Finding tag:\\)" '(1 font-lock-keyword-face))
              (list "^\\(In:\\) \\(.*\\)" '(1 font-lock-keyword-face) '(2 font-lock-string-face))
              (list tagname '(0 font-lock-function-name-face))))
  (setq font-lock-defaults '(etags-select-mode-font-lock-keywords))
  (setq overlay-arrow-position nil)
  (run-hooks 'etags-select-mode-hook))

(provide 'etags-select)
;;; etags-select.el ends here
