;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
 (setq desktop-globals-to-save
       (append '((extended-command-history . 30)
                 (file-name-history        . 100)
                 (grep-history             . 30)
                 (compile-history          . 30)
                 (minibuffer-history       . 50)
                 (query-replace-history    . 60)
                 (read-expression-history  . 60)
                 (regexp-history           . 60)
                 (regexp-search-ring       . 20)
                 (search-ring              . 20)
                 (shell-command-history    . 50)
                 tags-file-name
                 register-alist)))
(desktop-save-mode 1)

(savehist-mode 1)

;; via http://snarfed.org/space/gnu%20emacs%20backup%20files
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir "~/emacs_backups/")
(make-directory autosave-dir t)
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(setq backup-directory-alist (list (cons "." autosave-dir)))

(provide 'de_backup)