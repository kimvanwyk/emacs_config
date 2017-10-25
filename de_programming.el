(require-if-exists 'c-funcs)
(require-if-exists 'browse-funcs)
(require-if-exists 'cq)
;; Grab the tag-funcs for working with ctags and do some key-binding
(require-if-exists 'tag-funcs)
(global-set-key "\C-c\C-f" 'tag-file-find)

;; Scroll compilation windows
(setq compilation-scroll-output t)

;; (require-if-exists 'etags-select)
(global-set-key "\M-." 'etags-select-find-tag-at-point)

;; Never use tabs to indent:
(setq-default indent-tabs-mode nil)
 
;; Tab stops:
(setq tab-width 4)
 
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)

;; Rebind completion for python-mode
;; (meta ?\/) is the key M-/
(defun my-python-keys ()
  (define-key python-mode-map [(meta ?\/)] 'python-complete-symbol)) 
(add-hook 'python-mode-hook 'my-python-keys)

;; ;; isort support
;; (require-if-exists'py-isort)
;; (add-hook 'before-save-hook 'py-isort-before-save)

;; Where am I?
(which-func-mode t)

(defun make-tags()
  "Makes a tags file in current directory"
  (interactive)
  (message (concat "Making a TAGS file in " (file-name-directory (pwd))))
  (shell-command "c:\\utils\\ctags\\ctags -e -R")
  (message "TAGS file created")
)

;; Auto-load C-mode bindings for .c and .h files
(setq auto-mode-alist 
        (append '(("\\.c$" . c-mode) 
                  ("\\.h$" . c-mode))
                 auto-mode-alist))

;; bind some keys for gud
(global-set-key (quote [f5]) (quote gud-step))
(global-set-key (quote [f6]) (quote gud-next))
(global-set-key (quote [f9]) (quote gud-cont))

;;Add a hook to let CR do indentation for C-mode at least
(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "" (quote newline-and-indent)))

(add-hook 'c-initialization-hook 'my-make-CR-do-indent)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c-mode . "gnu")))

;; a function lister for C
(defun list-functions ()
  "Find occurrence of functions in buffer"
  (interactive)
  (occur "^\\w+.*)[^;]*$")
)
(global-set-key "\C-cf" 'list-functions)

(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call 'perl x.pl' in a shell.
The file can be php, perl, python, java.
File suffix is used to determine what program to run."
(interactive)
  (let (ext-map file-name file-ext prog-name cmd-str)
; get the file name
; get the program name
; run it
    (setq ext-map
          '(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("sh" . "bash")
            ("java" . "javac")
            )
          )
    (setq suffix-map
          '(
            ("php" . "")
            ("pl" . "")
            ("py" . "debug")
            ("sh" . "debug")
            ("java" . "debug")
            )
          )

    (setq file-name (buffer-file-name))
    (setq file-ext (file-name-extension file-name))
    (setq prog-name (cdr (assoc file-ext ext-map)))
    (setq suffix (cdr (assoc file-ext suffix-map)))
    (setq cmd-str (concat prog-name " \"" file-name "\"" " " suffix))
    (shell-command cmd-str)))

(global-set-key "\C-cR" 'run-current-file)

;; Set compilation skip threshold to only go to errors
(setq compilation-skip-threshold 2)

;; A command to kill gud
(defun kill-gud()
(interactive)
;; If the *gud* buffer is live, kill it
(if (buffer-live-p (get-buffer "*gud*"))
    (progn
    (kill-buffer "*gud*")
    (message "Buffer *gud* killed"))
  (message "Buffer *gud* already killed")))

; Add a function to remove double-quoted-strings as valid strings from delphi-mode, and add to the appropriate hook
(load-file "~/emacs/lisp/addons/delphi-mode-changes.el")
(add-hook 'delphi-mode-hook 'remove-double-string-from-delphi-mode)

;; Over-ride auto-mode matching to use preferred modes for some files
(setq auto-mode-alist
(append '(("\\.pas\\'" . delphi-mode)) auto-mode-alist)
)
(setq auto-mode-alist
(append '(("\\.rnc\\'" . rnc-mode)) auto-mode-alist)
)
(setq auto-mode-alist
(append '(("\\.pyx\\'" . python-mode)) auto-mode-alist)
)

;; NXML stuff
;path to where nxml is
(if (file-exists-p "~/emacs/lisp/addons/nxml-mode")
(progn
  (add-to-list 'load-path "~/emacs/lisp/addons/nxml-mode")
  (set 'nxml-path "~/emacs/lisp/addons/nxml-mode/")
  (load (concat nxml-path "rng-auto.el") t)
  (add-to-list 'auto-mode-alist
               (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
                     'nxml-mode))
  (unify-8859-on-decoding-mode 1)
  (setq magic-mode-alist
        (cons '("<\\?xml " . nxml-mode)
              magic-mode-alist))
  (fset 'xml-mode 'nxml-mode)
))

;; RNC mode is to syntax highlight RELAX NG schema's`
(require-if-exists 'rnc-mode)

;; Dos batch file mode
(if (require-if-exists 'dosbat "dosbat/")
(progn
    (add-to-list 'auto-mode-alist '("\\.bat$" . bat-mode))
    (add-hook 'bat-mode-hook 'turn-on-font-lock)))

;; JS2 mode (Javascript mode from Steve Yegge)
(autoload 'js2-mode' "js2" nil t)
(setq auto-mode-alist 
        (append '(("\\.js$" . js2-mode) 
                  ("\\.json$" . js2-mode))
                 auto-mode-alist))

(require-if-exists 'json)

;; Parenthesis highlighting
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; NSI Mode
(if (require-if-exists 'nsi-mode)
    (add-to-list 'auto-mode-alist '("\\.nsi$" . nsi-mode)))

;; Add support for Wiki mode
(require-if-exists 'wikipedia-mode)

;; ASCII Table support
(require-if-exists 'ascii-table)

;; via http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun kvw-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original" 
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; duplicate a line
(global-set-key (kbd "\C-cwd") 'kvw-duplicate-line)

;; duplicate a line and comment the first
(global-set-key (kbd "\C-cwc") (lambda()(interactive)(kvw-duplicate-line t)))

;; Django major mode support
(require-if-exists 'django-html-mode "django-html-mode/")

(defun run_gdb (s)
" kills gud and runs gdb with specified arg"
(interactive "sGDB Argument:")
(kill-gud)
(gdb s))

;; po-mode stuff
(if (require-if-exists 'po-mode "po_mode/")
    (progn
      (autoload 'po-mode "po-mode+" "Major mode for translators to edit PO files" t)
      (eval-after-load 'po-mode '(load "gb-po-mode"))
      ))

;; Require C-sharp mode
(require-if-exists 'csharp-mode "csharp-mode/")

;; JSON formatting, by piping through emacs. Seen at http://irreal.org/blog/?p=354
;; http://irreal.org/blog/?p=354
(defun json-format ()
(interactive)
(save-excursion
(shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(provide 'de_programming)
