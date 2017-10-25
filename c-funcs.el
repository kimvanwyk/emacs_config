(defun define-block(start end defname)
"Adds
#ifdef <name>
...
#endif /*<name>*/
to the selected block of text. Prompts for <name>"
(interactive "r
sEnter Define Name:")
(narrow-to-region start end)
(goto-char (point-min))
(insert (concat "#ifdef " defname "\n"))
(goto-char (- (point-max) 1))
(insert (concat "\n#endif /*" defname "*/"))
(widen)
)

(defun which-case ()
  "Displays the label of the nearest case clause.
Currently limited in that it will return the name of the previous case, reegardless of whether point is currently in that case or not"

  (interactive)
  (save-excursion
    (search-backward-regexp "case \\(.*\\):")
    (message (concat "Current case: " (match-string 1)))
  )
)

;; Provide a list of all the functions in the current c file
(defvar funck-command (concat "ctags -x \"--c-kinds=f\" -f -") "The command run by the funck function.") 
(defvar funck-use-search-in-buffer-name t
"If non-nil, use the search string in the ack buffer's name.")
(define-compilation-mode funck-mode "Funck"
  "Specialization of compilation-mode for use with funck."
  nil)
(defvar funck-regexp-alist '(("^\\(.*\\) function\s *\\([0-9]*\\) \\([A-Za-z\\./_:-]*\\) \\(.*\\)" 
                              3 2 nil nil nil)))

;;'(("\\([a-z_-]*\\)\\s *\\(function\\)\\s *\\([0-9]*\\) \\([A-Za-z.:/]*\\)" 4 3)
(defun funck ()
  "Run funck, with user-specified ARGS, and collect output in a buffer.
While funck runs asynchronously, you can use the \\[next-error] command to
find the text that ack hits refer to. The command actually run is
defined by the funck-command variable."
  (interactive)
  (let (compile-command
        (compilation-error-regexp-alist funck-regexp-alist)
        (compilation-directory default-directory)
        (funck-full-buffer-name (concat "*funck-" args "*")))
;;    (save-some-buffers (not compilation-ask-about-save) nil)
    (compilation-start (concat funck-command " " args) 'funck-mode
                       (when funck-use-search-in-buffer-name
                         (function (lambda (ignore)
                                     funck-full-buffer-name)))
                      (regexp-quote args))))

(defun cfuncs-toggle-define ()
  "toggles the first #define found on the current line to #undef, or vice-versa"
  (interactive)
  (save-exc ursion
    (narrow-to-region (point-at-bol) (point-at-eol))
    (beginning-of-buffer)
    (if (search-forward "#" nil t)
        (if (search-forward "define" nil t)
            (replace-match "undef")
          (if (search-forward "undef" nil t)
              (replace-match "define")
            )))
    (widen)
    ))

;; (defun find-c-function () 
;; "Find a c function name, searching forward from current point.
;; Depends on the function body to have a brace in column 1 underneath the line holding the function name. "
;; (interactive)
;; ;; Find the function name, by finding text between a space and a (possible) space-opening bracket combo, which does not have a ; at the end
;; (if (search-forward-regexp " \\(\\w+?\\) *(.*?)[^;]*
;; {" nil t)
;;     (progn
;;       ;; Set match strng and function beginning pos to list
;;       ((setq retlist (list (match-string 1) (line-beginning-position)))
;;        ;; find the end pos of the funct, a closing brace in col 1
;;        (if (search-forward-regexp "^}" nil t)
;;            ;; match found, use match pos
;;            (append retlist (list (match-end 0)))
;;          ;; match not found, append nil
;;          (append retlist '(nil)))
;;        ))
;;   ;; No match found, return a list of nils
;;   (setq retlist '(nil nil nil)))
;; (message "%s %d %d" retlist)
;; (retlist)
;; )

(defun decorate-c-functions () 
"Decorate all c functions in active buffer , placing their names in comments at the end of each function, if there is no text there already
Depends on the function body to have a brace in column 1 underneath the line holding the function name. 
Also currently depends on there being no comments at the end of the function line"
(interactive)
;; Find the function name, by finding text between a space and a (possible) space-opening bracket combo, which does not have a ; at the end
(while (search-forward-regexp " \\([A-Za-z0-9_]+?\\) *(.*?) *?
{" nil t)
  (message (match-string 1))
  ;; Set match strng and function beginning pos to list
  ;; find the end pos of the funct, a closing brace in col 1
  (setq func (match-string 1))
  (if (search-forward-regexp "^} *$" nil t)
      ;; match found, use match pos
      (progn
      (end-of-line)
      (insert (concat " /* " func " */")))
  )
))

(defun decorate-c-functions-in-file (fpath)
"Decorate all the c functions in the provided file path"
(interactive "f")
(save-excursion)
(let (fname buf opened oldpoint)
  (setq fname (file-name-nondirectory fpath))
  (setq buf (find-file fpath))
  ;; determine in buf already open
  (if (equal (point) (point-min))
    ;; buffer not opened before
    (setq opened nil)
  (progn
    ;; buffer already open, mark as such by storing point and move to beginning of buffer
    (setq opened t)
    (setq oldpoint (point))
    (goto-char (point-min)) 
    ))
  (decorate-c-functions)
  (save-buffer)
  (if (equal opened t)
      ;; buffer was opened before, restore char
      (goto-char oldpoint)
    ;; Buffer was created for this purpose, kill it
    (kill-buffer buf))
  )
)

(defun indent-c-functions-in-file (fpath)
"Indent all the c functions in the provided file path"
(interactive "f")
(save-excursion)
(let (fname buf opened oldpoint)
  (setq fname (file-name-nondirectory fpath))
  (setq buf (find-file fpath))
  ;; determine in buf already open
  (if (equal (point) (point-min))
    ;; buffer not opened before
    (setq opened nil)
  (progn
    ;; buffer already open, mark as such by storing point and move to beginning of buffer
    (setq opened t)
    (setq oldpoint (point))
    (goto-char (point-min)) 
    ))
  (c-mode)
  (indent-region (point-min) (point-max) nil)
  (save-buffer)
  (if (equal opened t)
      ;; buffer was opened before, restore char
      (goto-char oldpoint)
    ;; Buffer was created for this purpose, kill it
    (kill-buffer buf))
  )
)

(provide 'c-funcs)