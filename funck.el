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

(provide 'funck)