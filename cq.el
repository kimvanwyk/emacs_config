(defvar funclist)
(defvar cq-compilation-started 'nil)
(defun cg-init ()
  (setq compilation-finish-functions 'cq-call) ;; this function called when compilation has finished - see  compile.el
  (setq compilation-exit-message-function
        (lambda (status code msg)
          ;; If M-x compile exists with a 0
          (when (and (eq status 'exit) (zerop code))
            ;; then bury the *compilation* buffer, so that C-x b doesn't go there
            (bury-buffer "*compilation*")
            ;; and return to whatever were looking at before
            (replace-buffer-in-windows "*compilation*"))
          ;; Always return the anticipated result of compilation-exit-message-function
          (message "Compilation Error")
          (cons msg code)))
)

(defun cq-add ()
"Adds function before point to a queue.  The first function is called when there are at least two functions in the queue"
  (interactive)
  (save-excursion
    (search-backward-regexp "(\\(.*\\))")  
    (if (boundp 'funclist) 
        (add-to-list 'funclist (intern (match-string-no-properties 1)) t) 
      (setq funclist (list (intern (match-string-no-properties 1)))))
    (setq overlay-arrow-position (make-marker)) 
    (setq overlay-arrow-string "1") 
    (set-marker overlay-arrow-position (line-beginning-position) 
                (current-buffer))
           
    (message "Added %s"  (match-string-no-properties 1))
    (unless cq-compilation-started (cq-start))
    )
  )

(defun cq-add-func (func)
  "Adds function given as argument to a queue.  The first function is called when there are at least two functions in the queue"
(interactive)
  (if (boundp 'funclist) 
      (add-to-list 'funclist (func) t) 
   
 (setq funclist (list func)))
  (message "Added %s"  func)
  (unless cq-compilation-started (cq-start))
  )
  

;;(setq compilation-finish-functions 'nil)
(setq compilation-finish-functions 'cq-call) ;; this function called when compilation has finished - see  compile.el

(defun cq-call (buffer string)
  "Calls the next function in the global variable funclist"
  (if (and (boundp 'funclist) (not (eq funclist nil))) (funcall (pop funclist)) 
    (cq-after-compilation)
    (setq cq-compilation-started 'nil)) ;; while list is not nil!

  )

(defun cq-start ()
"Starts the compilation process"
(interactive)
(if (boundp 'funclist) (progn
                         (funcall (pop funclist))
                         (setq cq-compilation-started 't))
                         (message "Queue is empty!"))
)

(defun cq-display ()
"Displays the compilation queue"
(interactive)
(if (boundp 'funclist) (print funclist t))
)

(defun cq-clear ()
"Clears the compilation queue"
(interactive)
(if (boundp 'funclist)
    (progn
      (setq funclist ())
      (setq cq-compilation-started 'nil)))
)

(defun cq-mouse-add ()
(interactive)

)

(defun cq-after-compilation ()

  (setq visible-bell 'nil)
  (beep t)
  (setq visible-bell t)
)

(provide 'cq)