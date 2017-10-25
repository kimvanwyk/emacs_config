(require 'compile)

(let project-file)
(defun delphi-compile (project)
"Run a delphi compilation, prompting for the project file location.
Saves the project file location for re-use in a recompile"
  (interactive (read-file-name "Run ack in directory: " nil "" t))
    ;; Check that it's really a file.
    (or (file-regular-p project)
        (error "delphi compile needs a file: %s" project))
    (setq project-file project)
  (let (compile-command
        (compilation-directory (file-name-directory project-file))))
  (compile compile-command)
  (save-excursion
    (set-buffer "*compilation*")
    (insert-file-contents 
    )
  
(provide 'ack-emacs)

