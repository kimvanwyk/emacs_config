(if (equal window-system 'w32)
(progn
;;define funct's to allow maximisation on startup

  (defun w32-restore-frame ()
    "Restore a minimized frame"
    (interactive)
    (w32-send-sys-command 61728))

  (defun w32-maximize-frame ()
    "Maximize the current frame"
    (interactive)
    (set-font)
    (w32-send-sys-command 61488)
    )

  (add-hook 'window-setup-hook 'w32-maximize-frame t)

  (require-if-exists 'w32-browser)
  (require-if-exists 'w32-find-dired)
  (require-if-exists 'w32shell)

  (setenv "PATH" (concat "C:\\Program Files\\gnuwin32\\bin" path-separator (getenv "PATH")))

  ;;Bind C-xC-g to w32-find-dired
  (global-set-key "\C-x\C-g" 'w32-find-dired)
  
  ;; Tramp stuff for windows:
  (setq tramp-default-method "plink")

  ;; Try to fix random hanging
  (setq w32-get-true-file-attributes nil)

  ;; Add aspell support instead of ispell
  ;; Do some kind of existance checking?
  (setq ispell-program-name "c:/utils/aspell/bin/aspell.exe")
  (setq ispell-dictionary "british")

  ;; via http://zhangda.wordpress.com/2010/02/03/open-the-path-of-the-current-buffer-within-emacs/
  (defun open-buffer-path ()
    "Run explorer on the directory of the current buffer."
     (interactive)
     (shell-command (concat "explorer " (replace-regexp-in-string "/" "\\\\" (file-name-directory (buffer-file-name)) t t))))

  ;; via http://slashusr.wordpress.com/2010/01/20/emacs-function-to-add-new-path-elements-to-the-path-environment-variable/
  (defun add-dir-to-path (path-element)
    "Add the specified path element to the Emacs PATH"
    (interactive "DEnter directory to be added to path: ")
    (if (file-directory-p path-element)
        (setenv "PATH"
                (concat (expand-file-name path-element)
                        path-separator (getenv "PATH")))))
  
  ))

(provide 'de_windows)
