(if (equal window-system 'x)
    (progn
      ;; W3M Support
      (add-to-list 'load-path "~/emacs/lisp/addons/emacs-w3m")
      (require-if-exists 'w3m-load)
      ))

(provide 'de_x)