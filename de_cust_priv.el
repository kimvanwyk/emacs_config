;; Functions for customise file and private data handling

;; Load custom file
(load "~/emacs/lisp/addons/custom.el" t)
(setq custom-file "~/emacs/lisp/addons/custom.el")

;; private-location is a variable which must already be defined
;; ~/.emacs is a good location for this
;; Load private functions and data
(if (file-exists-p private-location)
    (progn 
      (add-to-list 'load-path (file-name-directory private-location))
      (load "private.el")
      ;; Set up private twitter-mode details
      (priv-twitter-mode-setup)
      ))

(provide 'de_cust_priv)