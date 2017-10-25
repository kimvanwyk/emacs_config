;;(global-set-key "\C-f" 'find-name-dired)
 
(put 'dired-find-alternate-file 'disabled nil)
;;Dired stuff
;;  (require 'dired-x)
;; (load (find-file "~/emacs/lisp/addons/dired+.el"))

;; dired-single stuff
;; (if (require-if-exists 'dired-single)
;; (progn
;;   (defun my-dired-init ()
;;     "Bunch of stuff to run for dired, either immediately or when it's
;;         loaded."
;;     (define-key dired-mode-map [return] 'joc-dired-single-buffer)
;;     (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
;;     (define-key dired-mode-map "^"
;;       (function
;;        (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;;   ;; if dired's already loaded, then the keymap will be bound
;;   (if (boundp 'dired-mode-map)
;;       ;; we're good to go; just add our bindings
;;       (my-dired-init)
;;     ;; it's not loaded yet, so add our bindings to the load-hook
;;     (add-hook 'dired-load-hook 'my-dired-init))))

(global-set-key [(f3)] 'joc-dired-magic-buffer)
(global-set-key [(control f3)] (function
                                (lambda nil (interactive)
                                  (joc-dired-magic-buffer default-directory))))
(global-set-key [(shift f3)] (function
                              (lambda nil (interactive)
                                (message "Current directory is: %s" default-directory))))
(global-set-key [(meta f3)] 'joc-dired-toggle-buffer-name)

;; Hide verbose dired details by default. 
;; Via http://whattheemacsd.com//setup-dired.el-01.html
(require 'dired-details+)
(require 'dired-details)
(setq-default dired-details-hidden-string "")
(dired-details-install)

(provide 'de_dired)