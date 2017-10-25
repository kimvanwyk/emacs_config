;; Org mode customisations

;;TO-DO Stuff
(if (require-if-exists 'org-install "org-mode/lisp/")
    (progn
      (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
      (define-key global-map "\C-ck" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (setq org-log-done t)))

(provide 'de_org)