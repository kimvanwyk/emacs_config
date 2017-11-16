;; VCS customisations

;; (require-if-exists 'psvn)
;; (require-if-exists 'ahg "ahg_repo/")

;; (remove-hook 'find-file-hooks 'vc-find-file-hook)

(global-set-key (kbd "C-x g") 'magit-status)

(provide 'de_vcs)
