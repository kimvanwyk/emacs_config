;; VCS customisations

(require-if-exists 'psvn)
(require-if-exists 'ahg "ahg_repo/")

(remove-hook 'find-file-hooks 'vc-find-file-hook)

(provide 'de_vcs)
