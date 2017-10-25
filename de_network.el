;; Network related customaisations

;; Jabber
(require-if-exists 'jabber "emacs-jabber/")

;; LJ update stuff
(require-if-exists 'ljupdate "ljupdate/")
(require-if-exists 'lj-edit "ljupdate/")

;; Twitter
(if (file-exists-p "~/emacs/lisp/addons/twitter.el")
    (progn
      (autoload 'twitter-get-friends-timeline "twitter" nil t)
      (autoload 'twitter-status-edit "twitter" nil t)
      (global-set-key "\C-c\C-tc" 'twitter-status-edit)
      (global-set-key "\C-c\C-tf" 'twitter-get-friends-timeline)
      (add-hook 'twitter-status-edit-mode-hook 'longlines-mode)))

(provide 'de_network)