;; yasnippet
;; (require-if-exists 'yasnippet)
(yas-global-mode 1)
(setq yas/root-directory '("~/emacs/lisp/addons/kvw_yasnippets"
                          "~/emacs/lisp/addons/yasnippet/snippets/"))
(mapc 'yas/load-directory yas/root-directory)

(provide 'de_yasnippet)
