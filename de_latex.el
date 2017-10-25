;; Word wrap:
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
 
;; AUCTeX Support: 
;; Different to normal loading procedure as Auctex is installed via the package manager on some Unix systems and is not in addons
(if (load "auctex.el" t t t)
    (progn
      (setq TeX-auto-save t)
      (setq TeX-parse-self t)
      (setq-default TeX-master nil)))

;; Text to bold and make red the selected text, via LaTeX syntax
(defun kvw-pandoc-bold-make-red ()
(interactive)
(goto-char (region-beginning))
(insert "**\\textcolor{red}{")
(goto-char (region-end))
(insert "}**"))

;; Place <<n:XX>> around the selected XX text
(defun kvw-kppe-markup-name ()
(interactive)
(goto-char (region-end))
(insert ">>")
(goto-char (region-beginning))
(insert "<<n:")
)

;; Pandoc support
(require-if-exists 'pandoc-mode)
(provide 'de_latex)
