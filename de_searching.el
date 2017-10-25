;; Bring in ack
(require-if-exists 'ack-emacs)

;; ;; ack-and-a-half, from https://github.com/jhelwig/ack-and-a-half/blob/master/ack-and-a-half.el
;; (autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
;; (autoload 'ack-and-a-half "ack-and-a-half" nil t)
;; (autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
;; (autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;; ;; Create shorter aliases
;; (defalias 'ack 'ack-and-a-half)
;; (defalias 'ack-same 'ack-and-a-half-same)
;; (defalias 'ack-find-file 'ack-and-a-half-find-file)
;; (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; isearch-occur
(defun isearch-occur ()
      "Invoke `occur' from within isearch."
      (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(global-set-key (kbd "M-s") 'multi-occur-in-matching-buffers)

;; Codewright-like Grep in file
(defun occur-at-point()
  "invokes occur with the thing-at-point"
  (interactive)
  (if (thing-at-point 'symbol)
      (occur (thing-at-point 'symbol))
;;    (call-interactively 'occur)
   (call-interactively 'other-window) 
  )
)  
(global-set-key "\C-cg" 'occur-at-point)
(global-set-key (quote [C-down]) (quote next-error))
(global-set-key (quote [C-up]) (quote previous-error))

(global-set-key (quote [M-down]) (quote scroll-other-window))
(global-set-key (quote [M-up]) (quote scroll-other-window-down))

;; Use findstr as grep
(setq grep-find-command '("findstr /sn *" . 13))

(global-set-key [f4] 'rgrep) 
(provide 'de_searching)