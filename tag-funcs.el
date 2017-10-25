(defun tag-file-find (filename)
"Uses tag apropos to find files, escaping the regex components of the name"
(interactive (list (read-string "File name to search tag file for: ")))
(tags-apropos (regexp-quote filename))
(other-window 1)
(delete-other-windows)
(toggle-read-only)
(keep-lines "^- ")
(toggle-read-only)
;; Check to see if any buttons at all:
(if (button-at (point))
    (if (equal nil (next-button (line-end-position) 0)) 
        ;; Only one button - push it
        (progn
          (message (concat "Only one file found for " filename ". Opening."))
          (push-button)
          (kill-buffer "*Tags List*")
          ))
  ;; No buttons at all - report failure of the search
  (message (concat "No results for a search for " filename))
  (kill-buffer "*Tags List*")
  )
)

(provide 'tag-funcs)