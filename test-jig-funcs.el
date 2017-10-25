(defun clean-delphi-ird-log ()
"Clean up a delphi IRD log, removing front matter and repalcing newlines, tabs and spaces"
(interactive)
(save-excursion)
;; Remove first 7 lines
(setq kill-whole-line t)
(beginning-of-buffer)
(kill-line 7)
;; Move to the point to delete from
(end-of-buffer)
(forward-line -1)
(forward-char 44)
;; Remove front matter
(delete-extract-rectangle (point-min) (point))
(beginning-of-buffer)
;; Delete all newline chars
(while (search-forward "
" nil t)
  (replace-match "" nil t))
;; Replace [0A], [0D], [20] and [09] as appropriate
(beginning-of-buffer)
(while (search-forward "[0A]" nil t)
   (replace-match "" nil t))
(beginning-of-buffer)
(while (search-forward "[0D]" nil t)
  (replace-match "
" nil t))
(beginning-of-buffer)
(while (search-forward "[20]" nil t)
  (replace-match " " nil t))
(beginning-of-buffer)
(while (search-forward "[09]" nil t)
  (replace-match "	" nil t))
(beginning-of-buffer)
(while (search-forward "[00]" nil t)
  (replace-match "" nil t))
)

(provide 'test-jig-funcs)