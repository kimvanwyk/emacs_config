(defun occur-at-point()
  "invokes occur with the thing-at-point"
  (interactive)
  (if (thing-at-point 'symbol)
      (occur (thing-at-point 'symbol))
    (call-interactively 'other-window) 
    )
  ) 

(provide 'browse-funcs)