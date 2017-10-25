(defun require-if-exists (req &optional path)
"Funct to require an argument if its accompanying path exists, adding that path to the load-path list if needed
The path is specified relative to the base addons folder

If the path is null, don't use a filename, relying on the filename being the same as the argument name
Returns nil if the require fails"

;; If a nil path, just require the pattern, ignoring errors
(if (not path)
    (progn
      ;;(message "no path %s" req)
      (setq fname nil))
  
  ;; Not a null path
  ;; If path exists, add to load path if not the base addons folder and require req
  (setq path (concat "~/emacs/lisp/addons/" path))
  ;;  (message "path %s" path)
  (if (not (equal (substring (file-name-directory path) -7) "addons/"))
      (add-to-list 'load-path path))
  ;; Get the filename, if there is one and it exists. Otherwise set fname to nil, to do a require based on require name
  (if (and (not (file-directory-p path)) (file-exists-p path))  
      (setq fname (file-name-nondirectory path))
    (setq fname nil)))
;; require, ignoring on error
;;  (message "fname %s" fname)
(setq reqqed (require req fname t))
(if reqqed 
    (progn
      ;; Return reqqed, as the import was a success
      ;; (message "%s was required successfully" req)
      
    reqqed)
  ;; req failed, print a warning
  (warn "Require failed for %s, using filename of %s and path of %s" req fname path))
)

(provide 'de_require-if-exists)