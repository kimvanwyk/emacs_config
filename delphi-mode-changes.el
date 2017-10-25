(defun remove-double-string-from-delphi-mode()
  (defconst delphi-strings
    '(string)
    "Tokens that represent string literals.")

  (defun delphi-literal-start-pattern (literal-kind)
    ;; Returns the start pattern of the literal kind.
    (cdr (assoc literal-kind
                '((comment-single-line . "//")
                  (comment-multi-line-1 . "{")
                  (comment-multi-line-2 . "(*")
                  (string . "'")))))

  (defun delphi-literal-end-pattern (literal-kind)
    ;; Returns the end pattern of the literal kind.
    (cdr (assoc literal-kind
                '((comment-single-line . "\n")
                  (comment-multi-line-1 . "}")
                  (comment-multi-line-2 . "*)")
                  (string . "'")))))

  (defun delphi-literal-stop-pattern (literal-kind)
    ;; Returns the pattern that delimits end of the search for the literal kind.
    ;; These are regular expressions.
    (cdr (assoc literal-kind
                '((comment-single-line . "\n")
                  (comment-multi-line-1 . "}")
                  (comment-multi-line-2 . "\\*)")
                  ;; Strings cannot span lines.
                  (string . "['\n]")))))

  (defun delphi-parse-next-literal (limit)
    ;; Searches for the next literal region (i.e. comment or string) and sets the
    ;; the point to its end (or the limit, if not found). The literal region is
    ;; marked as such with a text property, to speed up tokenizing during face
    ;; coloring and indentation scanning.
    (let ((search-start (point)))
      (cond ((not (delphi-is-literal-end search-start))
             ;; We are completing an incomplete literal.
             (let ((kind (delphi-literal-kind (1- search-start))))
               (delphi-complete-literal kind limit)
               (delphi-set-text-properties
                search-start (point) (delphi-literal-text-properties kind))))

            ((re-search-forward
              "\\(//\\)\\|\\({\\)\\|\\((\\*\\)\\|\\('\\)"
              limit 'goto-limit-on-fail)
             ;; We found the start of a new literal. Find its end and mark it.
             (let ((kind (cond ((match-beginning 1) 'comment-single-line)
                               ((match-beginning 2) 'comment-multi-line-1)
                               ((match-beginning 3) 'comment-multi-line-2)
                               ((match-beginning 4) 'string)))
                   (start (match-beginning 0)))
               (delphi-set-text-properties search-start start nil)
               (delphi-complete-literal kind limit)
               (delphi-set-text-properties
                start (point) (delphi-literal-text-properties kind))))

            ;; Nothing found. Mark it as a non-literal.
            ((delphi-set-text-properties search-start limit nil)))
      (delphi-step-progress (point) "Parsing" delphi-parsing-progress-step)))
  )