(put 'icicle-prefix-word-complete 'icicle-completing-command t)
;;;###autoload
(defun icicle-prefix-word-complete ()   ; Bound to `M-SPC' in minibuffer.
  "Complete the minibuffer contents at most a single word.
After one word is completed as much as possible, a space or hyphen
is added, provided that matches some possible completion.
Return nil if there is no valid completion, else t.
Candidate completions are appropriate names whose prefix is the
minibuffer input, where appropriateness is determined by the context
\(command, variable, and so on).

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-prefix-word-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-input
        (if (and (symbolp last-command) (get last-command 'icicle-cycling-command))
            icicle-last-input           ; $$ Previously didn't allow the -action's.
          (icicle-minibuffer-contents-from-minibuffer)))
  (let ((return-value (minibuffer-complete-word)))
    (setq icicle-completion-candidates
          (if (icicle-file-name-input-p)
              (icicle-file-name-prefix-candidates icicle-current-input)
            (icicle-prefix-candidates icicle-current-input)))
    (when (get-buffer-window "*Completions*" 0)
      (icicle-display-candidates-in-Completions))
    (setq icicle-last-completion-command 'icicle-prefix-word-complete)
    return-value))

(put 'icicle-apropos-complete 'icicle-completing-command t)
;;;###autoload
(defun icicle-apropos-complete ()       ; Bound to `S-TAB' in minibuffer.
  "Complete the minibuffer contents as far as possible.
This uses \"apropos completion\", defined as follows:
A completion contains the minibuffer input somewhere, as a substring.
Display a list of possible completions in buffer *Completions*.
Scroll *Completions* window if this command is repeated.
Candidate completions are appropriate names that match the current
input, taken as a regular expression, where appropriateness is
determined by the context (command, variable, and so on).
Return nil if there is no valid completion.
Otherwise, return the list of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((error-msg nil)
         (candidates
          (condition-case lossage
              (icicle-apropos-complete-1)
            (invalid-regexp
             (setq error-msg (car (cdr lossage)))
             ;;$$ (setq icicle-within-brackets (string-match "\\`Unmatched \\[" error-msg))
             (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
               (setq error-msg "incomplete input")))
            (error (setq error-msg (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))

(put 'icicle-apropos-complete-no-display 'icicle-completing-command t)
;;;###autoload
(defun icicle-apropos-complete-no-display () ; Bound to `C-M-S-TAB' in minibuffer.
  "Like `icicle-apropos-complete', but without displaying *Completions*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-apropos-complete-no-display]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let* ((error-msg nil)
         (candidates
          (condition-case lossage
              (icicle-apropos-complete-1 'no-display)
            (invalid-regexp
             (setq error-msg (car (cdr lossage)))
             ;;$$ (setq icicle-within-brackets (string-match "\\`Unmatched \\[" error-msg))
             (when (string-match "\\`Premature \\|\\`Unmatched \\|\\`Invalid " error-msg)
               (setq error-msg "incomplete input")))
            (error (setq error-msg (error-message-string lossage))))))
    (when error-msg (minibuffer-message (concat "  " error-msg)))
    candidates))

(defun icicle-apropos-complete-1 (&optional no-display-p)
  "Helper function for `icicle-apropos-complete(-no-display)'.
This does everything, except deal with regexp-match errors.
Optional argument NO-DISPLAY-P non-nil means do not display buffer
*Completions*.  Return the list of completion candidates."
  (setq icicle-current-completion-mode 'apropos
        icicle-current-input           (if (and icicle-last-input (symbolp last-command)
                                                (get last-command 'icicle-cycling-command))
                                           icicle-last-input
                                         (icicle-minibuffer-contents-from-minibuffer))
        icicle-input-fail-pos          nil)
  (unless (and (stringp icicle-current-input) (stringp icicle-last-input)
               (string= icicle-current-input icicle-last-input)
               (eq last-command 'icicle-apropos-complete))
    (setq icicle-completion-candidates
          (if (icicle-file-name-input-p)
              (icicle-file-name-apropos-candidates icicle-current-input)
            (icicle-apropos-candidates icicle-current-input))))
  ;; If input matches an empty directory, then use that directory as the sole completion.
  (when (and (icicle-file-name-input-p) (null icicle-completion-candidates)
             (string-match "/$" icicle-current-input))
    (setq icicle-completion-candidates '("")))
  (icicle-save-or-restore-input)
  (cond ((null icicle-completion-candidates)
         (setq icicle-nb-of-other-cycle-candidates 0)
         (icicle-highlight-input-noncompletion (icicle-minibuffer-contents))
         (save-selected-window (icicle-remove-Completions-window))
         (minibuffer-message "  [No apropos completion]"))
        ((null (cdr icicle-completion-candidates)) ; Single candidate. Update minibuffer.
         (setq icicle-nb-of-other-cycle-candidates 0)
         (unless icicle-edit-update-p
           (icicle-clear-minibuffer)
           (setq icicle-last-completion-candidate (car icicle-completion-candidates))
           (let ((inserted (if (and (icicle-file-name-input-p) insert-default-directory)
                               (icicle-abbreviate-or-expand-file-name
                                icicle-last-completion-candidate
                                (icicle-file-name-directory-w-default icicle-current-input))
                             icicle-last-completion-candidate)))
             (insert inserted)
             (when (icicle-file-directory-p (icicle-abbreviate-or-expand-file-name inserted))
               (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                               inserted)))))
         (save-selected-window (icicle-remove-Completions-window))
         (icicle-transform-sole-candidate)
         (unless (boundp 'icicle-apropos-complete-and-exit-p)
           (icicle-highlight-complete-input)
           (if icicle-top-level-when-sole-completion-flag
               (condition-case icicle-apropos-complete-1
                   (throw 'icicle-read-top (car icicle-completion-candidates))
                 (no-catch (setq icicle-current-input (car icicle-completion-candidates))
                           (icicle-retrieve-last-input)
                           icicle-current-input)
                 (error (message (error-message-string icicle-apropos-complete-1))))
             (if icicle-edit-update-p
                 (minibuffer-message (format "  [One apropos completion: %s]"
                                             (car icicle-completion-candidates)))
               (minibuffer-message "  [Sole apropos completion]")))))
        (t                              ; Multiple candidates.
         (if icicle-edit-update-p
             (icicle-display-candidates-in-Completions nil no-display-p)
           (icicle-clear-minibuffer)
           (insert icicle-current-input) ; Update minibuffer.
           (deactivate-mark)
           (icicle-highlight-initial-whitespace icicle-current-input)
           (when (and (icicle-file-name-input-p)
                      (icicle-file-directory-p icicle-last-completion-candidate))
             (setq icicle-default-directory (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate)))
           (when (member (if (icicle-file-name-input-p)
                             (icicle-file-name-nondirectory icicle-current-input)
                           icicle-current-input)
                         icicle-completion-candidates)
             (icicle-highlight-complete-input))
           (if (get-buffer-window "*Completions*" 0)
               (if (and (eq icicle-last-completion-command 'icicle-apropos-complete)
                        (memq last-command '(icicle-apropos-complete handle-switch-frame)))
                   ;; Second `S-TAB' in a row.  Scroll window around.
                   (icicle-scroll-Completions)
                 ;; Did something else (e.g. changed input).  Update the display.
                 (icicle-display-candidates-in-Completions nil no-display-p))
             ;; No window yet.  Show window.
             (icicle-display-candidates-in-Completions nil no-display-p)))))
  (setq icicle-last-completion-command 'icicle-apropos-complete)
  icicle-completion-candidates)

(defun icicle-transform-sole-candidate ()
  "Transform matching candidate according to `icicle-list-use-nth-parts'."
  (when (and icicle-list-use-nth-parts icicle-list-join-string)
    (let ((newcand (icicle-transform-multi-completion (car icicle-completion-candidates))))
      (icicle-clear-minibuffer)
      (insert newcand)
      (setq icicle-completion-candidates      (list newcand)
            icicle-last-completion-candidate  newcand))))

(defun icicle-transform-multi-completion (candidate)
  "Transform CANDIDATE according to `icicle-list-use-nth-parts'.
If CANDIDATE is not a multi-completion, do nothing.
Return the possibly transformed candidate."
  (if (and icicle-list-use-nth-parts icicle-list-join-string)
      (let* ((parts (split-string candidate icicle-list-join-string))
             (maxpart (length parts))
             (indexes icicle-list-use-nth-parts)
             (cand "")
             (firstp t)
             partnum)
        (while indexes
          (setq partnum (car indexes))
          (when (> partnum maxpart) (setq partnum maxpart))
          (unless firstp (setq cand (concat cand icicle-list-nth-parts-join-string)))
          (setq firstp nil)
          (setq cand (concat cand (nth (1- partnum) parts)))
          (setq indexes (cdr indexes)))
        cand)
    candidate))

;;;###autoload
(defun icicle-switch-to-Completions-buf () ; Bound to `C-insert' in minibuffer.
  "Select the completion list window.
The cursor is placed on the first occurrence of the current minibuffer
content.  You can use \\<completion-list-mode-map>\
`\\[icicle-insert-completion]' to get back to the minibuffer.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-switch-to-Completions-buf]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))
  (let ((window (get-buffer-window "*Completions*" 0))
        (search-fn 'search-forward))
    (unless window                      ; Make sure we have a completions window.
      (icicle-apropos-complete)
      (setq window (get-buffer-window "*Completions*" 0)
            search-fn 're-search-forward)) ; Use regexp search: input is not yet complete.
    (when window
      (select-window window)
      (let ((case-fold-search (if (and (icicle-file-name-input-p)
                                       (boundp 'read-file-name-completion-ignore-case))
                                  read-file-name-completion-ignore-case
                                completion-ignore-case)))
        (goto-char (icicle-start-of-candidates-in-Completions))
        (when (icicle-file-name-input-p)
          (setq icicle-current-input (icicle-file-name-nondirectory icicle-current-input)))
        (when (and (eq icicle-last-completion-command 'icicle-apropos-complete)
                   ;; $$ Previously allowed the -action's.
                   (not (and (symbolp last-command) (get last-command 'icicle-cycling-command))))
          (setq search-fn 're-search-forward)) ; Use regexp search: input is not yet complete.
        (while (and (not (eobp))
                    (save-restriction
                      (narrow-to-region (point) (next-single-property-change (point) 'mouse-face
                                                                             nil (point-max)))
                      (not (funcall search-fn icicle-current-input nil 'leave-at-end)))))
        (unless (eobp)
          (goto-char (match-beginning 0))
          (let ((prop (get-text-property (1- (point)) 'mouse-face)))
            ;; If in a completion, move to the start of it.
            (when (and prop (eq prop (get-text-property (point) 'mouse-face)))
              (goto-char (previous-single-property-change (point) 'mouse-face nil (point-min)))))
          (icicle-place-overlay
           (point) (next-single-property-change (point) 'mouse-face nil (point-max))
           'icicle-current-completion-candidate-overlay 'icicle-current-candidate-highlight
           100 (current-buffer)))))))

;;;###autoload
(defun icicle-insert-completion (&optional completion) ; Bound to `C-insert' in *Completions*.
  "Select the active minibuffer window.  Insert current completion.
The current candidate in *Completions* (under the cursor) is inserted
into the minibuffer as the current input.  You can use \\<minibuffer-local-completion-map>\
`\\[icicle-switch-to-Completions-buf]'
to switch to the *Completions* window.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-insert-completion]').

Non-interactively, optional arg COMPLETION is the completion inserted."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (when (active-minibuffer-window)
    (unwind-protect                     ; If no current completion, return to minibuffer anyway.
         (progn
           (setq completion (or completion (icicle-transform-multi-completion
                                            (icicle-current-completion-in-Completions))))
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if *Completions* is redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)
             (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                         (icicle-file-name-directory-w-default icicle-current-input)
                       "")
                     completion)
             (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))))
      (select-window (active-minibuffer-window)))))

(defun icicle-current-completion-in-Completions ()
  "The completion candidate under the cursor in buffer *Completions*.
Return the name as a string."           ; This code comes from `choose-completion'.
  (let ((buffer completion-reference-buffer)
        (base-size completion-base-size)
        beg end completion)
    (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
      (setq end (point) beg (1+ (point))))
    (when (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
      (setq end (1- (point)) beg (point)))
    (when (null beg) (error "No completion here"))
    (setq beg (or (previous-single-property-change beg 'mouse-face) (point-min))
          end (or (next-single-property-change end 'mouse-face) (point-max)))
    (buffer-substring beg end)))

;;;###autoload
(defun icicle-switch-to/from-minibuffer () ; Bound to `pause' in Icicle mode.
  "Switch to minibuffer or previous buffer, in other window.
If current buffer is the minibuffer, then switch to the buffer that
was previously current.  Otherwise, switch to the minibuffer."
  (interactive)
  (unless (active-minibuffer-window) (error "Minibuffer is not active"))  
  (if (eq (selected-window) (active-minibuffer-window))
      (switch-to-buffer-other-window icicle-pre-minibuffer-buffer)
    (select-window (active-minibuffer-window))))


;; Replaces `previous-completion' (defined in `simple.el').
;;;###autoload
(defun icicle-move-to-previous-completion (n) ; Bound to `left', `S-TAB' in *Completions*.
  "Move to the previous item in the completion list.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-move-to-previous-completion]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (setq n (or n 0))
  (icicle-move-to-next-completion (- n)))


;; Replaces `next-completion' (defined in `simple.el').
;; This is the same code, except:
;; 1. This highlights the current candidate.
;; 2. This wraps around from first to last and last to first.
;;;###autoload
(defun icicle-move-to-next-completion (n &optional no-minibuffer-follow-p) ; Bound to `right', `TAB'
                                        ;  in *Completions*.
  "Move to the next item in the completion list.
With prefix argument N, move N items (negative N means move backward).
Optional second argument, if non-nil, means do not copy the completion
back to the minibuffer.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-move-to-next-completion]')."
  (interactive "p")
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (setq n (or n 0))
  (let ((beg (icicle-start-of-candidates-in-Completions))
        (end (point-max)))
    (while (and (> n 0) (not (eobp)))
      ;; If in a completion, move to the end of it.
      (when (get-text-property (point) 'mouse-face)
        (goto-char (next-single-property-change (point) 'mouse-face nil end)))
      ;; Move to start of next one.
      (unless (get-text-property (point) 'mouse-face)
        (goto-char (or (next-single-property-change (point) 'mouse-face)
                       beg)))           ; Wrap back to first candidate.
      (setq n (1- n)))
    (while (and (< n 0) (>= (count-lines 1 (point)) 3))
      (let ((prop (get-text-property (1- (point)) 'mouse-face)))
        ;; If in a completion, move to the start of it.
        (when (and prop (eq prop (get-text-property (point) 'mouse-face)))
          (goto-char (previous-single-property-change (point) 'mouse-face nil beg))))
      ;; Move to end of the previous completion.
      (unless (or (< (count-lines 1 (point)) 3)
                  (get-text-property (1- (point)) 'mouse-face))
        (goto-char (or (previous-single-property-change (point) 'mouse-face)
                       end)))           ; Wrap back to last candidate.
      ;; Move to the start of that one.
      (goto-char (previous-single-property-change (point) 'mouse-face nil beg))
      (setq n (1+ n)))
    (icicle-place-overlay
     (point) (next-single-property-change (point) 'mouse-face nil end)
     'icicle-current-completion-candidate-overlay 'icicle-current-candidate-highlight
     100 (current-buffer)))
  (unless no-minibuffer-follow-p
    (save-excursion (save-window-excursion (icicle-insert-completion)))))

;;;###autoload
(defun icicle-previous-line ()          ; Bound to `up' *Completions*.
  "Move up a line, in *Completions* buffer.  Wrap around first to last.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-previous-line]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (let ((bolp-at-start (bolp)))
    (if (> (count-lines 1 (point)) (if bolp-at-start 3 4))
        (icicle-move-to-previous-completion 2)
      (goto-char (point-max))
      (icicle-move-to-previous-completion 1)
      (if bolp-at-start
          (while (not (bolp)) (icicle-move-to-previous-completion 1))
        (while (bolp) (icicle-move-to-previous-completion 1))))))

;;;###autoload
(defun icicle-next-line ()              ; Bound to `down' in *Completions*.
  "Move down a line, in *Completions* buffer.  Wrap around last to first.

You can use this command only from buffer *Completions* (`\\<completion-list-mode-map>\
\\[icicle-next-line]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions))
  (let ((num-lines (- (count-lines (point-min) (point-max)) 1))
        (bolp-at-start (bolp)))
    (cond ((< (count-lines 1 (point)) (if bolp-at-start num-lines (1+ num-lines)))
           (icicle-move-to-next-completion 2)
           (when (and (bolp) (not bolp-at-start)) (icicle-move-to-next-completion 1)))
          (t
           (goto-char (point-min))
           (icicle-move-to-next-completion 1)
           (if bolp-at-start
               (while (not (bolp))
                 (icicle-move-to-next-completion 1))
             (while (bolp) (icicle-move-to-next-completion 1)))))))

;;;###autoload
(defun icicle-all-candidates-action ()  ; Bound to `C-!' in minibuffer.
  "Take action on all completion candidates.
Apply `icicle-candidate-action-fn' to each completion candidate that
matches the current input (a regular expression), successively.
The candidates that were not successfully acted upon are listed in
buffer *Help*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (unless (or icicle-all-candidates-action-fn icicle-candidate-action-fn)
    (error "No action defined."))
  (icicle-all-candidates-action-1 (or icicle-all-candidates-action-fn icicle-candidate-action-fn)
                                  icicle-all-candidates-action-fn))

;;;###autoload
(defun icicle-all-candidates-alt-action () ; Bound to `C-S-insert' in minibuffer.
  "Take alternative action on all completion candidates.
Apply `icicle-candidate-alternative-action-fn' to each completion
andidate that matches the current input (a regular expression),
successively.  The candidates that were not successfully acted upon
are listed in buffer *Help*.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-all-candidates-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (unless (or icicle-all-candidates-alternative-action-fn icicle-candidate-alternative-action-fn)
    (error "No alternative action defined."))
  (icicle-all-candidates-action-1 (or icicle-all-candidates-alternative-action-fn
                                      icicle-candidate-alternative-action-fn)
                                  icicle-all-candidates-alternative-action-fn))

(defun icicle-all-candidates-action-1 (fn-var listp)
  "Helper function for `icicle-all-candidates(-alt)-action'."
  (let ((candidates icicle-completion-candidates)
        (failures nil)
        (icicle-all-candidates-action-p t))
    (if listp
        (funcall fn-var candidates)
      (while candidates
        (let ((error-msg (condition-case act-on-each
                             (funcall fn-var (car candidates))
                           (error (error-message-string act-on-each)))))
          (when error-msg (setq failures (cons (cons (car candidates) error-msg) failures)))
          (setq candidates (cdr candidates))))
      (when failures
        (with-output-to-temp-buffer "*Help*"
          (princ "Action failures:")(terpri)(terpri)
          (mapcar (lambda (entry)
                    (princ (car entry)) (princ ":") (terpri) (princ "  ")
                    (princ (cdr entry)) (terpri))
                  failures)))))
  (icicle-abort-minibuffer-input))

;;;###autoload
(defun icicle-candidate-action ()       ; Bound to `C-RET' and `C-o' in minibuffer.
  "Take action on the current minibuffer-completion candidate.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the current candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-candidate-action-1 icicle-candidate-action-fn))

;;;###autoload
(defun icicle-candidate-alt-action ()   ; Bound to `C-S-RET' in minibuffer.
  "Take alternative action on the current completion candidate.
If `icicle-candidate-alternative-action-fn' is non-nil, it is a
function to apply to the current candidate, to perform the action.

If `icicle-candidate-alternative-action-fn' is nil, the default action
is performed: display help on the candidate - see
`icicle-help-on-candidate'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-alt-action]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-candidate-action-1 icicle-candidate-alternative-action-fn 'alternative-p))

;;$$$ (defun icicle-candidate-action-1 (fn-var &optional altp)
;;   "Helper function for `icicle-candidate(-alt)-action'."
;;   ;; If no last candidate, then reset to first candidate matching input.
;;   (unless (stringp icicle-last-completion-candidate)
;;     (setq icicle-last-completion-candidate icicle-current-input)
;;     (setq last-command (if altp 'icicle-candidate-alt-action 'icicle-candidate-action))
;;     (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
;;                                  'icicle-prefix-candidates
;;                                'icicle-apropos-candidates)
;;                            (not (eq icicle-current-completion-mode 'prefix))))
;;   (if (not fn-var)
;;       (icicle-help-on-candidate)
;;     (funcall fn-var icicle-last-completion-candidate)
;;     (icicle-raise-Completions-frame)))

(defun icicle-candidate-action-1 (fn-var &optional altp)
  "Helper function for `icicle-candidate(-alt)-action'."
  (cond ((not fn-var) (icicle-help-on-candidate))
        (icicle-require-match-p
         ;; $$$ Should we do this if `icicle-last-completion-candidate' is not yet a string?
         (unless nil ;; $$$ (stringp icicle-last-completion-candidate)
           (setq icicle-last-completion-candidate icicle-current-input)
           (setq last-command (if altp 'icicle-candidate-alt-action 'icicle-candidate-action))
           (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                        'icicle-prefix-candidates
                                      'icicle-apropos-candidates)
                                  (not (eq icicle-current-completion-mode 'prefix))))
         (if (not fn-var)
             (icicle-help-on-candidate)
           (when icicle-completion-candidates (funcall fn-var icicle-last-completion-candidate))
           (icicle-raise-Completions-frame)))
        (t
         (let ((icicle-last-input (icicle-minibuffer-contents-from-minibuffer))
               (icicle-default-directory icicle-default-directory))
           (when (and (icicle-file-name-input-p) (icicle-file-directory-p icicle-last-input))
             (setq icicle-default-directory icicle-last-input))
           (funcall fn-var icicle-last-input)
           (when (and icicle-use-candidates-only-once-flag
                      (equal icicle-last-input
                             (if (icicle-file-name-input-p)
                                 (expand-file-name icicle-last-completion-candidate
                                                   (file-name-directory icicle-last-input))
                               icicle-last-completion-candidate)))
             (icicle-remove-candidate-display-others))
           (icicle-raise-Completions-frame)))))

;;;###autoload
(defun icicle-mouse-candidate-action (event) ; Bound to `C-down-mouse-2' in *Completions*.
  "Take action on the completion candidate clicked by `mouse-2'.
If `icicle-candidate-action-fn' is non-nil, it is a function to apply
to the clicked candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive "e")
  (icicle-mouse-candidate-action-1 event icicle-candidate-action-fn))

;;;###autoload
(defun icicle-mouse-candidate-alt-action (event) ; Bound to `C-S-down-mouse-2' in *Completions*.
  "Take alternative action on the candidate clicked by `mouse-2'.
If `icicle-candidate-alternative-action-fn' is non-nil, it is a
function to apply to the clicked candidate, to perform the action.

If `icicle-candidate-action-fn' is nil, the default action is
performed: display help on the candidate - see
`icicle-help-on-candidate'."
  (interactive "e")
  (icicle-mouse-candidate-action-1 event icicle-candidate-alternative-action-fn))

(defun icicle-mouse-candidate-action-1 (event fn-var)
  "Helper function for `icicle-mouse-candidate(-alt)-action'."
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    (read-event)                        ; Swallow mouse up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (icicle-transform-multi-completion (buffer-substring beg end))))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event)))
          icicle-last-completion-candidate choice)
    (if (not fn-var)
        (icicle-help-on-candidate)
      (funcall fn-var icicle-last-completion-candidate)
      (when icicle-use-candidates-only-once-flag (icicle-remove-candidate-display-others))
      (icicle-raise-Completions-frame posn-col posn-row))))

;;;###autoload
(defun icicle-remove-candidate ()       ; Bound to `delete' in minibuffer during completion.
  "Remove current completion candidate from the set of candidates.
This has no effect on the object, if any, represented by the
candidate; in particular, that object is not deleted.

All candidates that have the same appearance in *Completions* as the
current candidate are also removed.  Thus, for purposes of this
command, candidates are distinguished only by the strings that
represent them in *Completions*, not by any additional information
that might be associated with these strings.

Note: For Emacs versions prior to 22, this does not really remove a
file-name candidate as a possible candidate.  If you use \\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete] or \\[icicle-apropos-complete],
it will reappear as a possible candidate.

You can use this command only from the minibuffer (`\\[icicle-remove-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (icicle-remove-candidate-display-others))

;;;###autoload
(defun icicle-mouse-remove-candidate (event) ; Bound to `S-mouse-2' in *Completions*.
  "Remove clicked completion candidate from the set of candidates.
This has no effect on the object, if any, represented by the
candidate; in particular, that object is not deleted.

See `icicle-remove-candidate' for more information."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    (read-event)                        ; Swallow mouse up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (icicle-transform-multi-completion
                        (buffer-substring-no-properties beg end)))))))
  (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event))))
  (icicle-remove-candidate-display-others))

(defun icicle-remove-candidate-display-others ()
  "Remove current completion candidate from list of possible candidates.
Redisplay *Completions*, unless there is only one candidate left.
Note: This actually removes all candidates that look the same."
  (setq icicle-last-completion-candidate
        (elt icicle-completion-candidates (or icicle-candidate-nb (setq icicle-candidate-nb 0))))
  (setq icicle-completion-candidates    ; Delete candidate from `icicle-completion-candidates'.
        (delete icicle-last-completion-candidate icicle-completion-candidates))
  ;;$$  (setq minibuffer-completion-table ; This would work too, but only for an alist value.
  ;;      (icicle-assoc-delete-all icicle-last-completion-candidate minibuffer-completion-table)))

  ;; Update predicate to effectively remove this candidate from those possible.
  (cond ((and (icicle-file-name-input-p)
              (boundp 'read-file-name-predicate) read-file-name-predicate) ; Emacs 22+ only.
         (setq read-file-name-predicate
               `(lambda (fname)
                 (and (if ',read-file-name-predicate (funcall ',read-file-name-predicate fname) t)
                  (not (string= ,icicle-last-completion-candidate fname))))))
        ;; Do nothing for file name if < Emacs 22. `TAB' or `S-TAB' will bring it back as a candidate.
        ((not (icicle-file-name-input-p))
         (setq minibuffer-completion-predicate
               `(lambda (cand)
                 (and (if ',minibuffer-completion-predicate
                          (funcall ',minibuffer-completion-predicate cand)
                        t)
                  (not (string=         ; We don't treat hash table `minibuffer-completion-table' yet.
                        ,icicle-last-completion-candidate
                        (if (symbolp cand) (symbol-name cand) (car cand)))))))))
  (when icicle-candidates-alist         ; Delete candidate from `icicle-candidates-alist'.
    (setq icicle-candidates-alist (icicle-filter-alist icicle-candidates-alist
                                                       icicle-completion-candidates)))
  (cond ((and icicle-completion-candidates (cdr icicle-completion-candidates)) ; > 1 left.
         (icicle-maybe-sort-and-strip-candidates)
         (message "Displaying completion candidates...")
         (with-output-to-temp-buffer "*Completions*" (icicle-display-candidates-in-Completions))
         (with-current-buffer "*Completions*"
           (goto-char (icicle-start-of-candidates-in-Completions))
           (icicle-move-to-next-completion
            (mod icicle-candidate-nb (length icicle-completion-candidates)))
           (set-window-point (get-buffer-window "*Completions*" 0) (point))
           (setq icicle-last-completion-candidate (icicle-current-completion-in-Completions))
           (set-buffer-modified-p nil)))
        (icicle-completion-candidates   ; Single candidate left
         (save-selected-window (icicle-remove-Completions-window))
         (let ((completion (icicle-transform-multi-completion
                            (car icicle-completion-candidates))))
           (select-window (active-minibuffer-window))
           (with-current-buffer (window-buffer) ; Needed if *Completions* redirected to minibuffer.
             (goto-char (icicle-minibuffer-prompt-end))
             (icicle-clear-minibuffer)
             (insert (if (and (icicle-file-name-input-p) insert-default-directory)
                         (icicle-file-name-directory-w-default icicle-current-input)
                       "")
                     completion))))
        (t                              ; No candidates left
         (select-window (active-minibuffer-window))
         (with-current-buffer (window-buffer) ; Needed if *Completions* redirected to minibuffer.
           (goto-char (icicle-minibuffer-prompt-end))
           (icicle-clear-minibuffer)))))

(put 'icicle-delete-candidate-object 'icicle-cycling-command t)
(put 'icicle-delete-candidate-object 'icicle-prefix-cycling-command t)
(put 'icicle-delete-candidate-object 'icicle-apropos-cycling-command t)
;;;###autoload
(defun icicle-delete-candidate-object () ; Bound to `S-delete' in minibuffer.
  "Delete the object named by the current completion candidate.
If the value of variable `icicle-delete-candidate-object' is a
function, then apply it to the current completion candidate.  This
should delete some object named by the completion candidate.

If `icicle-delete-candidate-object' is not a function, then it should
be a symbol bound to an alist.  In this case, invoke
`icicle-delete-candidate-object' to delete the object named by the
current completion candidate from that alist.

Note: For convenience in cycling, any other candidate objects that
have the same name as the deleted object are removed from
*Completions*, as if you had hit `delete' (`icicle-remove-candidate'),
even though they are not deleted.  To show any remaining objects with
this name, use `TAB', `S-TAB', or `M-k' to refresh the *Completions*
display.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-delete-candidate-object]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  ;; If no last candidate, then reset to first candidate matching input.
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate icicle-current-input)
    (setq last-command 'icicle-delete-candidate-object)
    (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                 'icicle-prefix-candidates
                               'icicle-apropos-candidates)
                           (not (eq icicle-current-completion-mode 'prefix))))
  (let ((cand-to-delete icicle-last-completion-candidate)) ; Use local var: value might change.
    (save-selected-window
      (if (functionp icicle-delete-candidate-object)
          (funcall icicle-delete-candidate-object icicle-last-completion-candidate)
        (icicle-delete-current-candidate-object)))
    (icicle-remove-candidate-display-others)
    (message "Deleted object named: `%s'" cand-to-delete) (sleep-for 1.5))
  (select-frame-set-input-focus (window-frame (minibuffer-window))))

(defun icicle-delete-current-candidate-object ()
  "Delete the object(s) corresponding to the current completion candidate.
The value of `icicle-delete-candidate-object' must be a symbol
\(variable) that is bound to a list of completion-candidate objects.

The entries in the list must be completion candidates for the current
call to `completing-read', but the list itself need not be the TABLE
argument to `completing-read'.  For example, the list might be a list
of symbols, and the TABLE argument might be an obarray that contains
those symbols.

The list can be an alist, a list of strings, or a list of symbols.
Delete, from this list, the object that corresponds to the current
completion candidate.  If the variable is also a user option, then
save the option, after deleting the candidate object.

If `icicle-candidates-alist' is nil and the variable value is an
alist, then delete all entries that match the current candidate.

If `icicle-candidates-alist' is non-nil, then it means that the
current command allows multiple candidates with the same name, their
order in *Completions* is significant, and users cannot change that
order.  This order is used by `icicle-get-alist-candidate' to
determine which candidate object to delete.  Cycling or clicking
`mouse-2' specifies a unique candidate, but if completion is used
instead, and the completion matches more than one candidate, then an
error is raised.

Note that the full candidate object is what is deleted.  Therefore, do
not use this with multi-completions, unless the alist itself has
corresponding multi-completion entries."
  (let ((val (and (symbolp icicle-delete-candidate-object)
                  (symbol-value icicle-delete-candidate-object))))
    ;; The message could instead say "Value of `icicle-delete-candidate-object' must be a symbol
    ;; bound to a list", but this makes more sense.
    (unless (and val (consp val)) (error "Cannot delete candidate objects now"))
    (set icicle-delete-candidate-object ; Update the variable.
         (cond ((and icicle-candidates-alist (consp (car val)))
                (delete (icicle-get-alist-candidate icicle-last-completion-candidate) val))
               ((consp (car val))
                (icicle-assoc-delete-all icicle-last-completion-candidate val))
               ((stringp (car val)) (delete icicle-last-completion-candidate val))
               ((symbolp (car val)) (delete (intern icicle-last-completion-candidate) val))
               (t (error "Entry in list value of `icicle-delete-candidate-object' is \
not a cons, string, or symbol")))))
  (when (user-variable-p icicle-delete-candidate-object) ; Save the new user-option value.
    (customize-save-variable icicle-delete-candidate-object
                             (symbol-value icicle-delete-candidate-object))))

;;;###autoload
(defun icicle-mouse-help-on-candidate (event) ; Bound to `C-M-mouse-2' in minibuffer.
  "Display help on the minibuffer-completion candidate clicked by mouse."
  (interactive "e")
  (let ((icicle-candidate-action-fn nil)) (icicle-mouse-candidate-action event)))

;;;###autoload
(defun icicle-help-on-candidate ()      ; Bound to `C-M-RET', `C-help', and `C-f1' in minibuffer.
                                        ; Bound to `C-M-RET' in *Completions.
  "Display help on the current minibuffer-completion candidate.
The help displayed depends on the type of candidate, as follows:

 menu item - the corresponding command is described using
             `describe-function' (available only if `icicles-menu.el'
             is loaded)
 command or other function - described using `describe-function'
 user option or other variable - described using `describe-variable'
 face - described using `describe-face'
 property list - described using `apropos-describe-plist'
 buffer name - modes described using `describe-mode' (Emacs > 20)
 file name - file properties described

In the minibuffer, you can also use `\\<minibuffer-local-completion-map>\
\\[icicle-help-on-next-apropos-candidate]', `\\[icicle-help-on-previous-apropos-candidate]',
`\\[icicle-help-on-next-prefix-candidate]', and \
`\\[icicle-help-on-previous-prefix-candidate]', to display help on the candidate and then
move to the next or previous candidate.  See, for example,
`icicle-help-on-next-apropos-candidate'.
\
You can use this command only from the minibuffer or *Completions*
\(`\\[icicle-help-on-candidate]')."
  (interactive)                         ; Interactively, just describes itself.
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let ((frame-with-focus (selected-frame))
        cand-symb)
    (if (eq (current-buffer) (get-buffer "*Completions*"))
        (setq cand-symb (intern-soft (icicle-transform-multi-completion
                                      (icicle-current-completion-in-Completions))))

      ;; If no last candidate, then reset to first candidate matching input.
      (unless (stringp icicle-last-completion-candidate)
        (setq icicle-last-completion-candidate icicle-current-input)
        (setq last-command 'icicle-help-on-candidate)
        (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                     'icicle-prefix-candidates
                                   'icicle-apropos-candidates)
                               (not (eq icicle-current-completion-mode 'prefix))))
      (setq cand-symb (intern-soft (icicle-transform-multi-completion
                                    icicle-last-completion-candidate))))

    ;; If this is a call to `icicle-execute-menu-command' (defined in `icicles-menu.el'), then 
    ;; use command associated with a menu item.  `icicle-menu-items-alist' is set in
    ;; `icicles-menu.el'.  If non-nil, then `icicle-execute-menu-command' is being called.
    (when (consp icicle-menu-items-alist)
      (setq cand-symb (cdr (assoc icicle-last-completion-candidate icicle-menu-items-alist)))
      (unless (symbolp cand-symb) (setq cand-symb nil))) ; Menu item with lambda definition.

    ;; If this is a key-completion candidate, then get the true command from the candidate.
    (when (boundp 'icicle-completing-keys-p)
      (string-match "\\(.+\\)  =  \\(.+\\)" icicle-last-completion-candidate)
      (setq cand-symb (intern-soft (substring icicle-last-completion-candidate
                                              (match-beginning 2) (match-end 2)))))

    ;; Provide the help appropriate for the given type of candidate.
    (if cand-symb
        (icicle-help-on-candidate-symbol cand-symb)
      ;; Describe buffer's mode or a file's properties.  Otherwise, create a symbol and try again.
      (cond ((and (bufferp (get-buffer icicle-last-completion-candidate))
                  (with-current-buffer (get-buffer icicle-last-completion-candidate)
                    (describe-mode)
                    t)))
            ((file-exists-p icicle-last-completion-candidate)
             (icicle-describe-file icicle-last-completion-candidate))
            (t
             (setq cand-symb (intern icicle-last-completion-candidate)) ; Hard intern.
             (icicle-help-on-candidate-symbol cand-symb))))
    (icicle-raise-Completions-frame)
    ;; This is a hack for MS Windows - otherwise, we can't continue to get more candidates,
    ;; because the *Help* frame takes the focus away from the minibuffer frame.
    ;; MS Windows always gives focus to a newly created frame - in this case, *Help*.
    (let* ((help-window (get-buffer-window "*Help*" 0))
           (help-frame (and help-window (window-frame help-window))))
      (when help-frame (redirect-frame-focus help-frame frame-with-focus))))
  (message nil))                        ; Let minibuffer contents show immmediately.

(defun icicle-help-on-candidate-symbol (symb)
  "Helper function for `icicle-help-on-candidate'.  The arg is a symbol."
  (cond (icicle-candidate-help-fn (funcall icicle-candidate-help-fn (symbol-name symb)))
        ((functionp symb) (describe-function symb))
        ((boundp symb) (describe-variable symb))
        ((facep symb) (describe-face symb))
        ((symbol-plist symb) (apropos-describe-plist symb))
        (t
         (setq symb (symbol-name symb)) ; Convert symbol to string, and try some more.
         (cond ((and (bufferp (get-buffer symb))
                     (with-current-buffer (get-buffer symb) (describe-mode) t)))
               ((file-exists-p symb) (icicle-describe-file symb))
               (t (icicle-msg-maybe-in-minibuffer "No help"))))))

;; This is the same as `describe-file' in `misc-cmds.el', but we avoid requiring that library.
;;;###autoload
(if (and (not (fboundp 'icicle-describe-file)) (fboundp 'describe-file))
    (fset 'icicle-describe-file (symbol-function 'describe-file))
  (defun icicle-describe-file (filename) ; Suggestion: bind to `C-h M-f'.
    "Describe the file named FILENAME."
    (interactive "FDescribe file: ")
    (help-setup-xref (list #'icicle-describe-file filename) (interactive-p))
    (let ((attrs (file-attributes filename)))
      (if (null attrs)
          (icicle-msg-maybe-in-minibuffer (format "Cannot open file `%s'" filename))
        (let ((type            (nth 0 attrs))
              (numlinks        (nth 1 attrs))
              (uid             (nth 2 attrs))
              (gid             (nth 3 attrs))
              (last-access     (nth 4 attrs))
              (last-mod        (nth 5 attrs))
              (last-status-chg (nth 6 attrs))
              (size            (nth 7 attrs))
              (permissions     (nth 8 attrs))
              ;; Skip 9: t iff file's gid would change if file were deleted and recreated.
              (inode           (nth 10 attrs))
              (device          (nth 11 attrs)))
          (save-excursion
            (with-output-to-temp-buffer "*Help*"
              (princ (format "Properties of `%s':\n\n" filename))
              (princ (format "Type:                       %s\n"
                             (cond ((eq t type) "Directory")
                                   ((stringp type) (format "Symbolic link to `%s'" type))
                                   (t "Normal file"))))
              (princ (format "Permissions:                %s\n" permissions))
              (unless (eq t type) (princ (format "Size in bytes:              %g\n" size)))
              (princ (format "Time of last access:        %s\n" last-access))
              (princ (format "Time of last modification:  %s\n" last-mod))
              (princ (format "Time of last status change: %s\n" last-status-chg))
              (princ (format "Number of links:            %d\n" numlinks))
              (princ (format "User ID (UID):              %s\n" uid))
              (princ (format "Group ID (GID):             %s\n" gid))
              (princ (format "Inode:                      %s\n" inode))
              (princ (format "Device number:              %s\n" device))
              (princ "\n\n")
              (print-help-return-message)
              (with-current-buffer standard-output (buffer-string))))))))) ; Return displayed text.

;;;###autoload
(defun icicle-candidate-read-fn-invoke () ; Bound to `M-RET' in minibuffer.
  "Read function name.  Invoke function on current completion candidate.
Set `icicle-candidate-action-fn' to the interned name.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-read-fn-invoke]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  ;; If no last candidate, then reset to first candidate matching input.
  (unless (stringp icicle-last-completion-candidate)
    (setq icicle-last-completion-candidate icicle-current-input)
    (setq last-command 'icicle-candidate-action)
    (icicle-next-candidate 1 (if (eq icicle-current-completion-mode 'prefix)
                                 'icicle-prefix-candidates
                               'icicle-apropos-candidates)
                           (not (eq icicle-current-completion-mode 'prefix))))
  (let ((enable-recursive-minibuffers t)
        (icicle-saved-completion-candidate icicle-last-completion-candidate)
        (icicle-candidate-action-fn 'icicle-apply-to-saved-candidate))
    (icicle-apply-to-saved-candidate
     (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                      obarray 'functionp))))

;;;###autoload
(defun icicle-mouse-candidate-read-fn-invoke (event) ; Bound to `M-mouse-2' in *Completions*.
  "Read function name.  Invoke function on candidate clicked by mouse."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    ;; (read-event)                 ; Swallow mouse up event. $$ Not needed if bound to up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event)))
          icicle-last-completion-candidate choice)
    (let ((enable-recursive-minibuffers t)
          (icicle-saved-completion-candidate icicle-last-completion-candidate)
          (icicle-candidate-action-fn 'icicle-apply-to-saved-candidate))
      (icicle-apply-to-saved-candidate
       (completing-read (format "Function to apply to `%s': " icicle-saved-completion-candidate)
                        obarray 'functionp)))))

(defun icicle-apply-to-saved-candidate (function &optional use-icicle-candidates-alist-p)
  "Apply FUNCTION to `icicle-saved-completion-candidate'.
If `current-prefix-arg' is non-nil, then pretty-print the result using
`pp-eval-expression'."
  (let ((real-fn (if use-icicle-candidates-alist-p
                     (cdr (icicle-get-alist-candidate function))
                   (car (read-from-string function)))))
    ;; Actually, we should test more than `functionp', to rule out macros and special forms.
    (unless (functionp real-fn) (error "Not a function: `%S'" real-fn))
    (condition-case icicle-candidate-read-fn-invoke
        (if current-prefix-arg
            (pp-eval-expression '(funcall real-fn icicle-saved-completion-candidate))
          (funcall real-fn icicle-saved-completion-candidate))
      (error (message  (format "ERROR invoking `%S' on `%s': %s" real-fn
                               icicle-saved-completion-candidate
                               (error-message-string icicle-candidate-read-fn-invoke)))
             (sleep-for 6)))
    (select-frame-set-input-focus (window-frame (minibuffer-window)))
    (icicle-raise-Completions-frame)))

(defun icicle-raise-Completions-frame (&optional mouse-col mouse-row)
  "Raise *Completions* frame, if displayed.
This helps keep *Completions* on top.

If `icicle-Completions-frame-at-right-flag' is non-nil and
*Completions* is in its own frame, then move that frame to the right,
out of the way.

Non-nil optional args MOUSE-COL and MOUSE-ROW move the mouse pointer
to column MOUSE-COL and row MOUSE-ROW.  Do this because
`icicle-candidate-action-fn' can call `select-frame-set-input-focus',
which can position mouse pointer on a standalone minibuffer frame."
  ;; Raise *Completions* frame, if displayed.  This helps keep *Completions* on top.
  (let ((compl-win (get-buffer-window "*Completions*" 'visible)))
    (when compl-win
      (save-window-excursion
        (select-window compl-win)
        ;; Move frame to the right, out of the way.
        (when (and (one-window-p t) icicle-Completions-frame-at-right-flag)
          (modify-frame-parameters
           (selected-frame)             ; Hard-code 7 here - what does it depend on?
           `((left . ,(- (x-display-pixel-width) (+ (frame-pixel-width) 7))))))
        (raise-frame)
        (when (and (integerp mouse-col) (integerp mouse-row))
          (set-mouse-position (selected-frame) mouse-col mouse-row))))))

;;;###autoload
(defun icicle-Completions-mouse-3-menu (event) ; Bound to `C-mouse-3' in *Completions.
  "Pop-up menu on `C-mouse-3' for the current candidate in *Completions*."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        candidate base-size menu-choice)
    ;; (read-event)                 ; Swallow mouse up event. $$ Not needed if bound to up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg       (previous-single-property-change beg 'mouse-face)
                end       (or (next-single-property-change end 'mouse-face)(point-max))
                candidate (buffer-substring-no-properties beg end)))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event)))
          icicle-last-completion-candidate candidate)
    (setq menu-choice
          (x-popup-menu
           event
           (list
            "Completion Candidate"
            (list
             "$$ NOT USED $$"
             '("Help About  (`C-M-RET')" . icicle-help-on-candidate)
             '("Act On  (`C-RET', `C-mouse-2')" . icicle-candidate-action)
             '("Apply a Function To...  (`M-RET', `M-mouse-2')" . icicle-candidate-read-fn-invoke)
             '("Insert in Minibuffer  (`insert')" .
               (lambda ()
                 (interactive)
                 (select-window (active-minibuffer-window))
                 (goto-char (icicle-minibuffer-prompt-end))
                 (icicle-clear-minibuffer)
                 (insert icicle-last-completion-candidate)))
             '("--")
             '("--")
             '("Change Sort Order  (`C-,')" . icicle-change-sort-order)
             '("Change Alternative Sort Order  (`M-,')" . icicle-change-alternative-sort-order)
             '("Swap Alternative Sort  (`C-M-,')" . icicle-toggle-alternative-sorting)
             '("--")
             '("Save All  (`C-M->')" . icicle-candidate-set-save)
             '("             to Variable...  (`C-M-})' " . icicle-candidate-set-save-to-variable)
             '("             to Cache File...  (`C-})" . icicle-candidate-set-save-to-cache-file)
             '("Add Candidates in Current Set  (`C->')" . icicle-candidate-set-save-more)
             '("Save Selected (Region) Candidates  (`C-M-)')" . icicle-candidate-set-save-selected)
             '("Add Selected (Region) Candidates  (`C-)')" . icicle-candidate-set-save-more-selected)
             '("Retrieve Saved  (`C-M-<')" . icicle-candidate-set-retrieve)
             '("--")
             '("Complement All  (`C-~')" . icicle-candidate-set-complement)
             '("Match Also Regexp...  (`M-*')" . icicle-narrow-candidates)
             '("Satisfy Also Predicate...  (`M-&')" . icicle-narrow-candidates-with-predicate)
             '("Save Predicate to Variable...  (`C-M-&')" . icicle-save-predicate-to-variable)
             '("Intersect Saved  (`C-*')" . icicle-candidate-set-intersection)
             '("Subtract Saved  (`C--')" . icicle-candidate-set-difference)
             '("Add (Union) Saved  (`C-+')" . icicle-candidate-set-union)
             '("Only Previously Entered  (`M-pause')" . icicle-keep-only-past-inputs)
             '("--")
             '("Act On All - Careful!  (`C-!')" . icicle-all-candidates-action)
             '("--")
             '("Toggle Duplicate Removal  (`C-$')" . icicle-toggle-transforming)
             '("Toggle Case Sensitivity  (`C-A')" . icicle-toggle-case-sensitivity)
             '("Toggle Highlighting Past Inputs  (`C-pause')" .
               icicle-toggle-highlight-historical-candidates)
             '("Toggle Angle Brackets  (`C-<')" . icicle-toggle-angle-brackets)
             '("Toggle Ignored File Extensions  (`C-.')" . icicle-toggle-ignored-extensions)
             '("Toggle Ignoring Space Prefix  (`C-^')" . icicle-toggle-ignored-space-prefix)
             '("Toggle Incremental Completion  (`C-#')" . icicle-toggle-incremental-completion)
             '("Toggle Escaping Special Regexp Chars  (`C-`')" . icicle-toggle-regexp-quote)
             '("Toggle Removal of Search Highlighting  (`C-.')" . icicle-toggle-search-cleanup)
             '("Toggle All-Current Search Highlighting  (`C-^')" .
               icicle-toggle-highlight-all-current)
             '("Toggle Using `~' For $HOME  (`M-~')" . icicle-toggle-~-for-home-dir)
             '("+ Toggle Any Option..." . icicle-toggle-option)
             '("+ Turn Off Option..." . icicle-reset-option-to-nil)
             '("+ Turn On Option..." . icicle-set-option-to-t)
             '("--")
             '("Restore Previous Completion Input  (`C-l')" . icicle-retrieve-previous-input)
             '("Restore Next Completion Input  (`C-L')" . icicle-retrieve-next-input)
             '("Scroll  (repeated `TAB' or `S-TAB')" . icicle-scroll-Completions)
             '("One-Off Eval...  (`M-:')" . icicle-pp-eval-expression)
             '("Insert `icicle-input-string'  (`C-=')" . icicle-insert-string-from-variable)
             '("--")
             '("Icicles Help  (`C-?')" . icicle-completion-help)))))
    (and menu-choice (call-interactively menu-choice))))

;;;###autoload
(defun icicle-narrow-candidates ()      ; Bound to `M-*' in minibuffer.
  "Narrow the set of completion candidates using another input regexp.
This, in effect, performs a set intersection operation on 1) the set
of candidates in effect before the operation and 2) the set of
candidates that match the current input.  You can repeatedly use this
command to continue intersecting candidate sets, progressively
narrowing the set of matches.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-narrow-candidates]')."
  ;; We handle `no-catch' errors here because `old-completing-read' and
  ;; `old-read-file-file-name' can still be called in Icicle mode by, for instance, an
  ;; `interactive' spec (e.g. (interactive "bBuffer: ")).  In that case, we throw to a
  ;; non-existant catch.  After doing that, we just insert the result, to pass it to the
  ;; next-higher recursive minibuffer.
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((enable-recursive-minibuffers t)
        (icicle-reminder-prompt-flag nil)) ; Inhibit reminder.
    (cond ((null icicle-completion-candidates)
           (error
            (substitute-command-keys
             "No completion candidates.  Did you use `\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]' or `\\[icicle-apropos-complete]'?")))
          ((null (cdr icicle-completion-candidates))
           (if (not icicle-top-level-when-sole-completion-flag)
               (minibuffer-message "  [Sole completion]")
             (condition-case i-narrow-candidates
                 (throw 'icicle-read-top (car icicle-completion-candidates))
               (no-catch (setq icicle-current-input (car icicle-completion-candidates))
                         (icicle-retrieve-last-input)
                         icicle-current-input)
               (error (message (error-message-string i-narrow-candidates))))))
          (t
           (let* ((current-candidates icicle-completion-candidates)
                  (result
                   (if (and (> emacs-major-version 21) (icicle-file-name-input-p))
                       (read-file-name "Match also (regexp): "
                                       (icicle-file-name-directory-w-default icicle-current-input)
                                       nil icicle-require-match-p nil
                                       (lambda (fname) (member fname current-candidates)))
                     (completing-read "Match also (regexp): "
                                      (mapcar #'list icicle-completion-candidates) nil
                                      icicle-require-match-p nil
                                      minibuffer-history-variable))))
             ;; Normally, `icicle-narrow-candidates' is called from the minibuffer.
             ;; If not, just return the result read.
             (if (> (minibuffer-depth) 0)
                 (condition-case i-narrow-candidates
                     (throw 'icicle-read-top result)
                   (no-catch (setq icicle-current-input result)
                             (icicle-retrieve-last-input)
                             icicle-current-input)
                   (error (message (error-message-string i-narrow-candidates))))
               result))))))

;;;###autoload
(defun icicle-apropos-complete-and-narrow () ; Bound to `S-SPC' in minibuffer.
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((icicle-top-level-when-sole-completion-flag t))
    (icicle-apropos-complete)
    (icicle-narrow-candidates)))

;;;###autoload
(defun icicle-narrow-candidates-with-predicate () ; Bound to `M-&' in minibuffer.
  "Narrow the set of completion candidates by applying a predicate.
You can repeatedly use this command to apply additional predicates,
progressively narrowing the set of candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-narrow-candidates-with-predicate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (let ((enable-recursive-minibuffers t)
        (icicle-reminder-prompt-flag nil) ; Inhibit reminder.
        (last-completion-cmd (or icicle-last-completion-command 'icicle-apropos-complete)))
    (cond ((null icicle-completion-candidates)
           (error
            (substitute-command-keys
             "No completion candidates.  Did you use `\\<minibuffer-local-completion-map>\
\\[icicle-prefix-complete]' or `\\[icicle-apropos-complete]'?")))
          ((null (cdr icicle-completion-candidates))
           (if (not icicle-top-level-when-sole-completion-flag)
               (minibuffer-message "  [Sole completion]")
             (condition-case i-narrow-candidates
                 (throw 'icicle-read-top (car icicle-completion-candidates))
               (no-catch (setq icicle-current-input (car icicle-completion-candidates))
                         (icicle-retrieve-last-input)
                         icicle-current-input)
               (error (message (error-message-string i-narrow-candidates))))))
          (t
           ;; Read new predicate to apply.
           (let ((pred (icicle-read-from-minibuf-nil-default "Additional predicate to apply: "
                                                             nil read-expression-map t
                                                             (if (boundp 'function-name-history)
                                                                 'function-name-history
                                                               'icicle-function-name-history))))
             ;; Update predicate to effectively remove this candidate from those possible.
             (cond ((and (icicle-file-name-input-p) ; Emacs 22+ only.
                         (boundp 'read-file-name-predicate)
                         read-file-name-predicate)
                    (setq read-file-name-predicate
                          `(lambda (fname)
                            (and (if ',read-file-name-predicate
                                     (funcall ',read-file-name-predicate fname)
                                   t)
                             (funcall ',pred fname)))))
                   ;; Do nothing for file name if < Emacs 22.
                   ;; `TAB' or `S-TAB' will bring it back as a candidate.
                   ((not (icicle-file-name-input-p))
                    (setq minibuffer-completion-predicate
                          `(lambda (cand)
                            (and (if ',minibuffer-completion-predicate
                                     (funcall ',minibuffer-completion-predicate cand)
                                   t)
                             (funcall ',pred cand)))))))))
    (funcall last-completion-cmd)))

;;;###autoload
(defun icicle-save-predicate-to-variable (askp) ; Bound to `C-M-&' in minibuffer.
  "Save the current completion predicate to a variable.
By default, the variable is `icicle-input-string'.  If you use a
prefix argument, then you are prompted for the variable to use.

You can retrieve the saved predicate as a string using `\\<minibuffer-local-completion-map>\
\\[icicle-insert-string-from-variable]'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-save-predicate-to-variable]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let* ((pred minibuffer-completion-predicate)
         (enable-recursive-minibuffers t)
         (var (if askp
                  (intern (completing-read "Save candidates in variable: " obarray 'boundp
                                           nil nil
                                           (if (boundp 'variable-name-history)
                                               'variable-name-history
                                             'icicle-variable-name-history)))
                'icicle-input-string)))
    (set var (prin1-to-string pred))
    (save-selected-window (select-window (minibuffer-window))
                          (minibuffer-message (format "  [Predicate SAVED to `%s']" var)))))

;;;###autoload
(defun icicle-candidate-set-swap ()     ; Bound to `C-%' in minibuffer.
  "Swap the saved set and current sets of completion candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-swap]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (setq icicle-saved-completion-candidates
        (prog1 icicle-completion-candidates
          (setq icicle-completion-candidates icicle-saved-completion-candidates)))
  (minibuffer-message "  [Saved set of candidates SWAPPED with current]"))

;;;###autoload
(defun icicle-candidate-set-define ()   ; Bound to `C-:' in minibuffer.
  "Define the set of current completion candidates by evaluating a sexp.
The Lisp sexp must evaluate to a list of strings, such as is returned
by `all-completions'.

You can use this command at top level or from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-define]')."
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (evald-sexp (eval-minibuffer "Set the completion candidates to sexp (eval): ")))
    (if (or (null evald-sexp) (and (consp evald-sexp) (stringp (car evald-sexp))))
        (setq icicle-completion-candidates evald-sexp)
      (error "Sexp did not evaluate to a list of strings: %S" evald-sexp)))
  (icicle-maybe-sort-and-strip-candidates)
  (message "List of completion candidates DEFINED: %S" icicle-completion-candidates)
  (when (> (minibuffer-depth) 0)
    (message "Displaying completion candidates...")
    (with-output-to-temp-buffer "*Completions*"
      (display-completion-list icicle-completion-candidates))
    (icicle-narrow-candidates)))

;;;###autoload
(defun icicle-candidate-set-difference () ; Bound to `C--' in minibuffer.
  "Take the set difference between the current and saved candidates.
The new set of candidates is the set of candidates prior to executing
this command minus the saved set of candidates.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-difference]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-1 'icicle-set-difference "  [saved set of candidates SUBTRACTED]"))

;;;###autoload
(defun icicle-candidate-set-union ()    ; Bound to `C-+' in minibuffer.
  "Take the set union between the current and saved candidates.
The new set of candidates is the union of the saved set of candidates
and the set of candidates prior to executing this command.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-union]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-1 'icicle-set-union "  [saved set of candidates ADDED]"))

;;;###autoload
(defun icicle-candidate-set-intersection () ; Bound to `C-*' in minibuffer.
  "Take the set intersection between the current and saved candidates.
The new set of candidates is the intersection of the saved set of
candidates and the set of candidates prior to executing this command.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-intersection]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-1 'icicle-set-intersection
                          "  [INTERSECTION of saved and current sets of candidates]"))

;;;###autoload
(defun icicle-candidate-set-complement () ; Bound to `C-~' in minibuffer.
  "Complement the set of current completion candidates.
The new set of candidates is the set of `all-completions' minus the
set of candidates prior to executing this command - that is, all
possible completions of the appropriate type, except for those that
are in the current set of completions.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-complement]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (setq icicle-completion-candidates
        (icicle-set-difference
         (all-completions "" minibuffer-completion-table minibuffer-completion-predicate
                          icicle-ignore-space-prefix-flag)
         icicle-completion-candidates))
  (icicle-maybe-sort-and-strip-candidates)
  (message "Displaying completion candidates...")
  (with-output-to-temp-buffer "*Completions*" (display-completion-list icicle-completion-candidates))
  (minibuffer-message "  [Set of candidates COMPLEMENTED]")
  (icicle-narrow-candidates))

(defun icicle-candidate-set-truncate (n) ; Bound to `M-$' in minibuffer.
  "Trim the set of current completion candidates at the end.
The first N candidates are kept.  N is read."
  ;; Ugly hack: `icicle-saved-completion-candidates-internal'.  No way to bind a variable
  ;; in `interactive' and have the binding be active in the function body.
  (interactive
   (list (let ((enable-recursive-minibuffers t))
           (setq icicle-saved-completion-candidates-internal icicle-completion-candidates)
           (if current-prefix-arg
               (prefix-numeric-value current-prefix-arg)
             (read-number "Number of candidates to keep: ")))))
  (setq icicle-completion-candidates icicle-saved-completion-candidates-internal)
  (setcdr (nthcdr (1- n) icicle-completion-candidates) nil)
  (icicle-maybe-sort-and-strip-candidates)
  (message "Displaying completion candidates...")
  (with-output-to-temp-buffer "*Completions*" (display-completion-list icicle-completion-candidates))
  (message (format "  [Set of candidates TRUNCATED to %d]" n))
  (icicle-narrow-candidates))
      
(defun icicle-retrieve-candidates-from-set (set-name)
  "Retrieve the saved set of completion candidates named SET-NAME.
The candidates are retrieved to `icicle-saved-completion-candidates'.
Return the name of the cache file for set SET-NAME."
  (let ((file-name (cdr (assoc set-name icicle-saved-completion-sets))))
    (unless file-name (error "Set `%s' not found in `icicle-saved-completion-sets'.  \
Use `icicle-add/update-saved-completion-set'" set-name))
    (unless (icicle-file-readable-p file-name) (error "Cannot read cache file `%s'" file-name))
    (let ((list-buf (find-file-noselect file-name 'nowarn 'raw))
          (candidates nil))
      (message "Retrieving saved candidates from `%s'..." file-name)
      (unwind-protect
           (when (listp (setq candidates (read list-buf)))
             (message "Set `%s' read from file `%s'" set-name file-name))
        (kill-buffer list-buf))
      (unless candidates (error "No completion candidates in file `%s'" file-name))
      (setq icicle-saved-completion-candidates candidates))
    file-name))                         ; Return cache-file name.

;;;###autoload
(defun icicle-candidate-set-retrieve (&optional arg) ; Bound to `C-M-<' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-save-candidate]', `M-S-mouse-2', 
`\\<minibuffer-local-completion-map>\\[icicle-candidate-set-save]', \
`\\[icicle-candidate-set-save-to-variable]', or `\\[icicle-candidate-set-save-to-cache-file]'.
With no prefix arg, retrieve candidates from
 `icicle-saved-completion-candidates'.
With a plain prefix arg `C-u', retrieve candidates from a cache file.
With a numeric prefix arg N, retrieve candidates from a variable.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (let ((name nil)
        (variablep (and arg (atom arg)))
        saved-cands)
    (if arg
        (let ((enable-recursive-minibuffers t))
          (if variablep
              (setq saved-cands
                    (symbol-value (setq name (intern
                                              (completing-read ; Variable name.
                                               "Retrieve candidates from variable: "
                                               icicle-saved-candidates-variables-obarray
                                               nil nil nil (if (boundp 'variable-name-history)
                                                               'variable-name-history
                                                             'icicle-variable-name-history))))))
            (let ((set-name (completing-read "Retrieve completion candidates from set: "
                                             icicle-saved-completion-sets nil nil nil
                                             'icicle-completion-set-history
                                             (caar icicle-saved-completion-sets))))
              (setq name (icicle-retrieve-candidates-from-set set-name))) ; File name.
            (setq saved-cands icicle-saved-completion-candidates)))
      (setq saved-cands icicle-saved-completion-candidates))
    (cond ((null saved-cands)
           (deactivate-mark)
           (icicle-display-candidates-in-Completions)
           (message "No saved candidates to restore") (sit-for 2))
          (t
           (setq icicle-completion-candidates saved-cands)
           (cond ((and (consp icicle-completion-candidates) (null (cdr icicle-completion-candidates)))
                  (icicle-remove-Completions-window)
                  (icicle-insert-completion (car icicle-completion-candidates)) ; Insert sole cand.
                  (minibuffer-message "  [Sole candidate restored]")
                  (save-selected-window (select-window (minibuffer-window))
                                        (icicle-highlight-complete-input)))
                 ((consp icicle-completion-candidates)
                  (deactivate-mark)
                  (icicle-display-candidates-in-Completions)
                  (save-selected-window
                    (select-window (minibuffer-window))
                    (minibuffer-message (if name
                                            (format "  [Saved candidates RESTORED from %s `%s']"
                                                    (if variablep "variable" "cache file") name)
                                          "  [Saved candidates RESTORED]")))
                  (icicle-narrow-candidates)))))))

;;;###autoload
(defun icicle-candidate-set-retrieve-from-variable () ; Bound to `C-M-{' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-save-candidate]', `M-S-mouse-2', or
`\\[icicle-candidate-set-save-to-variable]' (or `\\[icicle-candidate-set-save]' with a numeric \
prefix arg).

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-variable]')."
  (interactive)
  (icicle-candidate-set-retrieve 99))

;;;###autoload
(defun icicle-candidate-set-retrieve-from-cache-file () ; Bound to `C-{' in minibuffer.
  "Retrieve a saved set of completion candidates, making it current.
This retrieves candidates saved with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-cache-file]' or `C-u \\[icicle-candidate-set-save]'.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-cache-file]')."
  (interactive)
  (icicle-candidate-set-retrieve '(1)))

;;;###autoload
(defun icicle-save-candidate ()         ; Bound to `insert' in minibuffer.
  "Add current candidate to value of `icicle-saved-completion-candidates'.
You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-save-candidate]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (if (not (wholenump icicle-candidate-nb))
      (save-selected-window (select-window (minibuffer-window))
                            (minibuffer-message "  [No current candidate]"))
    (let ((cand (elt icicle-completion-candidates icicle-candidate-nb)))
      (unless (member cand icicle-saved-completion-candidates)
        (push cand icicle-saved-completion-candidates)))
    (save-selected-window (select-window (minibuffer-window))
                          (minibuffer-message "  [Candidate SAVED]"))))

;;;###autoload
(defun icicle-mouse-save-candidate (event) ; Bound to `M-S-mouse-2' in *Completions.
  "Add clicked candidate to value of `icicle-saved-completion-candidates'."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Give temp modes such as isearch a chance to turn off.
  (let ((buffer (window-buffer))
        (posn-win (posn-window (event-start event)))
        (posn-col (car (posn-col-row (event-start event))))
        (posn-row (cdr (posn-col-row (event-start event))))
        choice base-size)
    (read-event)                        ; Swallow mouse up event.
    (save-excursion
      (set-buffer (window-buffer posn-win))
      (when completion-reference-buffer (setq buffer completion-reference-buffer))
      (setq base-size completion-base-size)
      (save-excursion
        (goto-char (posn-point (event-start event)))
        (let (beg end)
          (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
            (setq end (point) beg (1+ (point))))
          (when (null beg) (error "No completion here"))
          (setq beg    (previous-single-property-change beg 'mouse-face)
                end    (or (next-single-property-change end 'mouse-face)(point-max))
                choice (icicle-transform-multi-completion
                        (buffer-substring-no-properties beg end))))))
    (setq icicle-candidate-nb (icicle-nb-of-candidate-in-Completions (posn-point (event-start event)))
          icicle-last-completion-candidate choice)
    (unless (member icicle-last-completion-candidate icicle-saved-completion-candidates)
      (push icicle-last-completion-candidate icicle-saved-completion-candidates))
    (save-selected-window (select-window (minibuffer-window))
                          (minibuffer-message "  [Candidate SAVED]"))
    (deactivate-mark)
    (icicle-display-candidates-in-Completions)    
    (icicle-raise-Completions-frame posn-col posn-row)))

;;;###autoload
(defun icicle-mouse-candidate-set-save (ignore &optional arg) ; `M-S-mouse-3' in *Completions*.
  "`icicle-candidate-set-save(-selected)'.
If the region is active in *Completions*, then
`icicle-candidate-set-save-selected'.  Otherwise,
`icicle-candidate-set-save'."
  (interactive "e\nP")
  (if (and (get-buffer "*Completions*")
           (save-current-buffer
             (set-buffer (get-buffer "*Completions*"))
             (and mark-active (mark) (/= (point) (mark)))))
      (icicle-candidate-set-save-selected arg)
    (icicle-candidate-set-save arg)))

;;;###autoload
(defun icicle-mouse-candidate-set-save-more (ignore &optional arg) ; `M-mouse-3' in *Completions*.
  "`icicle-candidate-set-save-more(-selected)'.
If the region is active in *Completions*, then
`icicle-candidate-set-save-more-selected'.  Otherwise,
`icicle-candidate-set-save-more'."
  (interactive "e\nP")
  (if (and (get-buffer "*Completions*")
           (save-current-buffer
             (set-buffer (get-buffer "*Completions*"))
             (and mark-active (mark) (/= (point) (mark)))))
      (icicle-candidate-set-save-more-selected arg)
    (icicle-candidate-set-save-more arg)))

;;;###autoload
(defun icicle-mouse-save-then-kill (click &optional arg)
  "`mouse-save-then-kill', but click same place saves selected candidates."
  (interactive "e\nP")
  (flet ((mouse-save-then-kill-delete-region (beg end)
           (icicle-mouse-candidate-set-save-more nil arg)))
    (mouse-save-then-kill click))
  (setq this-command 'mouse-save-then-kill))

;;;###autoload
(defun icicle-candidate-set-save (&optional arg) ; Bound to `C-M->' in minibuffer.
  "Save the set of current completion candidates, for later recall.
Saves candidates in variable `icicle-saved-completion-candidates', by
default.
With a plain prefix arg (`C-u'), save candidates in a cache file.
With a numeric prefix arg (`C-u N'), save candidates in a variable.

You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-save-1 icicle-completion-candidates arg))

;;;###autoload
(defun icicle-candidate-set-save-more (&optional arg) ; Bound to `C->' in minibuffer.
  "Add current completion candidates to saved candidates set.
Adds candidates to `icicle-saved-completion-candidates', by default.
With a plain prefix arg (`C-u'), add candidates to a cache file.
With a numeric prefix arg (`C-u N'), add candidates to the value of
 a variable other than `icicle-saved-completion-candidates'.

The existing saved candidates are still saved.  The current candidates
are added to those already saved.

You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (icicle-candidate-set-save-1 icicle-completion-candidates arg t))

;;;###autoload
(defun icicle-candidate-set-save-selected (&optional arg) ; Bound to `C-M-)' in minibuffer.
  "`icicle-candidate-set-save', but only for the selected candidates.
Candidates at least partially in the region are saved."
  (interactive "P")
  (icicle-candidate-set-save-selected-1 arg))

;;;###autoload
(defun icicle-candidate-set-save-more-selected (&optional arg) ; Bound to `C-)' in minibuffer.
  "`icicle-candidate-set-save-more', but only for the selected candidates.
Candidates at least partially in the region are added to those saved."
  (interactive "P")
  (icicle-candidate-set-save-selected-1 arg t))

(defun icicle-candidate-set-save-selected-1 (arg &optional morep)
  "Helper function for `icicle-candidate-set-save(-more)-region'."
  (when (get-buffer-window "*Completions*" 0)
    (let ((beg-cand-nb 0)
          (end-cand-nb 0)
          (candidates ())
          (orig-buf (current-buffer)))
      (with-current-buffer "*Completions*"
        (when (and mark-active (mark) (/= (point) (mark)) icicle-completion-candidates)
          (let ((bob (icicle-start-of-candidates-in-Completions))
                (eob (point-max))
                (beg (region-beginning))
                (end (region-end))
                temp)
            ;; Extend region ends to include all of first and last selected candidates.
            (unless (get-text-property beg 'mouse-face)
              (if (setq temp (next-single-property-change beg 'mouse-face))
                  (setq beg temp)
                (setq beg (next-single-property-change temp 'mouse-face))))
            (unless (get-text-property end 'mouse-face)
              (if (setq temp (previous-single-property-change end 'mouse-face))
                  (setq end temp)
                (setq end (previous-single-property-change temp 'mouse-face))))
            (when (> beg end) (error "No candidates selected")) ; Active region but none selected.
            (while (and (>= beg bob) (get-text-property beg 'mouse-face)) (setq beg (1- beg)))
            (while (and (<= end eob) (get-text-property end 'mouse-face)) (setq end (1+ end)))
            (setq beg (1+ beg) end (1- end))
            (setq beg-cand-nb (icicle-nb-of-candidate-in-Completions beg)
                  end-cand-nb (icicle-nb-of-candidate-in-Completions end))
            (while (<= beg-cand-nb end-cand-nb)
              (push (elt icicle-completion-candidates beg-cand-nb) candidates)
              (setq beg-cand-nb (1+ beg-cand-nb))))))
      (setq candidates (nreverse candidates))
      (icicle-candidate-set-save-1 candidates arg morep t)
      (let ((win (get-buffer-window orig-buf 'visible)))
        (when win (select-window win))))))
            
(defun icicle-candidate-set-save-1 (new-cands arg &optional morep only-selected-p)
  "Helper function for `icicle-candidate-set-save*'."
  (let (where)
    (if arg
        (let ((enable-recursive-minibuffers t))
          (if (consp arg)
              (let* ((file-name         ; Write to cache too.
                      (prog1 (let ((icicle-completion-candidates icicle-completion-candidates))
                               (icicle-add/update-saved-completion-set))
                        (with-output-to-temp-buffer "*Completions*" ; Redisplay.
                          (display-completion-list icicle-completion-candidates))
                        (select-window (minibuffer-window))))
                     (list-buf (and morep (find-file-noselect file-name 'nowarn 'raw)))
                     (old-cands ()))
                (when morep
                  (unwind-protect
                       (condition-case nil
                           (setq old-cands (read list-buf))
                         (end-of-file
                          (save-selected-window
                            (select-window (minibuffer-window))
                            (minibuffer-message (format "  [No completion candidates in file `%s']"
                                                        file-name)))))
                    (kill-buffer list-buf)))
                (setq icicle-saved-completion-candidates (append new-cands old-cands)
                      where    (format "cache file `%s'" file-name))
                (with-temp-message (format "Writing candidates to cache file `%s'..." file-name)
                  (with-temp-file file-name
                    (prin1 icicle-saved-completion-candidates (current-buffer)))))
            (let* ((varname
                    (prog1 (let ((icicle-completion-candidates icicle-completion-candidates))
                             (completing-read (if morep
                                                  "Add candidates to variable: "
                                                "Save candidates in variable: ")
                                              icicle-saved-candidates-variables-obarray
                                              nil nil nil (if (boundp 'variable-name-history)
                                                              'variable-name-history
                                                            'icicle-variable-name-history)))
                      (with-output-to-temp-buffer "*Completions*"
                        (display-completion-list icicle-completion-candidates))
                      (select-window (minibuffer-window))))
                   (var (intern varname))) ; Intern in standard `obarray'.
              (intern varname icicle-saved-candidates-variables-obarray) ; For subsequent completion.
              (set var (if (and morep (boundp var) (listp (symbol-value var)))
                           (append new-cands (symbol-value var))
                         new-cands))
              (setq where (format "`%s'" var)))))
      (setq icicle-saved-completion-candidates
            (if (and morep (listp icicle-saved-completion-candidates))
                (append new-cands icicle-saved-completion-candidates)
              new-cands)
            where "`icicle-saved-completion-candidates'"))
    (deactivate-mark)
    (icicle-display-candidates-in-Completions)
    (save-selected-window
      (select-window (minibuffer-window))
      (minibuffer-message
       (if morep
           (if new-cands
               (format "  [%sandidates ADDED to %s]" (if only-selected-p "Selected c" "C") where)
             "  [NO candidates selected to add]")
         (if new-cands
             (format "  [%sandidates SAVED to %s]" (if only-selected-p "Selected c" "C") where)
           "  [SAVED candidates reset to NONE]"))))))

;; This is actually a top-level command, but it is in this file because it is used by
;; `icicle-retrieve-candidates-from-set' and `icicle-candidate-set-save'.
;;
;; We don't define this using `icicle-define-add-to-alist-command', because we want to
;; return the cache-file name.
;;;###autoload
(defun icicle-add/update-saved-completion-set ()
  "Add or update an entry in `icicle-saved-completion-sets'.
That is, create a new saved completion set or update an existing one.
You are prompted for the name of a set of completion candidates and
its cache file.  List `icicle-saved-completion-sets' is updated to
have an entry with these set and file names.
Return the cache-file name."
  (interactive)
  (let* ((set-name (completing-read "Saved completion set: " icicle-saved-completion-sets
                                    nil nil nil 'icicle-completion-set-history
                                    (caar icicle-saved-completion-sets)))
         (file-name ""))
    (while (not (icicle-file-writable-p file-name))
      (setq file-name (expand-file-name
                       (read-file-name "Cache file for the set: " default-directory nil nil
                                       (concat "icicles-"
                                               (icicle-delete-whitespace-from-string set-name)
                                               ".cache")))))
    (setq icicle-saved-completion-sets  ; Remove any old definition of this set.
          (icicle-assoc-delete-all set-name icicle-saved-completion-sets))
    (push (cons set-name file-name) icicle-saved-completion-sets) ; Add new set definition.
    (customize-save-variable 'icicle-saved-completion-sets icicle-saved-completion-sets)
    (message "Added set to `icicle-saved-completion-sets': `%s'" set-name)
    file-name))                         ; Return cache-file name.

;;;###autoload
(defun icicle-candidate-set-save-to-variable () ; Bound to `C-M-}' in minibuffer.
  "Save the set of current completion candidates in a variable you choose.
You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-variable]' (or `\\[icicle-candidate-set-retrieve]'
with a numeric prefix arg).
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-variable]')."
  (interactive)
  (icicle-candidate-set-save 99))

;;;###autoload
(defun icicle-candidate-set-save-to-cache-file () ; Bound to `C-}' in minibuffer.
  "Save the set of current completion candidates persistently in a file.
You can retrieve the saved set of candidates with `\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-retrieve-from-cache-file]' or `C-u \\[icicle-candidate-set-retrieve]'.
You can use the saved set of candidates for operations such as
\\<minibuffer-local-completion-map>
`icicle-candidate-set-union' (`\\[icicle-candidate-set-union]'),
`icicle-candidate-set-intersection' (`\\[icicle-candidate-set-intersection]'), and
`icicle-candidate-set-difference' (`\\[icicle-candidate-set-difference]').

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-candidate-set-save-to-cache-file]')."
  (interactive)
  (icicle-candidate-set-save '(1)))

;;;###autoload
(defun icicle-keep-only-past-inputs (&optional recent-first) ; Bound to`M-pause' in minibuffer.
  "Narrow completion candidates to those that have been used previously.
This filters the set of current completion candidates, keeping only
those that have been used before.  (You must first use `TAB' or
`S-TAB' to establish an explicit candidate set.)

With a prefix arg, the previous inputs are sorted chronologically,
most recent first.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs]')."
  (interactive "P")
  (when (interactive-p) (icicle-barf-if-outside-Completions-and-minibuffer))
  (if (and recent-first (interactive-p) icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (let ((icicle-sort-function (if recent-first 'icicle-most-recent-first-p icicle-sort-function)))
      (when (or recent-first (eq icicle-last-completion-command 'icicle-keep-only-past-inputs))
        (icicle-update-completions 'no-display))
      (if (null icicle-completion-candidates)
          (minibuffer-message "  [No completion candidates to filter]")
        (unless (boundp minibuffer-history-variable) (set minibuffer-history-variable nil))
        (when (consp (symbol-value minibuffer-history-variable))
          (setq icicle-completion-candidates
                (icicle-delete-if-not
                 (lambda (candidate)
                   (when (icicle-file-name-input-p)
                     (setq candidate (expand-file-name candidate
                                                       (file-name-directory icicle-last-input))))
                   (member candidate (symbol-value minibuffer-history-variable)))
                 icicle-completion-candidates))
          (cond ((null icicle-completion-candidates)
                 (save-selected-window (icicle-remove-Completions-window))
                 (minibuffer-message "  [None of the completions have been used before]"))
                (t
                 (cond ((and (symbolp last-command) (get last-command 'icicle-cycling-command))
                        (setq icicle-current-input icicle-last-input)
                        (icicle-retrieve-last-input))
                       (t
                        (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer))))
                 (cond ((null icicle-completion-candidates)
                        (setq icicle-nb-of-other-cycle-candidates 0)
                        (save-selected-window (icicle-remove-Completions-window))
                        (minibuffer-message "  [No matching history element]"))
                       ((null (cdr icicle-completion-candidates)) ; Single cand. Update minibuffer.
                        (setq icicle-nb-of-other-cycle-candidates 0)
                        (icicle-clear-minibuffer)
                        (setq icicle-last-completion-candidate (car icicle-completion-candidates))
                        (let ((inserted (if (and (icicle-file-name-input-p) insert-default-directory)
                                            (icicle-abbreviate-or-expand-file-name
                                             icicle-last-completion-candidate
                                             (icicle-file-name-directory-w-default
                                              icicle-current-input))
                                          icicle-last-completion-candidate)))
                          (insert inserted))
                        (save-selected-window (icicle-remove-Completions-window))
                        (icicle-highlight-complete-input)
                        (minibuffer-message (format "  [One matching history element]")))
                       (t
                        (when (member icicle-current-input icicle-completion-candidates)
                          (icicle-highlight-complete-input))
                        (icicle-display-candidates-in-Completions)
                        (save-window-excursion
                          (select-window (active-minibuffer-window))
                          (minibuffer-message
                           (concat "  [Filtered to (matching) historical candidates"
                                   (and recent-first ", most recent first")
                                   "]")))))
                 (setq icicle-last-completion-command 'icicle-keep-only-past-inputs)))))
      icicle-completion-candidates)))

;;;###autoload
(defun icicle-scroll-Completions ()     ; Actioned by repeated `TAB' or `S-TAB' in minubuffer.
  "Scroll the *Completions* window."
  (interactive)
  (save-selected-window
    (select-window (get-buffer-window "*Completions*" 0))
    (if (not (= (window-end) (point-max)))
        (scroll-up nil)
      (unless (= (window-start) (point-min))
        (goto-char (icicle-start-of-candidates-in-Completions))))))

;;;###autoload
(defun icicle-history ()                ; Bound to `M-h' in minibuffer.
  "Access the appropriate history list using completion or cycling.
The current minibuffer input is interpreted as a regexp and matched
against items in the history list in use for the current command.

Note:

If the required input is a file or directory name, then the entire
minibuffer input is what is matched against the history list.  The
reason for this is that file names in the history list are usually
absolute.  This is unlike the case for normal file-name completion,
which assumes the default directory.

Keep this in mind for apropos (regexp) completion; it means that to
match a file-name using a substring you must, in the minibuffer,
either not specify a directory or explicitly use \".*\" before the
file-name substring.

For example, `/foo/bar/lph' will not apropos-match the previously
input file name `/foo/bar/alphabet-soup.el'; you should use either
`/foo/bar/.*lph' or `lph' (no directory).

This also represents a difference in behavior compared to the similar
command `icicle-keep-only-past-inputs' (\\<minibuffer-local-completion-map>\
\\[icicle-keep-only-past-inputs] in the minibuffer).
That command simply filters the current set of completion candidates,
which in the case of file-name completion is a set of relative file
names.

You can use this command only from the minibuffer (`\\<minibuffer-local-completion-map>\
\\[icicle-history]')."
  (interactive)
  (when (interactive-p) (icicle-barf-if-outside-minibuffer))
  (when (icicle-file-name-input-p) (setq minibuffer-completion-predicate nil))
  (when (arrayp minibuffer-completion-table)
    (setq minibuffer-completion-predicate
          `(lambda (elt) (funcall ',minibuffer-completion-predicate (intern (car elt))))))
  (when (and (boundp minibuffer-history-variable) (consp (symbol-value minibuffer-history-variable)))
    (setq minibuffer-completion-table
          (mapcar #'list (icicle-remove-duplicates (symbol-value minibuffer-history-variable)))))
  (save-selected-window (unless icicle-last-completion-command (icicle-apropos-complete)))
  (cond ((and (symbolp last-command) (get last-command 'icicle-cycling-command))
         (setq icicle-current-input icicle-last-input)
         (icicle-retrieve-last-input))
        (t
         (setq icicle-current-input (icicle-minibuffer-contents-from-minibuffer)
               icicle-last-input    nil ; So `icicle-save-or-restore-input' thinks input has changed.
               last-command         'icicle-history)
         (funcall icicle-last-completion-command))))

;; This is not actually a minibuffer command, since `isearch' technically uses the echo area.
;;;###autoload
(defun icicle-isearch-complete ()       ; Bound to `S-TAB' in `isearch-mode-map'.
  "Complete the search string using candidates from the search ring."
  (interactive)
  (isearch-done 'nopush)
  (let* ((ring (if isearch-regexp regexp-search-ring search-ring))
         (completion (completing-read "Complete search string: "
                                      (mapcar #'list (icicle-remove-duplicates ring))
                                      nil nil isearch-string
                                      (if isearch-regexp 'regexp-search-ring 'search-ring))))
    (setq isearch-string completion)
    (icicle-isearch-resume isearch-string isearch-regexp isearch-word isearch-forward
                           (mapconcat 'isearch-text-char-description isearch-string "")
                           nil)))

(defun icicle-isearch-resume (search regexp word forward message case-fold)
  "Resume an incremental search.
SEARCH is the string or regexp searched for.
REGEXP non-nil means the resumed search was a regexp search.
WORD non-nil means resume a word search.
FORWARD non-nil means resume a forward search.
MESSAGE is the echo-area message recorded for the search resumed.
CASE-FOLD non-nil means the search was case-insensitive."
  (isearch-mode forward regexp nil nil word)
  (setq isearch-string search
	isearch-message message
	isearch-case-fold-search case-fold)
  (isearch-search-and-update))

(defalias 'toggle-icicle-WYSIWYG-Completions 'icicle-toggle-WYSIWYG-Completions)
;;;###autoload
(defun icicle-toggle-WYSIWYG-Completions ()
  "Toggle the value of option `icicle-WYSIWYG-Completions-flag'."
  (interactive)
  (setq icicle-WYSIWYG-Completions-flag (not icicle-WYSIWYG-Completions-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-WYSIWYG-Completions-flag
                                      "Using WYSIWYG for *Completions* display is now ON"
                                    "Using WYSIWYG for *Completions* display is now OFF")))

(defalias 'toggle-icicle-~-for-home-dir 'icicle-toggle-~-for-home-dir)
;;;###autoload
(defun icicle-toggle-~-for-home-dir ()  ; Bound to `M-~' in the minibuffer.
  "Toggle the value of option `icicle-use-~-for-home-dir-flag'.
Bound to `M-~' in the minibuffer."
  (interactive)
  (setq icicle-use-~-for-home-dir-flag (not icicle-use-~-for-home-dir-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-use-~-for-home-dir-flag
                                      "Using `~' for home directory is now ON"
                                    "Using `~' for home directory is now OFF")))

(defalias 'toggle-icicle-alternative-sorting 'icicle-toggle-alternative-sorting)
;;;###autoload
(defun icicle-toggle-alternative-sorting () ; Bound to `C-M-,' in the minibuffer.
  "Toggle alternative sorting of minibuffer completion candidates.
This swaps `icicle-alternative-sort-function' and `icicle-sort-function'.
Bound to `C-M-,' in the minibuffer."
  (interactive)
  (let ((alt-sort-fn icicle-alternative-sort-function))
    (setq icicle-alternative-sort-function (or icicle-sort-function icicle-last-sort-function)
          icicle-sort-function (or alt-sort-fn icicle-last-sort-function))
    (icicle-update-completions)
    (icicle-msg-maybe-in-minibuffer
     (format "Sorting: `%s', Alternative: `%s'"
             icicle-sort-function icicle-alternative-sort-function))))

(defalias 'toggle-icicle-sorting 'icicle-toggle-sorting)
;;;###autoload
(defun icicle-toggle-sorting ()         ; Not bound to a key.
  "Toggle sorting of minibuffer completion candidates.
When sorting is active, comparison is done by `icicle-sort-function'."
  (interactive)
  (if (and (interactive-p) icicle-inhibit-sort-p)
      (icicle-msg-maybe-in-minibuffer "Cannot sort candidates now")
    (if icicle-sort-function
        (setq icicle-last-sort-function icicle-sort-function ; Save it, for restoring.
              icicle-sort-function      nil)
      (setq icicle-sort-function icicle-last-sort-function)) ; Restore it.
    (icicle-update-completions)
    (icicle-msg-maybe-in-minibuffer (if icicle-sort-function
                                        "Completion-candidate sorting is now ON"
                                      "Completion-candidate sorting is now OFF"))))

(defalias 'toggle-icicle-angle-brackets 'icicle-toggle-angle-brackets)
;;;###autoload
(defun icicle-toggle-angle-brackets () ; Bound to `C-<' in the minibuffer.
  "Toggle `icicle-key-descriptions-use-<>-flag'.
Bound to `C-<' in the minibuffer."
  (interactive)
  (setq icicle-key-descriptions-use-<>-flag (not icicle-key-descriptions-use-<>-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-key-descriptions-use-<>-flag
                                      "Displaying <...> in key descriptions is now ON"
                                    "Displaying <...> in key descriptions is now OFF")))

(defalias 'toggle-icicle-transforming 'icicle-toggle-transforming)
;;;###autoload
(defun icicle-toggle-transforming ()    ; Bound to `C-$' in the minibuffer.
  "Toggle transforming of minibuffer completion candidates.
When transforming is active, it is done by `icicle-transform-function'.

By default, transformation, if active, simply removes duplicate
candidates.  Icicles commands already \"do the right thing\" when it
comes to duplicate removal, so you might never need this command.

Bound to `C-$' in the minibuffer."
  (interactive)
  (if icicle-transform-function
      (setq icicle-last-transform-function icicle-transform-function ; Save it, for restoring.
            icicle-transform-function      nil)
    (setq icicle-transform-function icicle-last-transform-function)) ; Restore it.
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-transform-function
                                      "Completion-candidate transformation is now ON"
                                    "Completion-candidate transformation is now OFF")))

(defalias 'toggle-icicle-incremental-completion 'icicle-toggle-incremental-completion)
;;;###autoload
(defun icicle-toggle-incremental-completion () ; Bound to `C-#' in the minibuffer.
  "Toggle the value of option `icicle-incremental-completion-flag'.
If the current value is t or `always', then it is set to nil.
If the current value is nil, then it is set to t.
This command never sets the value to non-nil and non-t.

Bound to `C-#' in the minibuffer."
  (interactive)
  (setq icicle-incremental-completion-flag (not icicle-incremental-completion-flag))
  (setq icicle-incremental-completion-p icicle-incremental-completion-flag)
  (icicle-msg-maybe-in-minibuffer (if icicle-incremental-completion-flag
                                      "Incremental completion is now ON"
                                    "Incremental completion is now OFF")))

;;;###autoload
(defun icicle-dispatch-C-^ ()           ; Bound to `C-^' in the minibuffer.
  "Do the right thing for `C-^'
When Icicles searching, call `icicle-toggle-highlight-all-current'.
Otherwise, call `icicle-toggle-ignored-space-prefix'.
Bound to `C-^' in the minibuffer."
  (interactive)
  (if (eq icicle-candidate-action-fn 'icicle-search-action)
      (icicle-toggle-highlight-all-current)
    (icicle-toggle-ignored-space-prefix)))

(defalias 'toggle-icicle-ignored-space-prefix 'icicle-toggle-ignored-space-prefix)
;;;###autoload
(defun icicle-toggle-ignored-space-prefix () ; Bound to `C-^' in the minibuffer.
  "Toggle `icicle-ignore-space-prefix-flag'.
Bound to `C-^' in the minibuffer, except during Icicles searching.

Note: If the current command binds `icicle-ignore-space-prefix-flag'
locally, then it is the local, not the global, value that is changed.
For example, `icicle-buffer' binds it to the value of
`icicle-buffer-ignore-space-prefix-flag'.  If that is non-nil, then
\\<minibuffer-local-completion-map>`\\[icicle-dispatch-C-^]' toggles \
`icicle-ignore-space-prefix-flag' to nil only for the
duration of `icicle-buffer'."
  (interactive)
  (setq icicle-ignore-space-prefix-flag (not icicle-ignore-space-prefix-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-ignore-space-prefix-flag
                                      "Ignoring space prefix is now ON"
                                    "Ignoring space prefix is now OFF")))

(defalias 'toggle-icicle-highlight-all-current 'icicle-toggle-highlight-all-current)
;;;###autoload
(defun icicle-toggle-highlight-all-current () ; Bound to `C-^' in the minibuffer.
  "Toggle `icicle-search-highlight-all-current-flag'.
Bound to `C-^' in the minibuffer during Icicles searching."
  (interactive)
  (setq icicle-search-highlight-all-current-flag (not icicle-search-highlight-all-current-flag))
  (icicle-erase-minibuffer)
  (icicle-retrieve-last-input)
  (icicle-msg-maybe-in-minibuffer
   (if icicle-search-highlight-all-current-flag
       "Highlighting current input match in each main search hit is now ON"
     "Highlighting current input match in each main search hit is now OFF")))

(defalias 'toggle-icicle-highlight-historical-candidates
    'icicle-toggle-highlight-historical-candidates)
;;;###autoload
(defun icicle-toggle-highlight-historical-candidates () ; Bound to `C-pause' in the minibuffer.
  "Toggle `icicle-highlight-historical-candidates-flag'.
Bound to `C-pause' in the minibuffer."
  (interactive)
  (setq icicle-highlight-historical-candidates-flag
        (not icicle-highlight-historical-candidates-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer
   (if icicle-highlight-historical-candidates-flag
       "Highlighting previously used inputs in *Completions* is now ON"
     "Highlighting previously used inputs in *Completions* is now OFF")))

;;;###autoload
(defun icicle-dispatch-C-. ()           ; Bound to `C-.' in the minibuffer.
  "Do the right thing for `C-.'.
When completing a file name, call `icicle-toggle-ignored-extensions'.
Otherwise, call `icicle-toggle-search-cleanup'.

Bound to `C-.' in the minibuffer."
  (interactive)
  (if (icicle-file-name-input-p) (icicle-toggle-ignored-extensions) (icicle-toggle-search-cleanup)))

(defalias 'toggle-icicle-ignored-extensions 'icicle-toggle-ignored-extensions)
;;;###autoload
(defun icicle-toggle-ignored-extensions () ; Bound to `C-.' in minibuffer during file-name input.
  "Toggle respect of `completion-ignored-extensions'.
Bound to `C-.' in minibuffer during file-name input."
  (interactive)
  (if (consp completion-ignored-extensions)
      (setq icicle-saved-ignored-extensions  completion-ignored-extensions ; Save it.
            completion-ignored-extensions    nil
            icicle-ignored-extensions-regexp nil)
    (setq completion-ignored-extensions icicle-saved-ignored-extensions) ; Restore it.
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'")))
  ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
  ;; `completion-ignored-extensions' changes.
  (setq icicle-ignored-extensions completion-ignored-extensions)
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if completion-ignored-extensions
                                      "Ignoring selected file extensions is now ON"
                                    "Ignoring selected file extensions is now OFF")))

(defalias 'toggle-icicle-search-cleanup 'icicle-toggle-search-cleanup)
;;;###autoload
(defun icicle-toggle-search-cleanup () ; Bound to `C-.' in minibuffer, except for file-name input.
  "Toggle removal of `icicle-search' highlighting after a search.
This toggles option `icicle-search-cleanup-flag'.
Bound to `C-.' in the minibuffer, except for file-name input."
  (interactive)
  (setq icicle-search-cleanup-flag (not icicle-search-cleanup-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-search-cleanup-flag
                                      "Removal of Icicles search highlighting is now ON"
                                    "Removal of Icicles search highlighting is now OFF")))

;;$$$ (defun icicle-dispatch-C-backquote ()   ; Bound to `C-`' in the minibuffer.
;;   "Do the right thing for `C-`'.
;; When searching, call `icicle-toggle-literal-replacement'.
;; Otherwise, call `icicle-toggle-regexp-quote'.

;; Bound to `C-`' in the minibuffer."
;;   (interactive)
;;   (if icicle-searching-p (icicle-toggle-literal-replacement) (icicle-toggle-regexp-quote)))

(defalias 'toggle-icicle-regexp-quote 'icicle-toggle-regexp-quote)
;;;###autoload
(defun icicle-toggle-regexp-quote ()    ; Bound to `C-`' in the minibuffer.
  "Toggle escaping of regexp special chars (`icicle-regexp-quote-flag').

Bound to `C-`' in the minibuffer."
  (interactive)
  (setq icicle-regexp-quote-flag (not icicle-regexp-quote-flag))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if icicle-regexp-quote-flag
                                      "Escaping of regexp special characters is now ON"
                                    "Escaping of regexp special characters is now OFF")))

(defalias 'toggle-icicle-literal-replacement 'icicle-toggle-literal-replacement)
;;;###autoload
(defun icicle-toggle-literal-replacement () ; Bound to `C-M-`' in the minibuffer.
  "Toggle escaping of regexp special chars in replacement text.
This toggles option `icicle-search-replace-literally-flag'.

Bound to `C-M-`' in the minibuffer."
  (interactive)
  (setq icicle-search-replace-literally-flag (not icicle-search-replace-literally-flag))
  (icicle-msg-maybe-in-minibuffer (if icicle-search-replace-literally-flag
                                      "Replacement of text literally is now ON"
                                    "Replacement of text literally is now OFF")))

(defalias 'toggle-icicle-case-sensitivity 'icicle-toggle-case-sensitivity)
;;;###autoload
(defun icicle-toggle-case-sensitivity () ; Bound to `C-S-a' in the minibuffer, that is, `C-A'.
  "Toggle case sensitivity.
This toggles `case-fold-search', `completion-ignore-case', and
`read-file-name-completion-ignore-case'.  More precisely, it toggles
the default value of `case-fold-search', and then it sets the other
variables to the value of `case-fold-search'.

Note:

1. This toggles the default value of `case-fold-search'.  This means
that it does not matter which buffer is current when you call this
command, and all buffers will be affected henceforth.

2. Some commands bind one or more of these variables, so invoking this
command during command execution will not necessarily toggle the
global values of all of the variables.

Bound to `C-S-a' in the minibuffer, that is, `C-A'."
  (interactive)
  (setq-default case-fold-search (not case-fold-search))
  (setq completion-ignore-case case-fold-search)
  (when (boundp 'read-file-name-completion-ignore-case)
    (setq read-file-name-completion-ignore-case case-fold-search))
  (icicle-update-completions)
  (icicle-msg-maybe-in-minibuffer (if case-fold-search
                                      "Case-sensitive comparison is now OFF, everywhere"
                                    "Case-sensitive comparison is now ON, everywhere")))

;;;###autoload
(defun icicle-remove-Completions-window () ; Bound to `C-x 0' in the minibuffer.
  "Remove the *Completions* window.
Bound to `C-x 0' in the minibuffer."
  (interactive) (icicle-delete-windows-on "*Completions*")) ; Defined in `icicles-cmd.el'.

;; This is actually a top-level command, but it is in this file because it is used by
;; `icicle-remove-Completions-window'.
;;;###autoload
(defun icicle-delete-windows-on (buffer)
  "Delete all windows showing BUFFER."
  (interactive
   (list (read-buffer "Remove all windows showing buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer buffer))     ; Convert to buffer.
  (when buffer                          ; Do nothing if null BUFFER.
    ;; Avoid error message "Attempt to delete minibuffer or sole ordinary window".
    (let ((frames (icicle-frames-on buffer t)))
      (unless (and frames (null (cdr frames)) ; One frame shows buffer.
                   (cdr (assoc 'minibuffer (frame-parameters (car frames)))) ; Has a minibuffer.
                   (save-window-excursion
                     (select-frame (car frames))
                     (one-window-p t 'selected-frame))) ; Only one window.
        (dolist (fr frames)
          (delete-window (get-buffer-window buffer 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mcmd)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mcmd.el ends here

0

HTTP/1.1 200 OK
Date: Thu, 06 Sep 2007 07:07:57 GMT
Server: Apache/2.2.3 (Debian) DAV/2 SVN/1.4.2 mod_jk/1.2.18 mod_ssl/2.2.3 OpenSSL/0.9.8c
Etag: 1188102052
Cache-control: max-age=10
Last-Modified: Sun, 26 Aug 2007 04:20:52 GMT
Keep-Alive: timeout=15, max=98
Connection: Keep-Alive
Transfer-Encoding: chunked
Content-Type: text/plain; charset=UTF-8

1000
;;; icicles-mode.el --- Icicle Mode definition for Icicles
;;
;; Filename: icicles-mode.el
;; Description: Icicle Mode definition for Icicles
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2007, Drew Adams, all rights reserved.
;; Created: Mon Feb 27 10:21:10 2006
;; Version: 22.0
;; Last-Updated: Sat Aug 25 20:59:37 2007 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 3465
;; URL: http://www.emacswiki.org/cgi-bin/wiki/icicles-mode.el
;; Keywords: internal, extensions, help, abbrev, local, minibuffer,
;;           keys, apropos, completion, matching, regexp, command
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;;
;; Features that might be required by this library:
;;
;;   `apropos', `apropos-fn+var', `avoid', `cl', `color-theme',
;;   `cus-edit', `cus-face', `cus-load', `cus-start', `custom',
;;   `dired', `dired+', `dired-aux', `dired-x', `easymenu',
;;   `ediff-diff', `ediff-help', `ediff-init', `ediff-merg',
;;   `ediff-mult', `ediff-util', `ediff-wind', `ffap', `ffap-',
;;   `fit-frame', `frame-cmds', `frame-fns', `help-mode', `hexrgb',
;;   `icicles-cmd', `icicles-fn', `icicles-mcmd', `icicles-opt',
;;   `icicles-var', `info', `info+', `kmacro', `misc-fns', `mkhtml',
;;   `mkhtml-htmlize', `pp', `pp+', `strings', `thingatpt',
;;   `thingatpt+', `wid-edit', `widget'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  This is a helper library for library `icicles.el'.  It defines the
;;  command `icicle-mode'.  See `icicles.el' for documentation.
;;
;;  Commands defined here:
;;
;;    `icicle-mode', `icy-mode'.
;;
;;  Non-interactive functions defined here:
;;
;;    `icicle-activate-mark', `icicle-bind-completion-keys',
;;    `icicle-bind-isearch-keys',
;;    `icicle-bind-S-TAB-for-map-variable',
;;    `icicle-bind-S-TAB-in-keymaps-from',
;;    `icicle-cancel-Help-redirection', `icicle-define-icicle-maps',
;;    `icicle-minibuffer-setup', `icicle-rebind-completion-maps',
;;    `icicle-rebind-global', `icicle-rebind-non-completion-keys',
;;    `icicle-restore-non-completion-keys',
;;    `icicle-redefine-standard-commands',
;;    `icicle-redefine-standard-options',
;;    `icicle-redefine-std-completion-fns', `icicle-remap',
;;    `icicle-restore-completion-keys', `icicle-restore-region-face',
;;    `icicle-restore-standard-commands',
;;    `icicle-restore-standard-options',
;;    `icicle-restore-std-completion-fns',
;;    `icicle-run-icicle-post-command-hook',
;;    `icicle-run-icicle-pre-command-hook',
;;    `icicle-select-minibuffer-contents', `icicle-set-calling-cmd',
;;    `icicle-top-level-prep', `icicle-unbind-S-TAB-for-map-variable',
;;    `icicle-unbind-S-TAB-in-keymaps-from',
;;    `icicle-undo-std-completion-faces', `icicle-unmap',
;;    `icicle-update-ignored-extensions-regexp'.
;;
;;  User options defined here (in Custom group `Icicles'):
;;
;;    `icicle-mode', `icicle-mode-hook'.
;;
;;  Internal variables defined here:
;;
;;    `icicle-mode-map'.
 
;;(@> "Index")
;;
;;  If you have library `linkd.el' and Emacs 22 or later, load
;;  `linkd.el' and turn on `linkd-mode' now.  It lets you easily
;;  navigate around the sections of this doc.  Linkd mode will
;;  highlight this Index, as well as the cross-references and section
;;  headings throughout this file.  You can get `linkd.el' here:
;;  http://dto.freeshell.org/notebook/Linkd.html.
;;
;;  (@> "Change log")
;;  (@> "User Options (alphabetical)")
;;  (@> "Internal variables (alphabetical)")
;;  (@> "Icicle mode command")
;;  (@> "Other Icicles functions that define Icicle mode")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;(@* "Change log")
;;
;; 2007/08/25 dadams
;;     icy-mode, icicle-completion-help-string:
;;       icicle-clear-option -> clear-option.  Added toggle alias.
;; 2007/08/21 dadams
;;     icicle-completion-help-string: Mention C-M-l.
;; 2007/08/19 dadams
;;     icicle-minibuffer-setup: Reset icicle-input-fail-pos.
;;     icicle-(bind|restore)-completion-keys:
;;       (Re|u
1000
n)map reposition-window to icicle-kill-failed-input.
;; 2007/08/03 dadams
;;     icicle-mode: Remove icicle* hooks from local, not global, hooks.
;; 2007/07/22 dadams
;;     icicle-(redefine|restore)-standard-commands: Added customize-face(-other-window).
;;     Moved icicle-completing-p to icicles-fn.el.
;;     Require icicles-cmd.el.
;; 2007/07/06 dadams
;;     icicle-rebind-completion-maps:
;;       Moved icicle-Completions-mouse-3-menu to C-mouse-3.
;;       Added icicle(-mouse)-candidate-set-save(-more)-selected, icicle-candidate-set-retrieve,
;;             icicle-retrieve-previous-input.
;;     icicle-completion-help-string: Added icicle-candidate-set-save(-more)-selected.
;;     icicle-bind-completion-maps:
;;       Removed icicle-insert-history-element (inherited).
;;       Added: icicle-candidate-set-save-more(-selected), icicle-mouse-save-then-kill.
;; 2007/07/04 dadams
;;     icicle-rebind-completion-maps, icicle-(bind|restore)-completion-keys:
;;       Added icicle-insert-history-element to Minibuf menu.
;;     icicle-(bind|restore)-completion-keys:
;;       Added icicle-retrieve-(next|previous)-input to Minibuf menu.
;; 2007/07/03 dadams
;;     icicle-rebind-completion-maps, icicle-bind-completion-keys:
;;       Bind icicle-insert-history-element to M-o in all minibuffer maps.
;;     icicle-bind-completion-keys, icicle-completion-help-string:
;;       icicle-retrieve-(next|previous)-input, not icicle-retrieve-last-input.
;;     icicle-(redefine|restore)-std-completion-fns:
;;       defalias next-history-element to icicle-next-history-element.
;;     Removed defadvice for next-history-element.  Redefine in icicles-mcmd.el instead.
;; 2007/06/22 dadams
;;     Bound icicle-search-keywords and added to menus and help strings.
;; 2007/06/20 dadams
;;     Removed M-o binding for icicle-toggle-WYSIWYG-Completions.
;; 2007/06/19 dadams
;;     icicle-bind-completion-keys: Add icicle-save-predicate-to-variable to menus.
;;     icicle-completion-help-string:
;;       Mention icicle-save-predicate-to-variable and icicle-insert-string-from-variable.
;; 2007/06/18 dadams
;;     icy-mode doc string, icicle-completion-help-string: Added icicle-customize-face.
;;     icicle-define-icicle-maps: Added icicle-customize-face to menus.
;; 2007/06/17 dadams
;;     Bound icicle-toggle-WYSIWYG-Completions to M-o.
;;     icicle-minibuffer-setup: Reinitialize icicle-saved-candidate-overlays.
;; 2007/06/16 dadams
;;     icicle-(bind|restore)-completion-keys: Bound C-M-(help|f1).
;; 2007/06/15 dadams
;;     icicle-completion-help-string: Added and cleaned up set stuff.
;;     icicle-(bind|restore)-completion-keys: Cleanup.  Added menu items.
;; 2007/06/14 dadams
;;     Swap bindings for C-insert and insert.
;; 2007/06/13 dadams
;;     Bound C-insert and C-> to icicle-save-candidate and icicle-candidate-set-save-more.
;; 2007/06/12 dadams
;;     icicle-rebind-completion-maps: Bound icicle-mouse-save-candidate to M-S-mouse-2.
;; 2007/06/10 dadams
;;     icicle-mode: comint-mode-hook, compilation(-minor)-mode-hook, temp-buffer-show-hook.
;; 2007/06/08 dadams
;;     icy-mode: Added icicle-find-tag* to doc string.
;;     icicle-define-icicle-maps:
;;       Added icicle-find-tag*.  Remap find-tag* to icicle-find-tag*.
;;       Corrected Info menu.
;;     icicle-completion-help-string: Added icicle-find-tag*.
;; 2007/05/28 dadams
;;     icicle-restore-non-completion-keys: Unbind S-tab.
;;     Added: icicle-unbind-S-TAB-for-map-variable, icicle-unbind-S-TAB-in-keymaps-from.
;;     icicle-bind-S-TAB-in-keymaps-from: Treat S-tab and S-iso-lefftab separately.
;;     icicle-define-icicle-maps: Added icicle-imenu-* to Icicles/Search menus.
;; 2007/05/22 dadams
;;     Make [Icy] menu items invisible when not in Icicle mode.  Add :keys where appropriate.
;;     icicle-define-icicle-maps, icicle-rebind-completion-maps,
;;     icicle-(bind|restore)-completion-keys:
;;       icicle-menu-item-any-version -> menu-item.  Explicit put of enable property -> :enable.
;;     Don't require icicles-mac.el.
;;     icicle-bind-completion-keys: Added icicle-narrow-
1000
candidates, and corrected :enable forms.
;; 2007/05/21 dadams
;;     icicle-define-icicle-maps: Remap minibuffer-keyboard-quit to icicle-abort-minibuffer-input.
;;       Needed, even though local-must-match inherits from local-completion in Emacs 22, because
;;       delsel.el binds C-g to minibuffer-keyboard-quit in minibuffer maps.
;;     menu-item-any-version -> icicle-menu-item-any-version.
;;     Added Icicles/Search menu items:
;;       Search (Buffer|File|Saved Region|All Saved Regions|Definition|Text Property).
;;     Renamed: Search a Region -> Search Saved Region, Choose a Region -> Choose Saved Region,
;;              Add Current Region to List -> Save Current Region.
;; 2007/05/20 dadams
;;     Enable menu-bar Minibuf: 
;;       icicle-rebind-completion-maps:
;;         Use menu-item-any-version.
;;         Don't define menu for maps if it is defined by parent map.
;;         Add Enter and Help items for *-local-map.
;;         Add Enter, Help and Quit items for *-local-(ns|isearch)-map.
;;       icicle-bind-completion-keys:
;;         Use menu-item-any-version.
;;         Add Enter item for *-local-completion-map, unless defined by parent map.
;;       icicle-restore-completion-keys:
;;         Use menu-item-any-version.
;;         Add Enter and Quit items for *-local-completion-map, unless defined by parent map.
;;         Do not unmap kill-region(-wimpy).
;;         Bind [(control pause)] to nil.
;; 2007/05/13 dadams
;;     icicle-restore-completion-keys: Restore some forgotten minibuf menu items.
;; 2007/05/08 dadams
;;     Bound icicle-save-predicate-to-variable to C-M-&.
;; 2007/05/06 dadams
;;     icicle-rebind-completion-maps: Updated icicle-completion-help-string.
;;     Added defvars to quiet byte compiler.
;;     Changed S-C- to C-S- and M-C- to C-M- in doc.
;; 2007/05/03 dadams
;;     Remap icicle-yank-function, not yank.
;;     icicle-define-icicle-maps: Bind icicle-search-word.
;;     icicle-mode, icicle-completion-help-string: Add icicle-search-word to doc.
;; 2007/05/02 dadams
;;     Bound M-q to icicle-dispatch-M-q, not to icicle-insert-key-description.
;;     Bound C-` to icicle-toggle-regexp-quote, not to icicle-dispatch-C-backquote.
;;     Bound C-M-` to icicle-toggle-literal-replacement.
;;     Update icicle-completion-help-string.
;; 2007/04/20 dadams
;;     icicle-minibuffer-setup: Don't reset icicle-search-context-level here.
;; 2007/04/17 dadams
;;     Bound M-, to icicle-dispatch-M-comma, not to icicle-change-alternative-sort-order.
;;     Bound C-` to icicle-dispatch-C-backquote, not to icicle-toggle-regexp-quote.
;; 2007/04/10 dadams
;;     icicle-minibuffer-setup: Initialize icicle-search-context-level.
;; 2007/04/09 dadams
;;     Bound icicle-imenu to C-c =.
;; 2007/04/08 dadams
;;     Bound icicle-all-candidates-alt-action to C-S-insert.
;; 2007/04/07 dadams
;;     icicle-completion-help-string: Updated.
;;     Bound icicle-dispatch-C-comma to C-,.
;;     Bound in menu: icicle-toggle-search-replace-whole.
;;     Bound icicle-(next|previous)-(apropos|prefix)-candidate-alt-action (forgot).
;; 2007/04/02 dadams
;;     Bound icicle-search-text-property to C-c ".  Added it to icicle-completion-help-string.
;; 2007/03/23 dadams
;;     Bound icicle-apropos-complete-and-narrow to S-SPC.  Mention in *-completion-help-string.
;; 2007/03/14 dadams
;;     Added: icicle-top-level-prep.
;;     Removed: icicle-reset-candidates-alist.
;;     Do top-level stuff in icicle-minibuffer-setup, not in icicle-mode.
;;     icicle-minibuffer-setup: Add icicle-top-level-prep to pre-command-hook.
;;     icicle-mode: Remove icicle-top-level-prep from pre-command-hook.
;; 2007/03/07 dadams
;;     icicle-cancel-Help-redirection: Use 0, not t, as frame arg to get-buffer-window.
;; 2007/03/02 dadams
;;     icicle-bind-S-TAB-in-keymaps-from: Bound S-iso-left-tab also.
;; 2007/03/02 dadams
;;     icicle-define-icicle-maps:
;;       Bound S-iso-left-tab also to icicle-generic-S-tab.  Thx to Shreevatsa R.
;; 2007/02/28 dadams
;;     Added: icicle-reset-candidates-alist.
;;     icicle-mode: Use icicle-reset-candidat
1000
es-alist.
;; 2007/02/27 dadams
;;     icicle-minibuffer-setup: Wait icicle-incremental-completion-delay before initial display.
;; 2007/02/24 dadams
;;     Bound: delete, S-mouse-2 to icicle(-mouse)-remove-candidate,
;;            C-S-RET, C-S-mouse-2 to icicle(-mouse)-candidate-alt-action,
;;            S-delete to icicle-delete-candidate-object.
;;     Don't remap icicle-kill-region(-wimpy) to delete key.
;; 2007/02/17 dadams
;;     Added: icicle-bind-S-TAB-in-keymaps-from, icicle-bind-S-TAB-for-map-variable.
;;     icicle-rebind-non-completion-keys:
;;       Bind S-TAB to keymaps in icicle-keymaps-for-key-completion.
;; 2007/02/02 dadams
;;     icicle-completing-p: Cache the value in variable icicle-completing-p.
;;     icicle-minibuffer-setup: Reset icicle-completing-p to nil.
;;     icicle-activate-mark: Use var, not function, icicle-completing-p, but after minibuf test.
;; 2007/01/23 dadams
;;     icicle-(redefine|restore)-std-completion-fns:
;;       Added icicle-read-face-name, icicle-face-valid-attribute-values.
;;     icicle-define-icicle-maps, icicle-rebind-completion-maps:
;;       Updated wrt toggles.  Added icicle*-highlight-historical-candidates*.
;;     icicle-bind-completion-keys: Added icicle-toggle-highlight-historical-candidates.
;; 2007/01/22 dadams
;;     Renamed icicle-regions to icicle-region-alist (forgot occurrences here).
;; 2007/01/20 dadams
;;     icicle-(redefine|restore)-std-completion-fns: Added icicle-display-completion-list.
;; 2007/01/15 dadams
;;     Moved C-, binding from icicle-toggle-sorting to icicle-change-sort-order.
;;     Moved icicle-toggle-alternative-sorting from M-, to C-M-,.
;;     Bound icicle-change-alternative-sort-order to M-,.
;;     Updated list of options in icicle-completion-help-string.
;; 2007/01/12 dadams
;;     Removed: icicle-override-maps-w-minibuffer-map, icicle-restore-overriding-local-map.
;;              Not used in minibuffer hooks.
;;     Removed [pause] bindings from minibuffer maps.
;;     Removed remap of yank in minibuffer maps.
;;     No longer bind icicle-remove-Completions-window in minibuffer maps.
;; 2007/01/11 dadams
;;     Renamed: icicle-define-icicle-mode-map to icicle-define-icicle-maps.
;;     icicle-define-icicle-maps: Use icicle-menu-map.  Don't recreate it.
;;     Bound [pause] to icicle-switch-to/from-minibuffer in all minibuffer maps.
;; 2007/01/10 dadams
;;     Added: icicle-override-maps-w-minibuffer-map, icicle-restore-overriding-local-map,
;;            icicle-(rebind|restore)-non-completion-keys. 
;;     Added: icicle-rebind-global: This used to be called icicle-remap.
;;     icicle-(remap|unmap): Different purpose and use now.  Redefined to use remapping when
;;        available (as was done before for self-insert-command).
;;     icicle-mode:
;;       Add, remove as minibuffer setup and exit hooks: icicle-override-maps-w-minibuffer-map,
;;                                                       icicle-restore-overriding-local-map.
;;       Call icicle-(rebind|restore)-non-completion-keys.
;;     icicle-define-icicle-mode-map:
;;       Use icicle-remap where previously used substitute-key-definition for top-level commands.
;;       Moved to icicle-(rebind|restore)-non-completion-keys:
;;         binding of Info commands in Info map and S-tab in all keymaps (to *-rebind-* only).
;;     icicle-(bind|restore)-completion-keys: Use new icicle-(remap|unmap) where possible.
;;       Use icicle-rebind-global and substitute-key-definition for keys defined in vanilla 
;;         completion maps.
;; 2007/01/06 dadams
;;     icicle-mode: Update doc and bind icicle-toggle-~-for-home-dir to M-~.
;; 2007/01/01 dadams
;;     Moved assq-delete-all to icicles-fn.el.
;;     Require at runtime, not compile-time: icicles-var.el, icicles-fn.el.
;; 2006-12-31 dadams
;;     icicle-define-icicle-mode-map: Delete icicle-mode entry from minor-mode-map-alist.
;;     icicle-mode: Unbind icicle-mode-map when the mode is turned off.
;;     Added assq-delete-all for Emacs 20.
;;     Use current-global-map function, not global-map variable.
;; 2006/12/25 dadams

1000
;;     Bound icicle-candidate-set-truncate to M-$.
;; 2006/12/24 dadams
;;     icicle-bind-completion-keys: transpose-yank(-pop) -> yank(-pop): typo.
;;     Bound mouse-3 to icicle-Completions-mouse-3-menu in completion-list-mode-map.
;; 2006/12/22 dadams
;;     Bound icicle-exchange-point-and-mark.
;;     :group 'icicles -> :group 'Icicles-Miscellaneous.
;; 2006/12/17 dadams
;;     Bound icicle(-mouse)-candidate-read-fn-invoke.
;; 2006/12/16 dadams
;;     icicle-define-icicle-mode-map: Protect icicle-kmacro with fboundp.
;; 2006/12/12 dadams
;;     Added icicle-customize-icicles-group, icicle-kill-buffer, icicle-delete-windows to I. menu.
;;     Added + to multi-command menu items.
;; 2006/12/11 dadams
;;     Added icicle-customize-apropos* and icicle-Info-* to menu-bar menus.
;; 2006/12/10 dadams
;;     Updated user options list in icicle-completion-help-string.
;;     Updated list of icicle-opt stuff used here.
;; 2006/12/06
;;     icicle-select-minibuffer-contents:
;;       Use icicle-minibuffer-prompt-end, not point-min.  Thx to Erik Postma.
;; 2006/11/26 dadams
;;     Added icicle-regions stuff.
;; 2006/11/24 dadams
;;     icicle-redefine-standard-options: Treat icicle-kmacro-ring-max.
;;     Bind icicle-kmacro to f5
;;     Replaced icicle-select-window-or-frame by icicle-other-window-or-frame.
;;     Removed binding of icicle-select-frame.
;;     Do not require mb-depth+.el for Emacs 21 (do it only for Emacs 22).
;; 2006/11/23 dadams
;;     Bound icicle-execute-named-keyboard-macro to C-x M-e.
;; 2006/11/18 dadams
;;     Soft require mb-depth+.el instead of minibuf-depth.el.
;; 2006/11/17 dadams
;;     Bind icicle-select-window-or-frame to whatever other-window(-or-frame) is bound to.
;;     Bind icicle-select-frame to whatever other-frame is bound to.
;; 2006/11/09 dadams
;;     Bind icicle-dispatch-C-^, not icicle-toggle-ignored-space-prefix, to C-^.
;;     icicle-rebind-completion-maps: Updated doc string for icicle-dispatch-C-^.
;; 2006/11/05 dadams
;;     Bound icicle-occur to C-c '.  Added it to menu-bar menus.
;; 2006/10/18 dadams
;;     icy-mode: Invoke icicle-define-icicle-mode-map unconditionally, not just first time.
;; 2006/10/16 dadams
;;     icicle-define-icicle-mode-map: Try to avoid binding S-TAB to menu maps.
;; 2006/10/15 dadams
;;     icicle-define-icicle-mode-map: Simplified and corrected binding of S-TAB for key completion.
;;                                    Use a separate map for the menu bar.
;;     Moved here from icicles-fn.el:
;;       icicle-bind-isearch-keys, icicle-rebind-completion-maps,
;;       icicle-(redefine|restore)-standard-(commands|options),
;;       icicle-(redefine|restore)-std-completion-fns, icicle-(re|un)map,
;;       icicle-(bind|restore)-completion-keys, icicle-minibuffer-setup,
;;       icicle-cancel-*Help*-redirection, icicle-activate-mark,
;;       icicle-run-icicle-(pre|post)-command-hook, icicle-set-calling-cmd,
;;       icicle-undo-std-completion-faces icicle-update-ignored-extensions-regexp,
;;       icicle-completing-p, icicle-restore-region-face.
;;     Renamed: icicle-cancel-*Help*-redirection to icicle-cancel-Help-redirection.
;;     Moved here from icicles-cmd.el: icicle-select-minibuffer-contents, next-history-element.
;;     Moved to icicles-cmd.el: icicle-generic-S-tab.
;;     Require icicles-opt.el.
;;     Added eval-when-compile's and defvars to quite byte compiler.
;; 2006/09/23 dadams
;;     icicle-define-icicle-mode-map: Corrected binding of icicle-yank-insert.
;; 2006/09/22 dadams
;;     icicle-minibuffer-setup: Set this-command and last-command, for scrolling *Completions*.
;; 2006/09/18 dadams
;;     icicle-mode: Picked up all global prefixes for S-TAB.
;; 2006/09/17 dadams
;;     Added: icicle-generic-S-tab.  Bound to S-TAB.
;;     icicle-mode:
;;       Bound icicle-complete-keys to prefix keys followed by S-TAB.
;;       Added run-hooks for Emacs 22 version.
;; 2006/09/12 dadams
;;     Bound icicle-switch-to/from-minibuffer to [pause].
;; 2006/08/27 dadams
;;     Bound icicle-abort-minibuffer-input to what abort-recursive-edit is normally bo
1000
und to.
;;       And add it to Icicle menu.
;; 2006/08/23 dadams
;;     Bound icicle-delete-window to what delete-window and delete-windows-for are normally
;;       bound to.
;;     Put use of Info-mode-map inside an eval-after-load.
;; 2006/08/18 dadams
;;     Added icicle-Info-goto-node-cmd to icicle-mode doc string.
;;       Substitute it for Info-goto-node binding.
;; 2006/08/13 dadams
;;     Added icicle-Info-index-cmd to icicle-mode doc string.
;;       Substitute it for Info-index binding.
;; 2006/08/04 dadams
;;     Added icicle-plist to menus.
;;     icicle-doc treats faces too now.
;; 2006/08/03 dadams
;;     Bound icicle-insert-yank to what yank is normally bound to.
;;     icicle-mode: Updated doc string.
;; 2006/07/29 dadams
;;     icy-mode, icicle-define-icicle-mode-map: Added missing toggle commands.
;; 2006/07/22 dadams
;;     Changed binding of C-c C-s for icicle-search to C-c ` for icicle-search-generic.
;;     Removed: add-hooks for icicle-compilation-search - see icicles-cmd.el.
;; 2006/06/08 dadams
;;     Converted global bindings in icicles-keys.el to icicle-mode-map bindings here.
;;     Added f10 binding for icicle-execute-menu-command.
;; 2006/05/19 dadams
;;     icicle-mode: (add-hook 'kill-emacs-hook 'icicle-control-reminder-prompt).
;; 2006/05/18 dadams
;;     Change :init-value to nil, per new Emacs convention.
;; 2006/05/13 dadams
;;     icicle-mode: Updated doc string.
;; 2006/05/10 dadams
;;     icicle-define-icicle-mode-map: Added menu item Send Bug Report.
;; 2006/04/03 dadams
;;     icicle-define-icicle-mode-map: Added icicle-toggle-(regexp-quote|incremental-completion).
;; 2006/03/16 dadams
;;     icicle-mode: Turn on minibuffer-indicate-depth-mode (Emacs 22 only).
;;     Added soft require of minibuf-depth.el for Emacs 22.
;; 2006/03/14 dadams
;;     Do not use icicle-reset-icicle-completing-p as minibuffer-exit-hook.
;; 2006/03/07 dadams
;;     Corrected menu items for icicle-doc (no name regexp input, just doc regexp).
;; 2006/03/05 dadams
;;     Moved here from icicle-opt.el: icicle-mode, icicle-mode-hook.
;;     Moved here from icicle-fn.el: icicle-mode-map.
;;     Added: icicle-define-icicle-mode-map.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'icicles-opt)
  ;; icicle-bind-top-level-commands-flag, icicle-buffer-configs, icicle-buffer-extras,
  ;; icicle-change-region-background-flag, icicle-cycling-respects-completion-mode-flag,
  ;; icicle-incremental-completion-flag, icicle-init-value-flag,
  ;; icicle-kmacro-ring-max, icicle-minibuffer-setup-hook, icicle-modal-cycle-down-key,
  ;; icicle-modal-cycle-up-key, icicle-redefine-standard-commands-flag,
  ;; icicle-regexp-search-ring-max, icicle-region-background, icicle-search-ring-max,
  ;; icicle-show-Completions-initially-flag, icicle-touche-pas-aux-menus-flag,
  ;; icicle-word-completion-key, icicle-yank-function
(require 'icicles-fn) ;; assq-delete-all, icicle-completing-p
(require 'icicles-var)
  ;; icicle-candidate-action-fn, icicle-candidate-nb, icicle-cmd-calling-for-completion,
  ;; icicle-completing-p, icicle-completion-candidates, icicle-completion-help-string,
  ;; icicle-current-completion-mode, icicle-default-directory, icicle-ignored-extensions,
  ;; icicle-ignored-ext
1000
ensions-regexp, icicle-incremental-completion-p, icicle-initial-value,
  ;; icicle-last-completion-candidate, icicle-last-completion-command, icicle-last-input,
  ;; icicle-menu-map, icicle-pre-minibuffer-buffer, icicle-minor-mode-map-entry, icicle-prompt,
  ;; icicle-prompt-suffix, icicle-saved-completion-candidates, icicle-saved-kmacro-ring-max,
  ;; icicle-saved-regexp-search-ring-max, icicle-saved-region-background,
  ;; icicle-saved-search-ring-max, icicle-search-current-overlay, icicle-search-overlays,
  ;; icicle-search-refined-overlays

(require 'icicles-cmd)
  ;; icicle-add-buffer-candidate, icicle-add-buffer-config, icicle-customize-face,
  ;; icicle-customize-face-other-window, icicle-dabbrev-completion, icicle-imenu, icicle-occur,
  ;; icicle-search, icicle-search-buffer, icicle-search-region, icicle-search-all-regions,
  ;; icicle-search-file

(when (>= emacs-major-version 22) (require 'mb-depth+ nil t)) ; Emacs 22

(eval-when-compile
 (when (< emacs-major-version 21) (require 'cl))) ;; push, dolist
                                                  ;; plus, for Emacs < 20: when, unless
(eval-when-compile (require 'menu-bar+ nil t)) ;; (no error if not found): menu-bar-frames-menu

;; `icicle-apropos-complete' is used here.  It is defined in `icicles-cmd.el'.
;; `icicle-file-name-input-p' is used here.  It is defined in `icicles-fn.el'.

;;; Defvars to quiet byte-compiler:
(when (< emacs-major-version 22)
  (defvar kmacro-ring-max)
  (defvar minibuffer-local-filename-completion-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;(@* "User Options (alphabetical)")

;;; User Options (alphabetical) --------------------------------------

;; Emacs 20 only
(unless (fboundp 'define-minor-mode)
  (defcustom icicle-mode nil
    "*Toggle minibuffer input completion and cycling.
Setting this variable directly does not take effect;
use either \\[customize] or command `icy-mode' (aka `icicle-mode')."
    :set (lambda (symbol value) (icicle-mode (if value 1 -1)))
    :initialize 'custom-initialize-default
    :type 'boolean :group 'Icicles-Miscellaneous :require 'icicles))

;;;###autoload
(defcustom icicle-mode-hook nil
  "*Functions run after entering and exiting Icicle mode."
  :type 'hook :group 'Icicles-Miscellaneous)
 
;;(@* "Internal variables (alphabetical)")

;;; Internal variables (alphabetical) --------------------------------

(defvar icicle-mode-map nil
  "Keymap for Icicle mode.  These are top-level key bindings.
See also `icicle-rebind-completion-maps' for minibuffer bindings.")
 
;;(@* "Icicle mode command")

;;; Icicle mode command ----------------------------------------------

;; Main command.  Inspired from `icomplete-mode'.
(defalias 'icy-mode 'icicle-mode)
;;;###autoload
(if (fboundp 'define-minor-mode)
    ;; Emacs 21+ ------------
    (eval '(define-minor-mode icicle-mode
            "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

The following top-level commands are also available in Icicle mode:

`clear-option' (alias)                 - Set binary option(s) to nil
`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-region'                    - Add to `icicle-region-alist'
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-zippy'                 - Show matching Zippy quotes
`icicle-bookmark'                      - Jump to bookmark(s)
`icicle-buffer'(`-other-window')       - Switch to buffer(s)
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-color-theme'                   - Change color
1000
 theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completion-help'               - Give bindings for completion
`icicle-customize-face'                - Multi-`customize-face'
`icicle-customize-icicles-group'       - Customize Icicles
`icicle-delete-file'                   - Delete file(s)/directory(s)
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows-on'             - Delete all windows for buffer
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-execute-extended-command'      - `execute-extended-command' +
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-find-file'(`-other-window')    - Visit file(s)/directory(s)
`icicle-find-tag'(`-other-window')     - Visit definition(s) with tag
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description(s)
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-imenu'                         - Navigate among Imenu entries
`icicle-Info-goto-mode'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-insert-kill'                   - Like `yank', without rotating
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-kill-buffer'                   - Kill buffer
`icicle-kmacro'                        - Execute a keyboard macro
`icicle-locate-file'(`-other-window')  - Visit file(s) in a directory
`icicle-map'                           - Apply function to alist items
`icy-mode' or `icicle-mode'            - Toggle Icicle mode
`icicle-occur'                         - `occur' + apropos icompletion
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-recent-file'(`-other-window')  - Open recently used file(s)
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-region'             - Remove from `icicle-region-alist'
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-keywords'               - Search with regexp keywords
`icicle-search-region'                 - Search multiple regions
`icicle-search-word'                   - Whole-word search
`icicle-select-frame'                  - Select and raise a frame
`icicle-select-region'                 - Select from multiple regions
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-ignored-extensions'     - Toggle ignoring file suffixes
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option(s)
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'      
11000
     - Toggle duplicate removal
`icicle-toggle-WYSIWYG-Completions'    - Toggle WYSIWYG *Completions*
`icicle-vardoc'                        - Show variable description(s)
`icicle-yank-insert'                   - `yank' using completion
`toggle' (alias)                       - Toggle binary user option(s)

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' \
when the minibuffer is active."
            :global t :group 'Icicles-Miscellaneous :lighter " Icy" :init-value nil
            (cond (icicle-mode
                   (icicle-define-icicle-maps)
                   (icicle-rebind-non-completion-keys)
                   (add-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                   (add-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
                   (add-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                   (add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)
                   ;; The pre- and post-command hooks are local to the minibuffer,
                   ;; So they are added in `icicle-minibuffer-setup', not here.
                   ;; Nevertheless, they are removed here when Icicle mode is exited.
                   (add-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
                   (add-hook 'completion-setup-hook    'icicle-set-calling-cmd 'append)
                   (add-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
                   (add-hook 'comint-mode-hook         'icicle-comint-hook-fn)
                   (add-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
                   (add-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
                   (add-hook 'temp-buffer-show-hook    'icicle-fit-Completions-window)
                   (icicle-undo-std-completion-faces)
                   (icicle-redefine-std-completion-fns)
                   (icicle-redefine-standard-commands)
                   (icicle-redefine-standard-options)
                   (when (fboundp 'minibuffer-indicate-depth-mode)
                     (minibuffer-indicate-depth-mode 99)))
                  (t
                   (makunbound 'icicle-mode-map)
                   (icicle-restore-non-completion-keys)
                   (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
                   (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
                   (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
                   (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
                   (remove-hook 'pre-command-hook         'icicle-top-level-prep)
                   (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook t)
                   (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook t)
                   (remove-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
                   (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
                   (remove-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
                   (remove-hook 'comint-mode-hook         'icicle-comint-hook-fn)
                   (remove-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
                   (remove-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
                   (remove-hook 'temp-buffer-show-hook    'icicle-fit-Completions-window)
                   ;; $$ Should restore standard completion faces here.
                   (icicle-restore-std-completion-fns)
                   (icicle-restore-standard-commands)
                   (icicle-restore-standard-options)
                   (when (fboundp 'minibuffer-indicate-depth-mode)
                     (minibuffer-indicate-depth-mode -99))))
            (message "Turning %s Icicle mode..." (if icicle-mode "ON" "OFF"))
            (icicle-rebind-completion-maps icicle-mode)
            (run-hooks 'icicle-mode-hook)
            (message "Turning %s Icicle mode...done" (if icicle-mode "ON" "OFF"))))

  ;; Emacs 20 ------------
  (defun icicle-mode (&optional arg)
    "Icicle mode: Toggle minibuffer input completion and cycling.
Non-nil prefix ARG turns mode on if ARG > 0, else turns it off.
Icicle mode is a global minor mode.  It binds keys in the minibuffer.

The following top-level commands are also available in Icicle mode:

`clear-option' (alias)                 - Set binary option(s) to nil
`icicle-add-buffer-candidate'          - Add always-candidate buffer
`icicle-add-buffer-config'             - To `icicle-buffer-configs'
`icicle-add-region'                    - Add to `icicle-region-alist'
`icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
`icicle-apropos'                       - `apropos', but shows matches
`icicle-apropos-command'               - Enhanced `apropos-command'
`icicle-apropos-variable'              - Enhanced `apropos-variable'
`icicle-apropos-zippy'                 - Show matching Zippy quotes
`icicle-bookmark'                      - Jump to bookmark(s)
`icicle-buffer'(`-other-window')       - Switch to buffer(s)
`icicle-buffer-config'                 - Pick `icicle-buffer' options
`icicle-buffer-list'                   - Choose a list of buffer names
`icicle-color-theme'                   - Change color theme
`icicle-comint-command'                - Reuse shell etc. command
`icicle-comint-search'                 - Reuse shell etc. command
`icicle-compilation-search'            - `icicle-search' and show hits
`icicle-complete-thesaurus-entry'      - Complete word using thesaurus
`icicle-completion-help'               - Give bindings for completion
`icicle-customize-face'                - Multi-`customize-face'
`icicle-customize-icicles-group'       - Customize Icicles
`icicle-delete-file'                   - Delete file(s)/directory(s)
`icicle-delete-window'                 - Delete window (`C-u': buffer)
`icicle-delete-windows-on'             - Delete all windows for buffer
`icicle-doc'                           - Show doc for fn, var, or face
`icicle-execute-extended-command'      - `execute-extended-command' +
`icicle-execute-named-keyboard-macro'  - Execute named keyboard macro
`icicle-find-file'(`-other-window')    - Visit file(s)/directory(s)
`icicle-find-tag'(`-other-window')     - Visit definition(s) with tag
`icicle-font'                          - Change font of frame
`icicle-frame-bg'                      - Change background of frame
`icicle-frame-fg'                      - Change foreground of frame
`icicle-fundoc'                        - Show function description(s)
`icicle-goto-global-marker'            - Go to a global marker
`icicle-goto-marker'                   - Go to a marker in this buffer
`icicle-imenu'                         - Navigate among Imenu entries
`icicle-Info-goto-mode'                - Multi-cmd `Info-goto-node'
`icicle-Info-index'                    - Multi-command `Info-index'
`icicle-insert-kill'                   - Like `yank', without rotating
`icicle-insert-thesaurus-entry'        - Insert thesaurus entry(s)
`icicle-kill-buffer'                   - Kill buffer
`icicle-kmacro'                        - Execute a keyboard macro
`icicle-locate-file'(`-other-window')  - Visit file(s) in a directory
`icicle-map'                           - Apply function to alist items
`icy-mode' or `icicle-mode'            - Toggle Icicle mode
`icicle-occur'                         - `occur' + apropos icompletion
`icicle-other-window-or-frame'         - Other window/frame or select
`icicle-plist'                         - Show symbols, property lists
`icicle-recent-file'(`-other-window')  - Open recently used file(s)
`icicle-remove-buffer-candidate'       - Remove always-candidate buf
`icicle-remove-buffer-config'          - From `icicle-buffer-configs'
`icicle-remove-region'             - Remove from `icicle-region-alist'
`icicle-remove-saved-completion-set'   - From
                                        `icicle-saved-completion-sets'
`icicle-reset-option-to-nil'           - Set binary option(s) to nil
`icicle-save-string-to-variable'       - Save text for use with `C-='
`icicle-search'                        - Search with regexps & cycling
`icicle-search-keywords'               - Search with regexp keywords
`icicle-search-region'                 - Search multiple regions
`icicle-search-word'                   - Whole-word search
`icicle-select-frame'                  - Select and raise a frame
`icicle-select-region'                 - Select from multiple regions
`icicle-select-window'                 - Select window by buffer name
`icicle-send-bug-report'               - Send Icicles bug report
`icicle-set-option-to-t'               - Set binary option(s) to t
`icicle-toggle-~-for-home-dir'         - Toggle using `~' for $HOME
`icicle-toggle-case-sensitivity'       - Toggle case sensitivity
`icicle-toggle-ignored-extensions'     - Toggle ignoring file suffixes
`icicle-toggle-ignored-space-prefix'   - Toggle ignoring space prefix
`icicle-toggle-incremental-completion' - Toggle apropos icompletion
`icicle-toggle-option'                 - Toggle binary user option(s)
`icicle-toggle-regexp-quote'           - Toggle regexp escaping
`icicle-toggle-search-cleanup'         - Toggle search highlighting
`icicle-toggle-sorting'                - Toggle sorting of completions
`icicle-toggle-transforming'           - Toggle duplicate removal
`icicle-toggle-WYSIWYG-Completions'    - Toggle WYSIWYG *Completions*
`icicle-vardoc'                        - Show variable description(s)
`icicle-yank-insert'                   - `yank' using completion
`toggle' (alias)                       - Toggle binary user option(s)

For more information, use `\\<minibuffer-local-completion-map>\\[icicle-completion-help]' \
when the minibuffer is active."
    (interactive "P")
    (setq icicle-mode (if arg (> (prefix-numeric-value arg) 0) (not icicle-mode)))
    (icicle-rebind-completion-maps icicle-mode)
    (cond (icicle-mode
           (icicle-define-icicle-maps)
           (icicle-rebind-non-completion-keys)
           ;; This is not really necessary after the first time - no great loss.
           (add-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (add-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
           (add-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (add-hook 'icicle-post-command-hook 'icicle-activate-mark 'append)
           ;; The pre- and post-command hooks are local to the minibuffer,
           ;; So they are added in `icicle-minibuffer-setup', not here.
           ;; Nevertheless, they are removed here when Icicle mode is exited.
           (add-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
           (add-hook 'completion-setup-hook    'icicle-set-calling-cmd 'append)
           (add-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
           (add-hook 'comint-mode-hook         'icicle-comint-hook-fn)
           (add-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
           (add-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
           (add-hook 'temp-buffer-show-hook    'icicle-fit-Completions-window)
           (icicle-redefine-std-completion-fns)
           (icicle-redefine-standard-commands)
           (icicle-redefine-standard-options)
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now ON"))
          (t
           (makunbound 'icicle-mode-map)
           (icicle-restore-non-completion-keys)
           (remove-hook 'minibuffer-setup-hook    'icicle-minibuffer-setup)
           (remove-hook 'minibuffer-exit-hook     'icicle-cancel-Help-redirection)
           (remove-hook 'minibuffer-exit-hook     'icicle-restore-region-face)
           (remove-hook 'icicle-post-command-hook 'icicle-activate-mark)
           (remove-hook 'pre-command-hook         'icicle-top-level-prep)
           (remove-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook t)
           (remove-hook 'post-command-hook        'icicle-run-icicle-post-command-hook t)
           (remove-hook 'isearch-mode-hook        'icicle-bind-isearch-keys)
           (remove-hook 'completion-setup-hook    'icicle-set-calling-cmd)
           (remove-hook 'kill-emacs-hook          'icicle-control-reminder-prompt)
           (remove-hook 'comint-mode-hook         'icicle-comint-hook-fn)
           (remove-hook 'compilation-mode-hook    'icicle-compilation-hook-fn)
           (remove-hook 'compilation-minor-mode-hook 'icicle-compilation-hook-fn)
           (remove-hook 'temp-buffer-show-hook    'icicle-fit-Completions-window)
           (icicle-restore-std-completion-fns)
           (icicle-restore-standard-commands)
           (icicle-restore-standard-options)
           (run-hooks 'icicle-mode-hook)
           (message "Icicle mode is now OFF"))))
  (add-to-list 'minor-mode-alist '(icicle-mode " Icy")))

(defun icicle-top-level-prep ()
  "Do top-level stuff.  Used in `pre-command-hook'."
  ;; Reset `icicle-candidates-alist' to nil; save top-level command.
  (when (= 0 (recursion-depth))
    (setq icicle-last-top-level-command this-command
          icicle-candidates-alist       nil)))

(defun icicle-define-icicle-maps ()
  "Define `icicle-mode-map' and `icicle-menu-map'."
  (setq icicle-mode-map (make-sparse-keymap)) ; Recreate it each time, to capture latest bindings.

  ;; Define Icicles menu-bar menu.  Create it only once.  Sacrifice latest bindings for speed.
  (unless icicle-menu-map
    (setq icicle-menu-map (make-sparse-keymap "Icicles"))
    (define-key icicle-menu-map [icicle-mode] '(menu-item "Turn Off Icicle Mode" icicle-mode))
    (define-key icicle-menu-map [icicle-abort]
      '(menu-item "Cancel Minibuffer" icicle-abort-minibuffer-input
        :enable (active-minibuffer-window)))
    (define-key icicle-menu-map [icicle-report-bug]
      '(menu-item "Send Bug Report" icicle-send-bug-report))
    (define-key icicle-menu-map [icicle-customize-icicles-group]
      '(menu-item "Customize Icicles" icicle-customize-icicles-group))
    (define-key icicle-menu-map [icicle-help]
      '(menu-item "Help" icicle-completion-help
        :help "Display help on minibuffer input and completion" :keys "C-?"))
    (define-key icicle-menu-map [icicle-separator-last] '("--"))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'Info-mode-menu)) ; Use Info menu, if available.
           (define-key Info-mode-menu [icicle-Info-goto-node]
             '(menu-item "[Icy] + Go to Node..." icicle-Info-goto-node :visible icicle-mode
               :enable (eq major-mode 'Info-mode)))
           (define-key Info-mode-menu [icicle-Info-index]
             '(menu-item "[Icy] + Look Up in Index..." icicle-Info-index :visible icicle-mode
               :enable (eq major-mode 'Info-mode)))
           (define-key Info-mode-menu [icicle-separator-Info]
             '(menu-item "--" icicle-separator-Info :visible icicle-mode
               :enable (eq major-mode 'Info-mode))))
          (t
           (define-key icicle-menu-map [icicle-Info-goto-node]
             '(menu-item "+ Go to Node..." icicle-Info-goto-node
               :enable (eq major-mode 'Info-mode)))
           (define-key icicle-menu-map [icicle-Info-index]
             '(menu-item "+ Look Up in Index..." icicle-Info-index
               :enable (eq major-mode 'Info-mode)))
           (define-key icicle-menu-map [icicle-separator-Info]
             '(menu-item "--" icicle-separator-Info :visible icicle-mode
               :enable (eq major-mode 'Info-mode)))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-frames-menu)) ; Defined in `menu-bar+.el'.
           (define-key menu-bar-frames-menu [icicle-separator-frame]
             '(menu-item "--" icicle-separator-frame :visible icicle-mode))
           (define-key menu-bar-frames-menu [icicle-font]
             '(menu-item "[Icy] + Change Font" icicle-font :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key menu-bar-frames-menu [icicle-frame-fg]
             '(menu-item "[Icy] + Change Foreground..." icicle-frame-fg :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key menu-bar-frames-menu [icicle-frame-bg]
             '(menu-item "[Icy] + Change Background..." icicle-frame-bg :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-font]
             '(menu-item "+ Change Font of Frame..." icicle-font
               :enable (and icicle-mode
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-frame-fg]
             '(menu-item "+ Change Foreground of Frame..." icicle-frame-fg
               :enable (and icicle-mode
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-frame-bg]
             '(menu-item "+ Change Background of Frame..." icicle-frame-bg
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-separator-frame] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-describe-menu)) ; Use Describe menu, if available.
           (define-key menu-bar-describe-menu [icicle-separator-doc]
             '(menu-item "--" icicle-separator-doc :visible icicle-mode))
           (define-key menu-bar-describe-menu [icicle-plist]
             '(menu-item "[Icy] + Symbol with Property List..." icicle-plist :visible icicle-mode))
           (define-key menu-bar-describe-menu [icicle-doc]
             '(menu-item "[Icy] + Doc of Fun, Var, or Face..." icicle-doc :visible icicle-mode))
           (define-key menu-bar-describe-menu [icicle-fundoc]
             '(menu-item "[Icy] + Function with Name, Doc..." icicle-fundoc :visible icicle-mode))
           (define-key menu-bar-describe-menu [icicle-vardoc]
             '(menu-item "[Icy] + Variable with Name, Doc..." icicle-vardoc :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-plist]
             '(menu-item "+ Symbol with Property List..." icicle-plist))
           (define-key icicle-menu-map [icicle-doc]
             '(menu-item "+ Doc of Fun, Var, or Face..." icicle-doc))
           (define-key icicle-menu-map [icicle-fundoc]
             '(menu-item "+ Describe Function with Name, Doc..." icicle-fundoc))
           (define-key icicle-menu-map [icicle-vardoc]
             '(menu-item "+ Describe Variable with Name, Doc..." icicle-vardoc))
           (define-key icicle-menu-map [icicle-separator-doc] '("--"))))

    (define-key icicle-menu-map [icicle-map]
      '(menu-item "+ Apply Function to Alist Items..." icicle-map))
    (define-key icicle-menu-map [icicle-save-string-to-variable]
      '(menu-item "Save String to Variable..." icicle-save-string-to-variable))
    (define-key icicle-menu-map [icicle-color-theme]
      '(menu-item "+ Choose Color Theme..." icicle-color-theme
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-remove-saved-completion-set]
      '(menu-item "+ Remove Saved Candidate Set..." icicle-remove-saved-completion-set
        :enable icicle-saved-completion-sets))
    (define-key icicle-menu-map [icicle-add/update-saved-completion-set]
      '(menu-item "Add/Update Saved Candidate Set..." icicle-add/update-saved-completion-set))
    (when (fboundp 'icicle-kmacro)
      (define-key icicle-menu-map [icicle-kmacro]
        '(menu-item "+ Execute Nth Keyboard Macro..." icicle-kmacro
          :enable (or (kmacro-ring-head) kmacro-ring))))
    (define-key icicle-menu-map [icicle-execute-named-keyboard-macro]
      '(menu-item "+ Execute Named Keyboard Macro..." icicle-execute-named-keyboard-macro))
    (define-key icicle-menu-map [icicle-separator-misc] '("--"))
    (define-key icicle-menu-map [icicle-imenu]
      '(menu-item "+ Imenu..." icicle-imenu
        :enable imenu-generic-expression))
    (define-key icicle-menu-map [icicle-goto-global-marker]
      '(menu-item "+ Go To Global Mark..." icicle-goto-global-marker
        :enable (consp (icicle-markers global-mark-ring))))
    (define-key icicle-menu-map [icicle-goto-marker]
      '(menu-item "+ Go To Mark..." icicle-goto-marker
        :enable (consp (icicle-markers mark-ring))))
    (define-key icicle-menu-map [icicle-separator-goto] '("--"))

    (define-key icicle-menu-map [icicle-remove-region]
      '(menu-item "+ Remove Saved Region from List..." icicle-remove-region
        :enable icicle-region-alist))
    (define-key icicle-menu-map [icicle-add-region]
      '(menu-item "Save Current Region" icicle-add-region
        :enable mark-active :keys "C-9 C-x C-x"))
    (define-key icicle-menu-map [icicle-search-region]
      '(menu-item "+ Search Saved Region..." icicle-search-region
        :enable icicle-region-alist))
    (define-key icicle-menu-map [icicle-select-region]
      '(menu-item "+ Choose Saved Region..." icicle-select-region
        :enable icicle-region-alist :keys "C-u C-x C-x"))
    (define-key icicle-menu-map [icicle-separator-region] '("--"))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-apropos-menu)) ; Use Apropos menu, if available.
           (define-key menu-bar-apropos-menu [icicle-separator-apropos]
             '(menu-item "--" icicle-separator-apropos :visible icicle-mode))
           (define-key menu-bar-apropos-menu [icicle-apropos-zippy]
             '(menu-item "[Icy] Zippy..." icicle-apropos-zippy :visible icicle-mode))
           (cond ((fboundp 'apropos-option)
                  (define-key menu-bar-apropos-menu [icicle-apropos]
                    '(menu-item "[Icy] Symbols..." icicle-apropos :visible icicle-mode))
                  (define-key menu-bar-apropos-menu [icicle-apropos-function]
                    '(menu-item "[Icy] Functions..." icicle-apropos-function :visible icicle-mode))
                  (define-key menu-bar-apropos-menu [icicle-apropos-variable]
                    '(menu-item "[Icy] Variables..." icicle-apropos-variable :visible icicle-mode))
                  (define-key menu-bar-apropos-menu [icicle-apropos-option]
                    '(menu-item "[Icy] Options..." icicle-apropos-option :visible icicle-mode))
                  (define-key menu-bar-apropos-menu [icicle-apropos-command]
                    '(menu-item "[Icy] Commands..." icicle-apropos-command :visible icicle-mode)))
                 (t
                  (define-key menu-bar-apropos-menu [icicle-apropos-variable]
                    '(menu-item "[Icy] Variables..." icicle-apropos-variable
                      :visible icicle-mode))))
           (define-key menu-bar-apropos-menu [icicle-apropos-command]
             '(menu-item "[Icy] Commands..." icicle-apropos-command :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-apropos-zippy]
             '(menu-item "Apropos Zippy..." icicle-apropos-zippy))
           (cond ((fboundp 'apropos-option)
                  (define-key icicle-menu-map [icicle-apropos]
                    '(menu-item "Apropos..." icicle-apropos))
                  (define-key icicle-menu-map [icicle-apropos-function]
                    '(menu-item "Apropos Functions..." icicle-apropos-function))
                  (define-key icicle-menu-map [icicle-apropos-variable]
                    '(menu-item "Apropos Variables..." icicle-apropos-variable))
                  (define-key icicle-menu-map [icicle-apropos-option]
                    '(menu-item "Apropos Options..." icicle-apropos-option))
                  (define-key icicle-menu-map [icicle-apropos-command]
                    '(menu-item "Apropos Commands..." icicle-apropos-command)))
                 (t
                  (define-key icicle-menu-map [icicle-apropos-variable]
                    '(menu-item "Apropos Variables..." icicle-apropos-variable))
                  (define-key icicle-menu-map [icicle-apropos-command]
                    '(menu-item "Apropos Commands..." icicle-apropos-command))))
           (define-key icicle-menu-map [icicle-separator-apropos] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-options-menu)) ; Use Options menu, if available.
           (define-key menu-bar-options-menu [icicle-separator-toggle]
             '(menu-item "--" icicle-separator-toggle :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-set-option-to-t]
             '(menu-item "[Icy] + Turn On Any Option..." icicle-set-option-to-t
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-reset-option-to-nil]
             '(menu-item "[Icy] + Turn Off Any Option..." icicle-reset-option-to-nil
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-option]
             '(menu-item "[Icy] + Toggle Any Option..." icicle-toggle-option :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-~-for-home-dir]
             '(menu-item "[Icy] Toggle Using `~' For $HOME" icicle-toggle-~-for-home-dir
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-WYSIWYG-Completions]
             '(menu-item "[Icy] Toggle WYSIWYG For *Completions*" icicle-toggle-WYSIWYG-Completions
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-highlight-all-current]
             '(menu-item "[Icy] Toggle All-Current Search Highlighting"
               icicle-toggle-highlight-all-current :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-search-cleanup]
             '(menu-item "[Icy] Toggle Removal of Search Highlighting"
               icicle-toggle-search-cleanup :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-regexp-quote]
             '(menu-item "[Icy] Toggle Escaping Special Chars" icicle-toggle-regexp-quote
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-incremental-completion]
             '(menu-item "[Icy] Toggle Incremental Completion"
               icicle-toggle-incremental-completion :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-ignore]
             '(menu-item "[Icy] Toggle Ignoring Space Prefix" icicle-toggle-ignored-space-prefix
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-ignore]
             '(menu-item "[Icy] Toggle Ignored File Extensions" icicle-toggle-ignored-extensions
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-angle-brackets]
             '(menu-item "[Icy] Toggle Angle Brackets" icicle-toggle-angle-brackets
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-highlight-historical-candidates]
             '(menu-item "[Icy] Toggle Highlighting Past Inputs"
               icicle-toggle-highlight-historical-candidates :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-case-sensitivity]
             '(menu-item "[Icy] Toggle Case Sensitivity" icicle-toggle-case-sensitivity
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-search-replace-whole]
             '(menu-item "[Icy] Toggle Replacing Whole Search Hit"
               icicle-toggle-search-replace-whole
               :visible icicle-mode :enable icicle-searching-p :keys "C-,"))
           (define-key menu-bar-options-menu [icicle-toggle-transforming]
             '(menu-item "[Icy] Toggle Duplicate Removal" icicle-toggle-transforming
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-toggle-alternative-sorting]
             '(menu-item "[Icy] Swap Alternative Sort" icicle-toggle-alternative-sorting
               :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-change-alternative-sort-order]
             '(menu-item "[Icy] Change Alternative Sort Order"
               icicle-change-alternative-sort-order :visible icicle-mode))
           (define-key menu-bar-options-menu [icicle-change-sort-order]
             '(menu-item "[Icy] Change Sort Order" icicle-change-sort-order :visible icicle-mode
               :enable (not icicle-inhibit-sort-p))))
          (t
           (define-key icicle-menu-map [icicle-set-option-to-t]
             '(menu-item "+ Turn On Any Option..." icicle-set-option-to-t))
           (define-key icicle-menu-map [icicle-reset-option-to-nil]
             '(menu-item "+ Turn Off Any Option..." icicle-reset-option-to-nil))
           (define-key icicle-menu-map [icicle-toggle-option]
             '(menu-item "+ Toggle Any Option..." icicle-toggle-option))
           (define-key icicle-menu-map [icicle-toggle-~-for-home-dir]
             '(menu-item "Toggle Using `~' For $HOME" icicle-toggle-~-for-home-dir))
           (define-key icicle-menu-map [icicle-toggle-WYSIWYG-Completions]
             '(menu-item "Toggle WYSIWYG For *Completions*" icicle-toggle-WYSIWYG-Completions))
           (define-key icicle-menu-map [icicle-toggle-highlight-all-current]
             '(menu-item "Toggle All-Current Search Highlighting"
               icicle-toggle-highlight-all-current))
           (define-key icicle-menu-map [icicle-toggle-search-cleanup]
             '(menu-item "Toggle Removal of Search Highlighting" icicle-toggle-search-cleanup))
           (define-key icicle-menu-map [icicle-toggle-regexp-quote]
             '(menu-item "Toggle Escaping Special Chars" icicle-toggle-regexp-quote))
           (define-key icicle-menu-map [icicle-toggle-incremental-completion]
             '(menu-item "Toggle Incremental Completion" icicle-toggle-incremental-completion))
           (define-key icicle-menu-map [icicle-toggle-ignore]
             '(menu-item "Toggle Ignoring Space Prefix" icicle-toggle-ignored-space-prefix))
           (define-key icicle-menu-map [icicle-toggle-ignore]
             '(menu-item "Toggle Ignored File Extensions" icicle-toggle-ignored-extensions))
           (define-key icicle-menu-map [icicle-toggle-angle-brackets]
             '(menu-item "Toggle Angle Brackets" icicle-toggle-angle-brackets))
           (define-key icicle-menu-map [icicle-toggle-highlight-historical-candidates]
             '(menu-item "Toggle Highlighting Past Inputs"
               icicle-toggle-highlight-historical-candidates))
           (define-key icicle-menu-map [icicle-toggle-case-sensitivity]
             '(menu-item "Toggle Case Sensitivity" icicle-toggle-case-sensitivity))
           (define-key icicle-menu-map [icicle-toggle-search-replace-whole]
             '(menu-item "Toggle Replacing Whole Search Hit" icicle-toggle-search-replace-whole
               :enable icicle-searching-p :keys "C-,"))
           (define-key icicle-menu-map [icicle-toggle-transforming]
             '(menu-item "[Toggle Duplicate Removal" icicle-toggle-transforming))
           (define-key icicle-menu-map [icicle-toggle-alternative-sorting]
             '(menu-item "Swap Alternative Sort" icicle-toggle-alternative-sorting))
           (define-key icicle-menu-map [icicle-change-alternative-sort-order]
             '(menu-item "Change Alternative Sort Order" icicle-change-alternative-sort-order))
           (define-key icicle-menu-map [icicle-change-sort-order]
             '(menu-item "Change Sort Order" icicle-change-sort-order
               :enable (not icicle-inhibit-sort-p)))
           (define-key icicle-menu-map [icicle-separator-toggle] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-custom-menu)) ; Use Customize menu, if available.
           (define-key menu-bar-custom-menu [icicle-separator-customize]
             '(menu-item "--" icicle-separator-customize :visible icicle-mode))
           (define-key menu-bar-custom-menu [icicle-customize-apropos-groups]
             '(menu-item "[Icy] Groups Matching Regexp..." icicle-customize-apropos-groups
               :visible icicle-mode))
           (define-key menu-bar-custom-menu [icicle-customize-apropos-faces]
             '(menu-item "[Icy] Faces Matching Regexp..." icicle-customize-apropos-faces
               :visible icicle-mode))
           (define-key menu-bar-custom-menu [icicle-customize-face]
             '(menu-item "[Icy] + Face" icicle-customize-face :visible icicle-mode))
           (define-key menu-bar-custom-menu [icicle-customize-apropos-options]
             '(menu-item "[Icy] Options Matching Regexp..." icicle-customize-apropos-options
               :visible icicle-mode))
           (define-key menu-bar-custom-menu [icicle-customize-apropos]
             '(menu-item "[Icy] Settings Matching Regexp..." icicle-customize-apropos
               :visible icicle-mode)))
          (t
           (define-key icicle-menu-map [icicle-separator-customize] '("--"))
           (define-key icicle-menu-map [icicle-customize-apropos-groups]
             '(menu-item "Groups Matching Regexp..." icicle-customize-apropos-groups))
           (define-key icicle-menu-map [icicle-customize-apropos-faces]
             '(menu-item "Faces Matching Regexp..." icicle-customize-apropos-faces))
           (define-key icicle-menu-map [icicle-customize-face]
             '(menu-item "+ Face" icicle-customize-face))
           (define-key icicle-menu-map [icicle-customize-apropos-options]
             '(menu-item "Options Matching Regexp..." icicle-customize-apropos-options))
           (define-key icicle-menu-map [icicle-customize-apropos]
             '(menu-item "Settings Matching Regexp..." icicle-customize-apropos))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-edit-menu)) ; Use Edit menu, if available.
           (define-key menu-bar-edit-menu [icicle-separator-edit]
             '(menu-item "--" icicle-separator-edit :visible icicle-mode))
           (define-key menu-bar-edit-menu [icicle-complete-thesaurus-entry]
             '(menu-item "[Icy] Complete with Thesaurus..." icicle-complete-thesaurus-entry
               :visible icicle-mode
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key menu-bar-edit-menu [icicle-insert-thesaurus-entry]
             '(menu-item "[Icy] + Insert Thesaurus Entry..." icicle-insert-thesaurus-entry
               :visible icicle-mode
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key menu-bar-edit-menu [icicle-insert-kill]
             '(menu-item "[Icy] + Paste Copied Text..." icicle-insert-kill :visible icicle-mode
               :enable (not buffer-read-only) :keys "C-y")))
          (t
           (define-key icicle-menu-map [icicle-separator-edit] '("--"))
           (define-key icicle-menu-map [icicle-complete-thesaurus-entry]
             '(menu-item "Complete with Thesaurus..." icicle-complete-thesaurus-entry
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-menu-map [icicle-insert-thesaurus-entry]
             '(menu-item "+ Insert Thesaurus Entry..." icicle-insert-thesaurus-entry
               :enable (and (not buffer-read-only) (boundp 'synonyms-obarray))))
           (define-key icicle-menu-map [icicle-insert-kill]
             '(menu-item "+ Paste Copied Text..." icicle-insert-kill
               :enable (not buffer-read-only) :keys "C-y"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-file-menu)) ; Use File menu, if available.
           (define-key menu-bar-file-menu [icicle-separator-file]
             '(menu-item "--" icicle-separator-file :visible icicle-mode))
           (define-key menu-bar-file-menu [icicle-kill-buffer]
             '(menu-item "[Icy] + Kill Buffer..." icicle-kill-buffer :visible icicle-mode))
           (define-key menu-bar-file-menu [icicle-delete-file]
             '(menu-item "[Icy] + Delete File..." icicle-delete-file :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (when (condition-case nil (require 'recentf) (error nil))
             (define-key menu-bar-file-menu [icicle-recent-file-other-window]
               '(menu-item "[Icy] + Open Recent File (Other Window)..."
                 icicle-recent-file-other-window :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key menu-bar-file-menu [icicle-recent-file]
               '(menu-item "[Icy] + Open Recent File..." icicle-recent-file :visible icicle-mode
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key menu-bar-file-menu [icicle-dired-saved-file-candidates-other-window]
             '(menu-item "[Icy] Dired Saved Candidates (Other Window)..."
               icicle-dired-saved-file-candidates-other-window :visible icicle-mode
               :enable (and icicle-saved-completion-candidates
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key menu-bar-file-menu [icicle-dired-saved-file-candidates]
             '(menu-item "[Icy] Dired Saved Candidates..." icicle-dired-saved-file-candidates
               :visible icicle-mode
               :enable (and icicle-saved-completion-candidates
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key menu-bar-file-menu [icicle-locate-file]
             '(menu-item "[Icy] + Open File under Directory..." icicle-locate-file
               :visible icicle-mode))
           (define-key menu-bar-file-menu [icicle-locate-file-other-window]
             '(menu-item "[Icy] + Open File under Directory (Other Window)..."
               icicle-locate-file-other-window :visible icicle-mode))
           (define-key menu-bar-file-menu [icicle-find-file-other-window]
             '(menu-item "[Icy] + Open File or Directory (Other Window)..."
               icicle-find-file-other-window :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key menu-bar-file-menu [icicle-find-file]
             '(menu-item "[Icy] + Open File or Directory..." icicle-find-file :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-kill-buffer]
             '(menu-item "+ Kill Buffer..." icicle-kill-buffer))
           (define-key icicle-menu-map [icicle-delete-file]
             '(menu-item "+ Delete File..." icicle-delete-file
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (when (condition-case nil (require 'recentf) (error nil))
             (define-key icicle-menu-map [icicle-recent-file-other-window]
               '(menu-item "+ Open Recent File (Other Window)..." icicle-recent-file-other-window
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
             (define-key icicle-menu-map [icicle-recent-file]
               '(menu-item "+ Open Recent File..." icicle-recent-file
                 :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-dired-saved-file-candidates-other-window]
             '(menu-item "Dired Saved Candidates (Other Window)..."
               icicle-dired-saved-file-candidates-other-window
               :enable (and icicle-saved-completion-candidates
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-dired-saved-file-candidates]
             '(menu-item "Dired Saved Candidates..." icicle-dired-saved-file-candidates
               :enable (and icicle-saved-completion-candidates
                        (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
           (define-key icicle-menu-map [icicle-locate-file-other-window]
             '(menu-item "+ Open File under Directory (Other Window)..."
               icicle-locate-file-other-window))
           (define-key icicle-menu-map [icicle-locate-file]
             '(menu-item "+ Open File under Directory..." icicle-locate-file))
           (define-key icicle-menu-map [icicle-find-file-other-window]
             '(menu-item "+ Open File or Directory (Other Window)..."
               icicle-find-file-other-window
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-find-file]
             '(menu-item "+ Open File or Directory ..." icicle-find-file
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))))
    (define-key icicle-menu-map [icicle-buffer-list]
      '(menu-item "+ Buffer List..." icicle-buffer-list))
    (define-key icicle-menu-map [icicle-remove-buffer-config]
      '(menu-item "+ Remove Buffer Configuration..." icicle-remove-buffer-config
        :enable icicle-buffer-configs))
    (define-key icicle-menu-map [icicle-add-buffer-config]
      '(menu-item "New Buffer Configuration..." icicle-add-buffer-config))
    (define-key icicle-menu-map [icicle-buffer-config]
      '(menu-item "+ Choose Buffer Configuration..." icicle-buffer-config
        :enable icicle-buffer-configs))
    (define-key icicle-menu-map [icicle-remove-buffer-candidate]
      '(menu-item "+ Don't Always Include Buffer..." icicle-remove-buffer-candidate
        :enable icicle-buffer-extras))
    (define-key icicle-menu-map [icicle-add-buffer-candidate]
      '(menu-item "+ Always Include Buffer..." icicle-add-buffer-candidate))
    (define-key icicle-menu-map [icicle-kill-buffer]
      '(menu-item "+ Kill Buffer..." icicle-kill-buffer))
    (define-key icicle-menu-map [icicle-delete-windows]
      '(menu-item "+ Delete Windows on Buffer..." icicle-delete-windows :keys "C-u C-x 0"))
    (define-key icicle-menu-map [icicle-buffer-other-window]
      '(menu-item "+ Switch to Buffer (Other Window)..." icicle-buffer-other-window
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (define-key icicle-menu-map [icicle-buffer]
      '(menu-item "+ Switch to Buffer..." icicle-buffer
        :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-bookmark-map)) ; Use Bookmarks menu, if available.
           (require 'bookmark)          ; `bookmark-buffer-name' is not autoloaded.
           (define-key menu-bar-bookmark-map [icicle-bookmark]
             '(menu-item "[Icy] + Jump to Bookmark Using Icicles..." icicle-bookmark
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-bookmark]
             '(menu-item "+ Jump To Bookmark..." icicle-bookmark
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-separator-bookmark-buffer] '("--"))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-search-tags-menu)) ; Use Tags menu, if available - `menu-bar+.el'
           (define-key menu-bar-search-tags-menu [icicle-find-tag]
             '(menu-item "[Icy] + Find Tag ..." icicle-find-tag :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key menu-bar-search-tags-menu [icicle-find-tag-other-window]
             '(menu-item "[Icy] + Find Tag Other Window ..." icicle-find-tag-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          ((and (not icicle-touche-pas-aux-menus-flag) ; Use Search, if available and no Tags menu.
                (boundp 'menu-bar-search-menu))
           (define-key menu-bar-search-menu [icicle-find-tag]
             '(menu-item "[Icy] + Find Tag ..." icicle-find-tag :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key menu-bar-search-menu [icicle-find-tag-other-window]
             '(menu-item "[Icy] + Find Tag Other Window ..." icicle-find-tag-other-window
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-find-tag]
             '(menu-item "Find Tag ..." icicle-find-tag
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-find-tag-other-window]
             '(menu-item "Find Tag Other Window ..." icicle-find-tag-other-window
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))))

    (cond ((and (not icicle-touche-pas-aux-menus-flag)
                (boundp 'menu-bar-search-menu)) ; Use Search menu, if available.
           (define-key menu-bar-search-menu [icicle-separator-search]
             '(menu-item "--" icicle-separator-search :visible icicle-mode))
           (define-key menu-bar-search-menu [icicle-search-highlight-cleanup]
             '(menu-item "[Icy] Remove Search Highlighting..." icicle-search-highlight-cleanup
               :visible icicle-mode
               :enable (or icicle-search-overlays (overlayp icicle-search-current-overlay)
                        (overlayp icicle-search-refined-overlays) icicle-search-refined-overlays)))
           (define-key menu-bar-search-menu [icicle-compilation-search]
             '(menu-item "[Icy] + Search Compilation/Grep Hits (Regexp)..."
               icicle-compilation-search :visible icicle-mode
               :enable (and (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
                        (condition-case nil (eq (current-buffer) (compilation-find-buffer))
                          (error nil)))
               :keys "C-`"))
           (define-key menu-bar-search-menu [icicle-imenu-non-interactive-function]
             '(menu-item "[Icy] + Search Non-Interactive Function Definition (Regexp)..."
               icicle-imenu-non-interactive-function
               :visible icicle-mode :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key menu-bar-search-menu [icicle-imenu-command]
             '(menu-item "[Icy] + Search Command Definition (Regexp)..." icicle-imenu-command
               :visible icicle-mode :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key menu-bar-search-menu [icicle-imenu]
             '(menu-item "[Icy] + Search Definition (Regexp)..." icicle-imenu
               :visible icicle-mode :enable imenu-generic-expression))
           (define-key menu-bar-search-menu [icicle-search-all-regions]
             '(menu-item "[Icy] + Search All Saved Regions (Regexp)..." icicle-search-all-regions
               :visible icicle-mode :keys "C-u C-`"))
           (define-key menu-bar-search-menu [icicle-search-region]
             '(menu-item "[Icy] + Search Saved Region (Regexp)..." icicle-search-region
               :visible icicle-mode))
           (define-key menu-bar-search-menu [icicle-search-file]
             '(menu-item "[Icy] + Search File (Regexp)..." icicle-search-file
               :visible icicle-mode))
           (define-key menu-bar-search-menu [icicle-search-buffer]
             '(menu-item "[Icy] + Search Buffer (Regexp)..." icicle-search-buffer
               :visible icicle-mode))
           (define-key menu-bar-search-menu [icicle-search-text-property]
             '(menu-item "[Icy] + Search Text Property..." icicle-search-text-property
               :visible icicle-mode))
           (define-key menu-bar-search-menu [icicle-search-word]
             '(menu-item "[Icy] + Search for Word..." icicle-search-word :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key menu-bar-search-menu [icicle-search-keywords]
             '(menu-item "[Icy] + Search with Keywords (Regexps)..." icicle-search-keywords
               :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key menu-bar-search-menu [icicle-search]
             '(menu-item "[Icy] + Search (Regexp)..." icicle-search :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
               :keys: "C-`"))
           (define-key menu-bar-search-menu [icicle-occur]
             '(menu-item "[Icy] + Occur (Regexp)..." icicle-occur :visible icicle-mode
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))
          (t
           (define-key icicle-menu-map [icicle-search-highlight-cleanup]
             '(menu-item "Remove Search Highlighting..." icicle-search-highlight-cleanup
               :enable (or icicle-search-overlays (overlayp icicle-search-current-overlay)
                        (overlayp icicle-search-refined-overlays)
                        icicle-search-refined-overlays)))
           (define-key icicle-menu-map [icicle-compilation-search]
             '(menu-item "+ Search Compilation/Grep Hits (Regexp)..." icicle-compilation-search
               :enable (and (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))
                        (condition-case nil (eq (current-buffer) (compilation-find-buffer))
                          (error nil)))))
           (define-key icicle-menu-map [icicle-imenu-non-interactive-function]
             '(menu-item "Search Non-Interactive Function Definition (Regexp)..."
               icicle-imenu-non-interactive-function
               :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-menu-map [icicle-imenu-command]
             '(menu-item "Search Command Definition (Regexp)..." icicle-imenu-command
               :enable (eq major-mode 'emacs-lisp-mode)))
           (define-key icicle-menu-map [icicle-imenu]
             '(menu-item "+ Search Definition (Regexp)..." icicle-imenu
               :enable imenu-generic-expression))
           (define-key icicle-menu-map [icicle-search-all-regions]
             '(menu-item "+ Search All Saved Regions (Regexp)..." icicle-search-all-regions))
           (define-key icicle-menu-map [icicle-search-region]
             '(menu-item "+ Search Saved Region (Regexp)..." icicle-search-region))
           (define-key icicle-menu-map [icicle-search-file]
             '(menu-item "+ Search File (Regexp)..." icicle-search-file))
           (define-key icicle-menu-map [icicle-search-buffer]
             '(menu-item "+ Search Buffer (Regexp)..." icicle-search-buffer))
           (define-key icicle-menu-map [icicle-search-text-property]
             '(menu-item "+ Search Text Property..." icicle-search-text-property))
           (define-key icicle-menu-map [icicle-search-word]
             '(menu-item "+ Search for Word..." icicle-search-word
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-search-keywords]
             '(menu-item "+ Search with Keywords (Regexps)..." icicle-search-keywords
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-search]
             '(menu-item "+ Search (Regexp)..." icicle-search
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame)))))
           (define-key icicle-menu-map [icicle-occur]
             '(menu-item "+ Occur (Regexp)..." icicle-occur
               :enable (not (window-minibuffer-p (frame-selected-window menu-updating-frame))))))))

  ;; Install Icicles menu-bar menu.
  (define-key icicle-mode-map [menu-bar icicles] (cons "Icicles" icicle-menu-map))

  ;; Optional `icicle-mode-map' bindings - governed by `icicle-bind-top-level-commands-flag'.
  (when icicle-bind-top-level-commands-flag
    (define-key icicle-mode-map [pause]    'icicle-switch-to/from-minibuffer)
    (define-key icicle-mode-map "\C-c`"    'icicle-search-generic) ; `C-c `'
    (define-key icicle-mode-map "\C-c$"    'icicle-search-word) ; `C-c $'
    (define-key icicle-mode-map "\C-c^"    'icicle-search-keywords) ; `C-c ^'
    (define-key icicle-mode-map "\C-c'"    'icicle-occur) ; `C-c ''
    (define-key icicle-mode-map "\C-c="    'icicle-imenu) ; `C-c ='
    (define-key icicle-mode-map "\C-c\""   'icicle-search-text-property) ; `C-c "'
    (define-key icicle-mode-map "\C-c/"    'icicle-complete-thesaurus-entry) ; `C-c /'
    (define-key icicle-mode-map "\C-x\M-e" 'icicle-execute-named-keyboard-macro) ; `C-x M-e'
    (when (fboundp 'icicle-kmacro)
      (define-key icicle-mode-map [f5] 'icicle-kmacro)) ; `f5' Emacs 22
      
    ;; Remap some top-level commands.
    (icicle-remap 'abort-recursive-edit                   'icicle-abort-minibuffer-input
                  icicle-mode-map (current-global-map)) ; `C-g'
    (icicle-remap 'minibuffer-keyboard-quit               'icicle-abort-minibuffer-input
                  icicle-mode-map (current-global-map)) ; `C-g'
    (icicle-remap 'execute-extended-command               'icicle-execute-extended-command
                  icicle-mode-map (current-global-map)) ; `M-x'
    (icicle-remap 'switch-to-buffer                       'icicle-buffer
                  icicle-mode-map (current-global-map)) ; `C-x b'
    (icicle-remap 'switch-to-buffer-other-window          'icicle-buffer-other-window
                  icicle-mode-map (current-global-map)) ; `C-x 4 b'
    (icicle-remap 'find-file                              'icicle-find-file
                  icicle-mode-map (current-global-map)) ; `C-x C-f'
    (icicle-remap 'find-file-other-window                 'icicle-find-file-other-window
                  icicle-mode-map (current-global-map)) ; `C-x 4 f'
    (when (fboundp 'command-remapping)  ; Not for Emacs 20 and 21: interferes with minibuffer `M-'.
      (icicle-remap 'find-tag                             'icicle-find-tag
                    icicle-mode-map (current-global-map))) ; `M-.'
    (icicle-remap 'find-tag-other-window                  'icicle-find-tag-other-window
                  icicle-mode-map (current-global-map)) ; `C-x 4 .'
    (icicle-remap 'kill-buffer                            'icicle-kill-buffer
                  icicle-mode-map (current-global-map)) ; `C-x k'
    (icicle-remap 'kill-buffer-and-its-windows            'icicle-kill-buffer
                  icicle-mode-map (current-global-map)) ; `C-x k'
    (icicle-remap 'delete-window                          'icicle-delete-window
                  icicle-mode-map (current-global-map)) ; `C-x 0'
    (icicle-remap 'delete-windows-for                     'icicle-delete-window
                  icicle-mode-map (current-global-map)) ; `C-x 0'
    (icicle-remap 'other-window-or-frame                  'icicle-other-window-or-frame
                  icicle-mode-map (current-global-map)) ; `C-x o'
    (icicle-remap 'other-window                           'icicle-other-window-or-frame
                  icicle-mode-map (current-global-map)) ; `C-x o'
    (icicle-remap 'exchange-point-and-mark                'icicle-exchange-point-and-mark
                  icicle-mode-map (current-global-map)) ; `C-x C-x'
    (icicle-remap icicle-yank-function                    'icicle-yank-insert
                  icicle-mode-map (current-global-map)) ; `C-y'
            
    ;; This is for Icicles Menu, not Icicles, but it's convenient to do this here.
    (when (fboundp 'icicle-execute-menu-command) ; Defined in `icicles-menu.el'.
      (define-key icicle-mode-map [?\e ?\M-x] 'icicle-execute-menu-command) ; `ESC M-x'
      (define-key icicle-mode-map [?\M-`] 'icicle-execute-menu-command) ; `M-`'
      (define-key icicle-mode-map [f10] 'icicle-execute-menu-command))) ; Replaces `tmm-menu'.

  ;; Bind `S-TAB' in `icicle-mode-map', for generic `S-TAB'.  Emacs 22.
  (when (fboundp 'map-keymap)
    (define-key icicle-mode-map [S-tab]         'icicle-generic-S-tab) ; `S-TAB'
    (define-key icicle-mode-map [S-iso-lefttab] 'icicle-generic-S-tab))

  ;; Install or update `icicle-mode-map'.
  (if icicle-minor-mode-map-entry
      (setcdr icicle-minor-mode-map-entry icicle-mode-map)
    (setq icicle-minor-mode-map-entry (cons 'icicle-mode icicle-mode-map))
    (add-to-list 'minor-mode-map-alist icicle-minor-mode-map-entry)))

(defun icicle-rebind-non-completion-keys ()
  "Rebind some keys in maps other than minibuffer maps and `icicle-mode-map'"
  ;; Replace some standard Info bindings.
  (if (not (boundp 'Info-mode-map))
      (eval-after-load "info"
        '(progn
          (icicle-remap 'Info-goto-node  'icicle-Info-goto-node-cmd  Info-mode-map) ; `g'
          (icicle-remap 'Info-index      'icicle-Info-index-cmd      Info-mode-map))) ; `i'
    (icicle-remap 'Info-goto-node  'icicle-Info-goto-node-cmd  Info-mode-map)
    (icicle-remap 'Info-index      'icicle-Info-index-cmd      Info-mode-map))

  ;; Bind `S-TAB' in major maps, for key completion.
  (when (fboundp 'map-keymap)           ; Emacs 22.
    (icicle-bind-S-TAB-in-keymaps-from (current-global-map))
    (mapcar #'icicle-bind-S-TAB-for-map-variable icicle-keymaps-for-key-completion)))

(defun icicle-bind-S-TAB-for-map-variable (keymap-var)
  "Bind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored."
  (let ((temp keymap-var))
    (when (boundp temp)
      (setq temp (symbol-value temp))
      (when (keymapp temp) (icicle-bind-S-TAB-in-keymaps-from temp)))))

(defun icicle-bind-S-TAB-in-keymaps-from (map)
  "Bind `S-TAB' to `icicle-complete-keys' in keymaps accessible from MAP."
  (dolist (key+map (accessible-keymaps map))
    (let ((map (cdr key+map)))
      (when (and (keymapp map) (not (stringp (car-safe (last map))))) ; Try to exclude menu maps.
        (unless (lookup-key map [S-tab])
          (condition-case nil (define-key map [S-tab] 'icicle-complete-keys)
            (error nil)))
        (unless (lookup-key map [S-iso-lefttab])
          (condition-case nil (define-key map [S-iso-lefttab] 'icicle-complete-keys)
            (error nil)))))))

(defun icicle-restore-non-completion-keys ()
  "Restore some bindings changed by `icicle-rebind-non-completion-keys'."
  (if (not (boundp 'Info-mode-map))
      (eval-after-load "info"
        '(progn
          (icicle-unmap 'Info-goto-node Info-mode-map 'icicle-Info-goto-node-cmd)
          (icicle-unmap 'Info-index     Info-mode-map 'icicle-Info-index-cmd)))
    (icicle-unmap 'Info-goto-node Info-mode-map 'icicle-Info-goto-node-cmd)
    (icicle-unmap 'Info-index     Info-mode-map 'icicle-Info-index-cmd))

  ;; Unbind `S-TAB' in major maps.
  (when (fboundp 'map-keymap)           ; Emacs 22.
    (icicle-unbind-S-TAB-in-keymaps-from (current-global-map))
    (mapcar #'icicle-unbind-S-TAB-for-map-variable icicle-keymaps-for-key-completion)))

(defun icicle-unbind-S-TAB-for-map-variable (keymap-var)
  "Unbind `S-TAB' in keymaps accessible from keymap KEYMAP-VAR.
KEYMAP-VAR should be bound to a keymap that has at least one prefix
keymap.  If KEYMAP-VAR is not bound to a keymap, it is ignored."
  (let ((temp keymap-var))
    (when (boundp temp)
      (setq temp (symbol-value temp))
      (when (keymapp temp) (icicle-unbind-S-TAB-in-keymaps-from temp)))))

(defun icicle-unbind-S-TAB-in-keymaps-from (map)
  "Unbind `S-TAB' in keymaps accessible from MAP."
  (dolist (key+map (accessible-keymaps map))
    (let ((map (cdr key+map)))
      (when (and (keymapp map) (not (stringp (car-safe (last map))))) ; Try to exclude menu maps.
        (when (eq (lookup-key map [S-tab]) 'icicle-complete-keys)
          (condition-case nil (define-key map [S-tab] nil) (error nil)))
        (when (eq (lookup-key map [S-iso-lefttab]) 'icicle-complete-keys)
          (condition-case nil (define-key map [S-iso-lefttab] nil) (error nil)))))))
 
;;(@* "Other Icicles functions that define Icicle mode")

;;; Other Icicles functions that define Icicle mode ------------------

(defun icicle-rebind-completion-maps (turn-on-p)
  "Rebind minibuffer completion maps to be able to cycle completions.
Also, update the bindings in the minibuffer-completion help variables.

This is called by `icicle-mode'.  When in Icicle mode, all keys that
are globally bound to `next-line' are rebound in the minibuffer to
`icicle-next-prefix-candidate', for minibuffer completion purposes.
Similarly for other keys."
  (cond
    (turn-on-p                          ; TURN IT ON ********************************

     ;; `minibuffer-local-map': default minibuffer map.
     (let ((map minibuffer-local-map))
       (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
         '(menu-item "Quit" icicle-abort-minibuffer-input
           :help "Cancel minibuffer input or recursive edit"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf completion-help]
         '(menu-item "Help" icicle-completion-help
           :help "Display help on minibuffer input and completion"))
       (define-key map [menu-bar minibuf icicle-insert-history-element]
         '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
           :enable (consp (symbol-value minibuffer-history-variable))
           :help "Use completion to insert a previously entered input in the minibuffer."))

       (define-key map [(control ?g)]  'icicle-abort-minibuffer-input)
       (define-key map [M-S-backspace] 'icicle-erase-minibuffer)
       (define-key map [M-S-delete]    'icicle-erase-minibuffer)
       (define-key map [(meta ?.)]     'icicle-insert-string-at-point)
       (define-key map [(control ?=)]  'icicle-insert-string-from-variable)
       (define-key map [(meta ?o)]     'icicle-insert-history-element)
       (define-key map [(meta ?k)]     'icicle-erase-minibuffer-or-history-element)
       (define-key map [(meta ?:)]     'icicle-pp-eval-expression))
     
     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map minibuffer-local-ns-map))
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-minibuffer-input
             :help "Cancel minibuffer input or recursive edit"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf completion-help]
           '(menu-item "Help" icicle-completion-help
             :help "Display help on minibuffer input and completion"))
         (define-key map [menu-bar minibuf icicle-insert-history-element]
           '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
             :enable (consp (symbol-value minibuffer-history-variable))
             :help "Use completion to insert a previously entered input in the minibuffer."))

         (define-key map [(control ?g)]  'icicle-abort-minibuffer-input)
         (define-key map [M-S-backspace] 'icicle-erase-minibuffer)
         (define-key map [M-S-delete]    'icicle-erase-minibuffer)
         (define-key map [(meta ?.)]     'icicle-insert-string-at-point)
         (define-key map [(control ?=)]  'icicle-insert-string-from-variable)
         (define-key map [(meta ?o)]     'icicle-insert-history-element)
         (define-key map [(meta ?k)]     'icicle-erase-minibuffer-or-history-element)
         (define-key map [(meta ?:)]     'icicle-pp-eval-expression)))
     
     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
       (let ((map minibuffer-local-isearch-map))
         (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
           '(menu-item "Quit" icicle-abort-minibuffer-input
             :help "Cancel minibuffer input or recursive edit"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf completion-help]
           '(menu-item "Help" icicle-completion-help
             :help "Display help on minibuffer input and completion"))
         (define-key map [menu-bar minibuf icicle-insert-history-element]
           '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
             :enable (consp (symbol-value minibuffer-history-variable))
             :help "Use completion to insert a previously entered input in the minibuffer."))

         (define-key map [(control ?g)]  'icicle-abort-minibuffer-input)
         (define-key map [M-S-backspace] 'icicle-erase-minibuffer)
         (define-key map [M-S-delete]    'icicle-erase-minibuffer)
         (define-key map [(meta ?.)]     'icicle-insert-string-at-point)
         (define-key map [(control ?=)]  'icicle-insert-string-from-variable)
         (define-key map [(meta ?o)]     'icicle-insert-history-element)
         (define-key map [(meta ?k)]     'icicle-erase-minibuffer-or-history-element)
         (define-key map [(meta ?:)]     'icicle-pp-eval-expression)))

     ;; `minibuffer-local-completion-map': completion map.
     (icicle-bind-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (unless (eq minibuffer-local-completion-map
                 (keymap-parent minibuffer-local-must-match-map))
       (icicle-bind-completion-keys minibuffer-local-must-match-map))
     (define-key minibuffer-local-must-match-map [S-return] ; `S-RET'
       'icicle-apropos-complete-and-exit)

     ;; `minibuffer-local-filename-completion-map' and `minibuffer-local-must-match-filename-map'
     ;; were introduced in Emacs 22, and they inherit from `minibuffer-local-completion' and
     ;; `minibuffer-local-must-match-map', respectively, so we need not do anything here for them.

     ;; `completion-list-mode-map': map for *Completions* buffer.
     ;; Abort on `C-g' or `q'.  Switch to minibuffer on `C-insert'.  Do not allow normal input.
     (let ((map completion-list-mode-map))
       (define-key map [(control ?g)]     'icicle-abort-minibuffer-input) ; `C-g'
       (define-key map "q"                'icicle-abort-minibuffer-input) ; `q'
       (define-key map [(control insert)] 'icicle-insert-completion) ; `C-insert'
       (define-key map [down]             'icicle-next-line) ; `down'
       (define-key map [up]               'icicle-previous-line) ; `up'
       (define-key map [S-iso-lefttab]    'icicle-move-to-previous-completion) ; `S-TAB'
       (define-key map [S-tab]            'icicle-move-to-previous-completion)
       (define-key map [left]             'icicle-move-to-previous-completion) ; `left'
       (define-key map [(control ?i)]     'icicle-move-to-next-completion) ; `TAB'
       (define-key map [tab]              'icicle-move-to-next-completion)
       (define-key map [right]            'icicle-move-to-next-completion) ; `right'
       (define-key map [S-down-mouse-2]   'icicle-mouse-remove-candidate) ; `S-mouse-2'
       (define-key map [S-mouse-2]        'ignore)
       (define-key map [C-S-down-mouse-2] 'icicle-mouse-candidate-alt-action) ; `C-S-mouse-2'
       (define-key map [C-S-mouse-2]      'ignore)
       (define-key map [C-down-mouse-2]   'icicle-mouse-candidate-action) ; `C-mouse-2'
       (define-key map [C-mouse-2]        'ignore)
       (define-key map [C-M-return] 
11000
      'icicle-help-on-candidate) ; `C-M-RET'
       (define-key map [C-M-down-mouse-2] 'icicle-mouse-help-on-candidate) ; `C-M-mouse-2'
       (define-key map [C-M-mouse-2]      'ignore)
       (define-key map [M-S-down-mouse-2] 'icicle-mouse-save-candidate) ; `M-S-mouse-2'
       (define-key map [M-S-mouse-2]      'ignore)
       (define-key map [M-down-mouse-2]   'icicle-mouse-candidate-read-fn-invoke) ; `M-mouse-2'
       (define-key map [M-mouse-2]        'ignore)
       (define-key map [C-down-mouse-3]   'icicle-Completions-mouse-3-menu) ; `C-mouse-3'
       (define-key map [C-mouse-3]        'ignore)
       (define-key map [M-down-mouse-3]   'icicle-mouse-candidate-set-save-more) ; `M-mouse-3'
       (define-key map [M-mouse-3]        'ignore)
       (define-key map [M-S-down-mouse-3] 'icicle-mouse-candidate-set-save) ; `M-S-mouse-3'
       (define-key map [M-S-mouse-3]      'ignore)
       (define-key map [mouse-3]          'icicle-mouse-save-then-kill) ; `mouse-3'
       (define-key map [(control ?>)]     'icicle-candidate-set-save-more) ; `C->'
       (define-key map [(control meta ?>)] 'icicle-candidate-set-save) ; `C-M->'
       (define-key map [(control ?\))]    'icicle-candidate-set-save-more-selected) ; `C-)'
       (define-key map [(control meta ?\))] 'icicle-candidate-set-save-selected) ; `C-M-)'
       (define-key map [(control meta ?<)] 'icicle-candidate-set-retrieve) ; `C-M-<'
       (define-key map [(control ?l)]      'icicle-retrieve-previous-input) ; `C-l'
;; (suppress-keymap map) ; Inhibit character self-insertion.
       ))

    (t                                  ; TURN IT OFF *******************************

     ;; `minibuffer-local-map': default minibuffer map.
     (let ((map minibuffer-local-map))
       (define-key map [menu-bar minibuf quit]
         '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
       (define-key map [menu-bar minibuf return]
         '(menu-item "Enter" exit-minibuffer
           :help "Terminate input and exit minibuffer" :keys "RET"))
       (define-key map [menu-bar minibuf completion-help] nil)
       (define-key map [menu-bar minibuf icicle-insert-history-element] nil)

       (define-key map [(control ?g)]  'abort-recursive-edit) ; `C-g'
       (define-key map [M-S-backspace] nil) ; `M-S-DEL'
       (define-key map [M-S-delete]    nil) ; `M-S-delete'
       (define-key map [(meta ?.)]     nil) ; `M-.'
       (define-key map [(control ?=)]  nil) ; `C-='
       (define-key map [(meta ?o)]     nil) ; `M-o'
       (define-key map [(meta ?k)]     nil) ; `M-k'
       (define-key map [(meta ?:)]     nil)) ; `M-:'

     ;; `minibuffer-local-ns-map': default minibuffer map when spaces are not allowed.
     ;; In Emacs 22+, local is parent of local-ns.
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-ns-map))
       (let ((map minibuffer-local-ns-map))
         (define-key map [menu-bar minibuf quit]
           '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf completion-help] nil)
         (define-key map [menu-bar minibuf icicle-insert-history-element] nil)

         (define-key map [(control ?g)]  'abort-recursive-edit) ; `C-g'
         (define-key map [M-S-backspace] nil) ; `M-S-DEL'
         (define-key map [M-S-delete]    nil) ; `M-S-delete'
         (define-key map [(meta ?.)]     nil) ; `M-.'
         (define-key map [(control ?=)]  nil) ; `C-='
         (define-key map [(meta ?o)]     nil) ; `M-o'
         (define-key map [(meta ?k)]     nil) ; `M-k'
         (define-key map [(meta ?:)]     nil))) ; `M-:'

     ;; `minibuffer-local-isearch-map': minibuffer map for editing isearch strings.
     ;; In Emacs 21+, local is parent of local-isearch
     (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-isearch-map))
       (let ((map minibuffer-local-isearch-map))
         (define-key map [menu-bar minibuf quit]
           '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
         (define-key map [menu-bar minibuf return]
           '(menu-item "Enter" exit-minibuffer
             :help "Terminate input and exit minibuffer" :keys "RET"))
         (define-key map [menu-bar minibuf completion-help] nil)
         (define-key map [menu-bar minibuf icicle-insert-history-element] nil)
         
         (define-key map [(control ?g)]  'abort-recursive-edit) ; `C-g'
         (define-key map [M-S-backspace] nil) ; `M-S-DEL'
         (define-key map [M-S-delete]    nil) ; `M-S-delete'
         (define-key map [(meta ?.)]     nil) ; `M-.'
         (define-key map [(control ?=)]  nil) ; `C-='
         (define-key map [(meta ?o)]     nil) ; `M-o'
         (define-key map [(meta ?k)]     nil) ; `M-k'
         (define-key map [(meta ?:)]     nil))) ; `M-:'
     
     ;; `minibuffer-local-completion-map': completion map.
     (icicle-restore-completion-keys minibuffer-local-completion-map)

     ;; `minibuffer-local-must-match-map': must-match map.
     ;; In Emacs 22+, local-completion is parent of local-must-match
     (unless (eq minibuffer-local-completion-map (keymap-parent minibuffer-local-must-match-map))
       (icicle-restore-completion-keys minibuffer-local-must-match-map))
     (define-key minibuffer-local-must-match-map [S-return] nil)

     ;; `minibuffer-local-filename-completion-map' and `minibuffer-local-must-match-filename-map'
     ;; were introduced in Emacs 22, and they inherit from `minibuffer-local-completion' and
     ;; `minibuffer-local-must-match-map', respectively, so we need not do anything here for them.

     ;; `completion-list-mode-map': map for *Completions* buffer.
     (let ((map completion-list-mode-map))
       (define-key map [(control ?g)]     nil)
       (define-key map "q"                nil)
       (define-key map [(control insert)] nil)
       (define-key map [down]             nil)
       (define-key map [up]               nil)
       (define-key map [S-iso-lefttab]    nil)
       (define-key map [S-tab]            nil)
       (define-key map [left]             'previous-completion)
       (define-key map [(control ?i)]     nil)
       (define-key map [tab]              nil)
       (define-key map [right]            'next-completion)
       (define-key map [S-down-mouse-2]   nil)
       (define-key map [S-mouse-2]        nil)
       (define-key map [C-S-down-mouse-2] nil)
       (define-key map [C-S-mouse-2]      nil)
       (define-key map [C-down-mouse-2]   nil)
       (define-key map [C-mouse-2]        nil)
       (define-key map [C-M-return]       nil)
       (define-key map [C-M-down-mouse-2] nil)
       (define-key map [C-M-mouse-2]      nil)
       (define-key map [M-S-down-mouse-2] nil)
       (define-key map [M-S-mouse-2]      nil)
       (define-key map [M-down-mouse-2]   nil)
       (define-key map [M-mouse-2]        nil)
       (define-key map [C-down-mouse-3]   nil)
       (define-key map [M-down-mouse-3]   nil)
       (define-key map [M-mouse-3]        nil)
       (define-key map [M-S-down-mouse-3] nil)
       (define-key map [M-S-mouse-3]      nil)
       (define-key map [mouse-3]          nil)
       (define-key map [C-mouse-3]        nil)
       (define-key map [(control ?>)]     nil)
       (define-key map [(control meta ?>)] nil)
       (define-key map [(control ?\))]    nil)
       (define-key map [(control meta ?\))] nil)
       (define-key map [(control meta ?<)] nil)
       (define-key map [(control ?l)]      nil)
       )))

  ;; Update the bindings within the help string.
  (setq icicle-completion-help-string
        (substitute-command-keys
         "\\<minibuffer-local-completion-map> 
                     Icicles Minibuffer Completion
                     -----------------------------

Minibuffer input can be completed in several ways.
These are the main Icicles actions and their minibuffer key bindings:

 * Show this help.                           \\[icicle-completion-help]
     For help on individual completion candidates, see \"Show help on
     individual completion candidates\", below.

 * Abandon or commit your input.
     Abandon input (from minibuffer)         \\[icicle-abort-minibuffer-input]
     Abandon input (from anywhere)           \\[abort-recursive-edit]
     Commit input to Emacs                   RET
       Complete partial input, then commit   \\<minibuffer-local-must-match-map>\
\\[icicle-apropos-complete-and-exit]\\<minibuffer-local-completion-map>

 * Change the set of completion candidates.  Modify your input.
     Edit your input                         (just edit in minibuffer)
     Erase your input (clear minibuffer)     \\[icicle-erase-minibuffer-or-history-element]
     Erase input portion that doesn't match  \\[icicle-kill-failed-input]
     Retrieve previous completion inputs     \\[icicle-retrieve-previous-input], \
\\[icicle-retrieve-next-input]
     Match another regexp (chaining)         \\[icicle-narrow-candidates]
     Satisfy another predicate (chaining)    \\[icicle-narrow-candidates-with-predicate]
     Remove a candidate from set of matches  delete, S-mouse-2
     Yank text at cursor into minibuffer     \\[icicle-insert-string-at-point]
     Insert text (string) from a variable    \\[icicle-insert-string-from-variable]
     Insert key description (key completion) \\[icicle-dispatch-M-q]

 * Complete your current input in the minibuffer.
     Apropos (regexp) completion             \\[icicle-apropos-complete]
       Without displaying candidates         \\[icicle-prefix-complete-no-display]
       Complete and match another regexp     \\[icicle-apropos-complete-and-narrow]
     Prefix completion
       As much as possible                   \\[icicle-prefix-complete]
         Without displaying candidates       \\[icicle-prefix-complete-no-display]
       A word at a time                      \\[icicle-prefix-word-complete]
     Complete and commit (if required match) \\<minibuffer-local-must-match-map>\
\\[icicle-apropos-complete-and-exit]\\<minibuffer-local-completion-map>
     Complete search string using past input \\[icicle-apropos-complete]

 * Display/navigate completions for current input (in *Completions*).
     Show completion candidates
       Prefix completion                     \\[icicle-prefix-complete] (repeat)
       Apropos completion                    \\[icicle-apropos-complete]
     Move between minibuffer and list        \\<completion-list-mode-map>\
\\[icicle-insert-completion]
     Cycle among completion candidates       right, left, \
\\[icicle-move-to-next-completion], \\[icicle-move-to-previous-completion]
       Within a *Completions* column         down, up
     Choose a completion candidate           \\[choose-completion], \
\\[mouse-choose-completion]\\<minibuffer-local-completion-map>

 * Cycle among input candidates.
     Prefix-completion candidates            down, up
     Apropos-completion candidates           next, prior
     Minibuffer history items                \\[next-history-element], \
\\[previous-history-element]
     Completion history items                \\[icicle-retrieve-previous-input], \
\\[icicle-retrieve-next-input]

 * Show help on individual completion candidates.
     Current candidate                       C-M-RET, C-M-mouse-2
     Next, previous prefix-match candidate   C-M-down, C-M-up
     Next, previous apropos-match candidate  C-M-next, C-M-prior

 * Choose a previous input from the minibuffer history.
     Apropos-complete against history items  \\[icicle-history], \
\\[icicle-keep-only-past-inputs]
     Restrict candidates to history items    \\[icicle-keep-only-past-inputs]
     List history items first in Completions \\[icicle-toggle-alternative-sorting]
     Cycle among minibuffer history items    \\[next-history-element], \
\\[previous-history-element]
     Search among minibuffer history items   \
\\[next-matching-history-element], \\[previous-matching-history-element]

 * Multi-commands: Act on completion candidates.
   For alternative action, use `C-S-' instead of `C-' (except `C-!').
     Current candidate                       C-RET, C-o, C-mouse-2
     Next, previous prefix-match candidate   C-down, C-up
     Next, previous apropos-match candidate  C-next, C-prior
     All candidates at once
       Primary action                        C-!
       Alternative action                    C-S-insert
     Delete object named by candidate        S-delete
     Remove candidate from set of matches    delete, S-mouse-2
     Save candidate (add to those saved)     insert, M-S-mouse-2
     Object-action: apply a fn to candidate  M-RET

 * Search and replace (e.g. `C-`').
     Use alternative action keys (prefix `C-S-') to navigate.
     Replace all                             C-S-insert
     Redefine the replacement string         \\[icicle-dispatch-M-comma]
     Toggle literal replacement              \\[icicle-toggle-literal-replacement]
     Toggle replacement of whole search hit  \\[icicle-dispatch-C-comma]
     Toggle whole-word searching             \\[icicle-dispatch-M-q]
     Toggle escaping of special regexp chars \\[icicle-toggle-regexp-quote]
     Toggle removal of search highlighting   \\[icicle-dispatch-C-.]
     Toggle input highlighting at all hits   \\[icicle-dispatch-C-^]

 * Perform set operations on candidate sets.
     Remove candidate from current set       delete, S-mouse-2
     Add current candidate to saved set      insert, M-S-mouse-2
     Save candidates in current set to...
       `icicle-saved-completion-candidates'  \\[icicle-candidate-set-save]
       another variable                      \\[icicle-candidate-set-save-to-variable]
       a cache file                          \\[icicle-candidate-set-save-to-cache-file]
     Retrieve saved set from...
       `icicle-saved-completion-candidates'  \\[icicle-candidate-set-retrieve]
       another variable                      \\[icicle-candidate-set-retrieve-from-variable]
       a cache file                          \\[icicle-candidate-set-retrieve-from-cache-file]
     Add candidates in current set           \\[icicle-candidate-set-save-more]
     Save, add selected candidates (region)  \\[icicle-candidate-set-save-selected], \
\\[icicle-candidate-set-save-more-selected]
     Add new or update existing saved set
       \\[icicle-add/update-saved-completion-set]
     Remove a saved completion set
       \\[icicle-remove-saved-completion-set]
     Swap current and saved sets             \\[icicle-candidate-set-swap]
     Define current set by evaluating sexp   \\[icicle-candidate-set-define]
     Restrict candidates to history items    \\[icicle-keep-only-past-inputs]
     Set complement                          \\[icicle-candidate-set-complement]
     Set difference                          \\[icicle-candidate-set-difference]
     Set union                               \\[icicle-candidate-set-union]
     Set intersection                        \\[icicle-candidate-set-intersection]
     Set intersection using regexp           \\[icicle-narrow-candidates]
     Set intersection using predicate        \\[icicle-narrow-candidates-with-predicate]
       Save current predicate to a variable  \\[icicle-save-predicate-to-variable]
       Insert string variable as input       \\[icicle-insert-string-from-variable]

 * Toggle Icicles options on the fly.
     Change sort order                       \\[icicle-dispatch-C-comma]
     Change alternative sort order           \\[icicle-dispatch-M-comma]
     Swap alternative sort                   \\[icicle-toggle-alternative-sorting]
     Removal of duplicate candidates         \\[icicle-toggle-transforming]
     Case sensitivity                        \\[icicle-toggle-case-sensitivity]
     Highlighting of past inputs             \\[icicle-toggle-highlight-historical-candidates]
     Angle brackets for key names            \\[icicle-toggle-angle-brackets]
     Ignoring certain file extensions        \\[icicle-dispatch-C-.]
     Ignoring space prefix                   \\[icicle-dispatch-C-^]
     Incremental completion                  \\[icicle-toggle-incremental-completion]
     Escaping of special regexp chars        \\[icicle-toggle-regexp-quote]
     Whole-word searching                    \\[icicle-dispatch-M-q]
     Replacement of whole search hit         \\[icicle-dispatch-C-comma]
     Removal of `icicle-search' highlighting \\[icicle-dispatch-C-.]
     `icicle-search' all-current highlights  \\[icicle-dispatch-C-^]
     Using WYSIWYG display for *Completions* \\[icicle-toggle-WYSIWYG-Completions]
     Using `~' for your home directory       \\[icicle-toggle-~-for-home-dir]

Remember: You can always input any character (e.g. \\[icicle-prefix-complete]) that is bound
          to a command by preceding it with \\<global-map>\\[quoted-insert].

Though it has no direct connection with completion, you can use \
`\\<minibuffer-local-completion-map>\\[icicle-pp-eval-expression]'
in the minibuffer at any time to evaluate an Emacs-Lisp expression.
This calls `pp-eval-expression', which displays the result in the
echo area or a pop-up buffer, *Pp Eval Output*.

----------------------------------------------------------------------
 
Customize Icicles: `M-x icicle-customize-icicles-group'.
Summary of customizable options and faces (alphabetical order).

Some of the binary options can be toggled; their toggle keys are
mentioned here.

* `completion-ignore-case', `read-file-name-completion-ignore-case'
                                         - Case sensitivity? (`C-A')
* `completion-ignored-extensions'        - Ignored filenames (`C-.')
* `icicle-add-buffer-name-flag'          - Add candidate's buffer?
* `icicle-alternative-sort-function'     - Other sort (`M-,', `C-M-,')
* `icicle-bind-top-level-commands-flag'  - Bind Icicles commands?
* `icicle-buffer-*'                      - `icicle-buffer' options
* `icicle-candidate-width-factor'        - Width %, candidate columns
* `icicle-change-region-background-flag' - Change region color?
* `icicle-change-sort-order-completion-flag' - Control `C-,' behavior
* `icicle-color-themes'                  - For `icicle-color-theme'
* `icicle-complete-keys-self-insert-flag'- `S-TAB' for self-insert?
* `icicle-completing*-prompt-prefix'     - Completing prompt indicator
* `icicle-Completions-display-min-input-chars' - Remove *Completions*
                                           if fewer chars input
* `icicle-Completions-frame-at-right-flag'- *Completions* at right?
* `icicle-Completions-window-default-width' - Width of *Completions*
* `icicle-Completions-window-max-height'  - Max lines in *Completions*
* `icicle-cycle-into-subdirs-flag'       - Explore subdirectories?
* `icicle-cycling-respects-completion-mode-flag' - Completion mode
                                           affects cycling mode?
* `icicle-default-thing-insertion'       - Control behavior of \
\\<minibuffer-local-completion-map>\\[icicle-insert-string-at-point]
* `icicle-expand-input-to-common-match-flag'- Maximally expand input?
* `icicle-highlight-historical-candidates-flag'
                                         - Highlight past input?
* `icicle-highlight-input-initial-whitespace-flag'
                                         - Highlight input whitespace?
* `icicle-ignore-space-prefix-flag'      - See initial space? (`C-^')
* `icicle-incremental-completion-delay'  - Before update *Completions*
* `icicle-incremental-completion-flag'   - Icompletion? (`C-#')
* `icicle-incremental-completion-threshold'- # of candidates for delay
* `icicle-init-value-flag'               - Use default as init value?
* `icicle-input-string'                  - String inserted by `C-='
* `icicle-inter-candidates-min-spaces'   - Min spaces among candidates
* `icicle-key-descriptions-use-<>-flag'  - Key shown as \"<>\"? (`C-<')
* `icicle-keymaps-for-key-completion'    - `S-TAB' = key-complete maps
* `icicle-kmacro-ring-max'               - Icicles `kmacro-ring-max'
* `icicle-list-*-string'                 - Multi-completion join/end
* `icicle-list-nth-parts-join-string'    - Join split-candidate parts
* `icicle-mark-position-in-candidate'    - Mark position in cycling
* `icicle-minibuffer-setup-hook'         - Functions run after setup
* `icicle-modal-cycle-*-key'             - Keys for modal cycling
* `icicle-point-position-in-candidate'   - Cursor position in cycling
* `icicle-redefine-standard-commands-flag'- Redefine std commands?
* `icicle-regexp-quote-flag'             - Escape chars? (`C-`')
* `icicle-regexp-search-ring-max'        - `regexp-search-ring-max'
* `icicle-region-alist'                  - Alist of saved regions
* `icicle-region-auto-open-files-flag'   - Open saved-region files?
* `icicle-region-background'             - Background for region
* `icicle-region-alist'                  - List of regions
* `icicle-regions-name-length-max'       - # chars to name a region
* `icicle-reminder-prompt-flag'          - Show reminder in prompt?
* `icicle-require-match-flag'            - Override REQUIRE-MATCH?
* `icicle-saved-completion-sets'         - Completion sets for `\\[icicle-candidate-set-retrieve]'
* `icicle-search-cleanup-flag'           - Remove search highlighting?
                                           (`C-.')
* `icicle-search-context-match-predicate'- Search-context predicate
* `icicle-search-highlight-all-current-flag'- In each hit (`C-^')
* `icicle-search-highlight-context-levels-flag' -
                                           Highlight match subgroups?
* `icicle-search-highlight-threshold'    - # hits to highlight at once
* `icicle-search-hook'                   - Functions run by `C-c `'
* `icicle-search-replace-literally-flag' - Replace text literally?
* `icicle-search-replace-whole-candidate-flag' - Replace input match
                                           or whole search hit?(`C-,')
* `icicle-search-ring-max'               - Icicles `search-ring-max'
* `icicle-search-whole-word-flag'        - Find whole words? (`M-q')
* `icicle-show-Completions-help-flag'    - Show *Completions* help?
* `icicle-show-Completions-initially-flag'- Show *Completions* first?
* `icicle-sort-function'                 - Sort candidates (`C-,')
* `icicle-sort-functions-alist'          - Functions for sorting
* `icicle-special-candidate-regexp'      - For highlighted candidates
* `icicle-TAB-shows-candidates-flag'     - 1st `TAB' shows candidates?
* `icicle-thing-at-point-functions'      - Functions to yank things
* `icicle-top-level-when-sole-completion-flag' -
                                           Exit if single completion? 
* `icicle-touche-pas-aux-menus-flag'     - Add to standard menus?
* `icicle-transform-function'            - Remove duplicates (`C-$')
* `icicle-update-input-hook'             - Fns run when input changes
* `icicle-use-~-for-home-dir-flag'       - Use `~' for $HOME (`M-~')
* `icicle-use-candidates-only-once-flag' - Remove used candidate?
* `icicle-word-completion-key'           - Key for word completion
* `icicle-WYSIWYG-Completions-flag'      - WYSIWYG for *Completions*
* `icicle-yank-function'                 - Yank function to use

Faces that highlight input in minibuffer.

* `icicle-complete-input'               - Input when it is complete
* `icicle-match-highlight-minibuffer'   - Matched part of input
* `icicle-whitespace-highlight'         - Initial whitespace in input

Faces that highlight candidates in buffer *Completions*.

* `icicle-common-match-highlight-Completions' - Max common substring
* `icicle-current-candidate-highlight'  - Highlight cycle candidate
* `icicle-historical-candidate'         - Highlight candidates used
* `icicle-match-highlight-Completions'  - Matched part of input

Faces that highlight for command `icicle-search'.

* `icicle-search-current-input'         - What input matches
* `icicle-search-main-regexp-current'   - Current match of 1st regexp
* `icicle-search-main-regexp-others'    - Other matches of 1st regexp

----------------------------------------------------------------------
 
Some top-level Icicles commands (alphabetical order, with exceptions).
Some are bound in Icicle mode.  Bind the others to keys you like.
See recommended bindings in `icicles.el'.
Multi-commands are indicated by `+': They act any number of times.
You can tell a multi-command when you execute it by the fact that the
input prompt is prefixed by `+'.

+ `clear-option' (alias)               - Set binary option to nil
+ `icicle-add-buffer-candidate'        - To always-candidate buffer
+ `icicle-remove-buffer-candidate'     -   From same
  `icicle-add-buffer-config'           - To `icicle-buffer-configs'
+ `icicle-remove-buffer-config'        -   From same
  `icicle-add-region'                  - To `icicle-region-alist'
+ `icicle-remove-region'               -   From same
  `icicle-add/update-saved-completion-set' - To
                                        `icicle-saved-completion-sets'
+ `icicle-remove-saved-completion-set' -   From same
  `icicle-apropos'                     - `apropos', but shows matches
  `icicle-apropos-command'             - Enhanced `apropos-command'
  `icicle-apropos-variable'            - Enhanced `apropos-variable'
  `icicle-apropos-zippy'               - Show matching Zippy quotes
+ `icicle-bookmark'                    - Jump to bookmark
+ `icicle-buffer'(`-other-window')     - Switch to buffer (`C-x b')
+ `icicle-buffer-config'               - Pick `icicle-buffer' options
+ `icicle-buffer-list'                 - Choose a list of buffer names
  `icicle-change-alternative-sort-order' - Choose an alternative sort
  `icicle-change-sort-order'           - Choose a sort order
+ `icicle-color-theme'                 - Change color theme
+ `icicle-comint-command'              - Reuse command (`C-c TAB')
+ `icicle-comint-search'               - Reuse command (`C-c `')
+ `icicle-compilation-search'          - Show hits (`C-c `')
+ `icicle-complete-keys'               - Complete keys (`S-TAB')
  `icicle-complete-thesaurus-entry'    - Complete word (`C-/')
  `icicle-completion-help'             - Display this help
+ `icicle-customize-face'              - Multi-`customize-face'
  `icicle-customize-icicles-group'     - Customize options and faces
+ `icicle-delete-file'                 - Delete file/directory
+ `icicle-delete-windows'              - Delete windows (`C-u C-x 0')
+ `icicle-doc'                         - Show doc for fn, var, or face
+ `icicle-execute-extended-command'    - Execute Emacs command (`M-x')
+ `icicle-execute-named-keyboard-macro' - Execute named keyboard macro
+ `icicle-find-file'(`-other-window')  - Visit file/dir (`C-x C-f')
+ `icicle-find-tag'(`-other-window')   - Visit symbol with tag (`M-.')
+ `icicle-font'                        - Change font of frame
+ `icicle-frame-bg'                    - Change background of frame
+ `icicle-frame-fg'                    - Change foreground of frame
+ `icicle-fundoc'                      - Show function description
+ `icicle-goto-global-marker'          - Go to a global marker
+ `icicle-goto-marker'                 - Go to a marker in this buffer
+ `icicle-imenu'                       - Navigate among Imenu entries
+ `icicle-Info-goto-node'              - Multi-cmd `Info-goto-node' 
+ `icicle-Info-index'                  - Multi-command `Info-index'
+ `icicle-insert-kill'                 - Like `yank', without rotating
+ `icicle-insert-thesaurus-entry'      - Insert thesaurus entry
+ `icicle-kill-buffer'                 - Kill buffer (`C-x k')
+ `icicle-kmacro'                      - Execute keyboard macro (`f5')
+ `icicle-locate-file'(`-other-window') - Visit file in a directory
+ `icicle-map'                         - Apply function to alist items
  `icy-mode' or `icicle-mode'          - Toggle Icicle mode
+ `icicle-occur'                       - Enhanced `occur' (`C-c '')
+ `icicle-other-window-or-frame'       - Other window/frame (`C-x o')
+ `icicle-plist'                       - Show symbols, property lists
+ `icicle-recent-file'(`-other-window') - Open recently used file
+ `icicle-reset-option-to-nil'         - Set binary option to nil
  `icicle-save-string-to-variable'     - Save text for use with \
`\\[icicle-insert-string-from-variable]'
+ `icicle-search'                      - Search (`C-c `')
+ `icicle-search-keywords'             - Search for keywords (`C-c ^')
+ `icicle-search-region'               - Search multiple regions
+ `icicle-search-text-property'        - Search for face... (`C-c \"')
+ `icicle-search-word'                 - Search for whole word
+ `icicle-select-frame'                - Select a frame by name
+ `icicle-select-region'               - Select a region
+ `icicle-select-window'               - Select window by buffer name
  `icicle-send-bug-report'             - Send Icicles bug report
+ `icicle-set-option-to-t'             - Set binary option to t
  `icicle-toggle-~-for-home-dir'       - Toggle using `~' for $HOME
  `icicle-toggle-alternative-sorting'  - Swap alternative sort
  `icicle-toggle-angle-brackets'       - Toggle angle brackets
  `icicle-toggle-case-sensitivity'     - Toggle case sensitivity
  `icicle-toggle-highlight-all-current' - Toggle max search highlight
  `icicle-toggle-highlight-historical-candidates'
                                       - Toggle past-input highlight
  `icicle-toggle-ignored-extensions'   - Toggle ignored files
  `icicle-toggle-ignored-space-prefix' - Toggle ignoring space
  `icicle-toggle-incremental-completion' - Toggle icompletion
+ `icicle-toggle-option'               - Toggle binary user option
  `icicle-toggle-regexp-quote'         - Toggle regexp escaping
  `icicle-toggle-search-cleanup'       - Toggle highlight removal
  `icicle-toggle-search-whole-word'    - Toggle whole-word searching
  `icicle-toggle-sorting'              - Toggle sorting
  `icicle-toggle-transforming'         - Toggle duplicate removal
  `icicle-toggle-WYSIWYG-Completions'  - Toggle WYSIWYG *Completions*
+ `icicle-vardoc'                      - Show variable description
  `icicle-yank-insert'                 - `yank' + completion (`C-y')
+ `toggle' (alias)                     - Toggle binary user option

----------------------------------------------------------------------

Send an Icicles bug report: `\\[icicle-send-bug-report]'.

----------------------------------------------------------------------
 
These are all of the minibuffer bindings during completion:

\\{minibuffer-local-completion-map}---------------------------------------\
-------------------------------
"))

  (setq icicle-prompt-suffix
        (substitute-command-keys
         " (\\<minibuffer-local-completion-map>\\[icicle-apropos-complete], \
\\[icicle-prefix-complete]: list, \\[icicle-completion-help]: help) "))
  (when (and (interactive-p) turn-on-p)
    (message (substitute-command-keys
              "Use `\\<minibuffer-local-completion-map>\
\\[icicle-completion-help]' in minibuffer for help."))))

(defun icicle-remap (old new map &optional oldmap)
  "Bind command NEW in MAP to all keys currently bound to OLD.
If command remapping is available, use that.  Otherwise, bind NEW to
whatever OLD is bound to in MAP, or in OLDMAP, if provided."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap old) new) ; Ignore OLDMAP for Emacs 22.
    (substitute-key-definition old new map oldmap)))

(defun icicle-unmap (command map current)
  "In MAP, unbind any keys that are bound to COMMAND.
If command remapping is available, remap COMMAND to nil in MAP,
unbinding it.
Otherwise, bind COMMAND to whatever CURRENT is bound to in MAP."
  (if (fboundp 'command-remapping)
      (define-key map (vector 'remap command) nil)
    (substitute-key-definition current command map)))

(defun icicle-rebind-global (old new map)
  "Bind command NEW in MAP to all keys currently bound globally to OLD."
  (substitute-key-definition old new map (current-global-map)))

(defun icicle-bind-completion-keys (map)
  "Bind keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))
    (define-key map [menu-bar minibuf quit] ; Replace `keyboard-escape-quit'
      '(menu-item "Quit" icicle-abort-minibuffer-input
        :help "Cancel minibuffer input or recursive edit"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET"))
    (define-key map [menu-bar minibuf completion-help]
      '(menu-item "Help" icicle-completion-help
        :help "Display help on minibuffer input and completion")))
  (define-key map [menu-bar minibuf separator-insert] '("--"))
  (define-key map [menu-bar minibuf icicle-retrieve-next-input]
    '(menu-item "Restore Next Completion Input" icicle-retrieve-next-input
      :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                       'icicle-previous-raw-file-name-inputs
                                     'icicle-previous-raw-non-file-name-inputs)))
      :help "Cycle forward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
  (define-key map [menu-bar minibuf icicle-retrieve-previous-input]
    '(menu-item "Restore Previous Completion Input" icicle-retrieve-previous-input
      :enable (consp (symbol-value (if (icicle-file-name-input-p)
                                       'icicle-previous-raw-file-name-inputs
                                     'icicle-previous-raw-non-file-name-inputs)))
      :help "Cycle backward to insert a previous completion input in the minibuffer (`C-u': \
complete)"))
;; $$  (define-key map [menu-bar minibuf icicle-insert-history-element]
;;     '(menu-item "Insert Previously Entered Input" icicle-insert-history-element
;;       :enable (consp (symbol-value minibuffer-history-variable))
;;       :help "Use completion to insert a previously entered input in the minibuffer."))
  (define-key map [menu-bar minibuf separator-last] '("--"))
  (define-key map [menu-bar minibuf ?\?] nil)
  (define-key map [menu-bar minibuf space] nil)
  (define-key map [menu-bar minibuf tab] nil)
  (define-key map [menu-bar minibuf action-all]
    '(menu-item "Act On All Candidates - Careful!" icicle-all-candidates-action
      :help "Apply the command action to *each* of the possible completion candidates"
      :enable icicle-candidate-action-fn))
  (define-key map [menu-bar minibuf separator-actions] '("--"))
  (define-key map [menu-bar minibuf set-define]
    '(menu-item "Define Candidates by Lisp Sexp" icicle-candidate-set-define
      :help "Define the set of current completion candidates by evaluating a sexp"))
  (define-key map [menu-bar minibuf icicle-keep-only-past-inputs]
    '(menu-item "Keep Only Previously Entered" icicle-keep-only-past-inputs
      :enable (and icicle-completion-candidates (consp (symbol-value minibuffer-history-variable)))
      :help "Removed candidates that you have not previously chosen and entered"))
  (define-key map [menu-bar minibuf set-union]
    '(menu-item "Add (Union) Saved Candidate Set" icicle-candidate-set-union
      :enable icicle-saved-completion-candidates
      :help "Set union between the current and saved completion candidates"))
  (define-key map [menu-bar minibuf set-difference]
    '(menu-item "Subtract Saved Candidate Set" icicle-candidate-set-difference
      :enable icicle-saved-completion-candidates
      :help "Set difference between the current and saved completion candidates"))
  (define-key map [menu-bar minibuf set-intersection]
    '(menu-item "Intersect Saved Candidate Set" icicle-candidate-set-intersection
      :enable icicle-saved-completion-candidates
      :help "Set intersection between the current and saved candidates"))
  (define-key map [menu-bar minibuf icicle-save-predicate-to-variable]
    '(menu-item "Save Predicate to Variable" icicle-save-predicate-to-variable
      :help "Save the current completion predicate to a variable"))
  (define-key map [menu-bar minibuf icicle-narrow-candidates-with-predicate]
    '(menu-item "Satisfy Also Predicate..." icicle-narrow-candidates-with-predicate
      :help "Match another input pattern (narrow completions set)"
      :enable icicle-completion-candidates))
  (define-key map [menu-bar minibuf icicle-narrow-candidates]
    '(menu-item "Match Also Regexp..." icicle-narrow-candidates
      :enable icicle-completion-candidates
      :help "Match another input pattern (narrow completions set)"))
  (define-key map [menu-bar minibuf set-complement]
    '(menu-item "Complement Candidates" icicle-candidate-set-complement
      :help "Complement the set of current completion candidates"))
  (define-key map [menu-bar minibuf separator-set1] '("--"))
  (define-key map [menu-bar minibuf set-swap]
    '(menu-item "Swap Saved and Current Sets" icicle-candidate-set-swap
      :enable icicle-saved-completion-candidates
      :help "Swap the saved and current sets of completion candidates"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more-selected]
    '(menu-item "Add Selected (Region) Candidates"
      icicle-candidate-set-save-more-selected
      :help "Add the candidates in the region to the saved candidates"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-selected]
    '(menu-item "Save Selected (Region) Candidates"
      icicle-candidate-set-save-selected
      :help "Save the candidates in the region, for later recall"))
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more]
    '(menu-item "Add Candidates in Current Set"
      icicle-candidate-set-save-more
      :help "Add current completion candidates to saved candidates set"))
  (define-key map [menu-bar minibuf set-retrieve-from-cache-file]
    '(menu-item "    from Cache File..."
      icicle-candidate-set-retrieve-from-cache-file
      :help "Retrieve saved completion candidates from a cache file, making them current"))
  (define-key map [menu-bar minibuf set-retrieve-from-variable]
    '(menu-item
      "    from Variable..." icicle-candidate-set-retrieve-from-variable
      :help "Retrieve saved completion candidates from variable, making them current"))
  (define-key map [menu-bar minibuf set-retrieve]
    '(menu-item "Retrieve Saved Candidates" icicle-candidate-set-retrieve
      :enable icicle-saved-completion-candidates
      :help "Retrieve the saved set of completion candidates, making it current"))
  (define-key map [menu-bar minibuf set-save-to-cache-file]
    '(menu-item "    to Cache File..." icicle-candidate-set-save-to-cache-file
      :help "Save current completion candidates to a cache file, for later recall"))
  (define-key map [menu-bar minibuf set-save-to-variable]
    '(menu-item "    to Variable..." icicle-candidate-set-save-to-variable
      :help "Save current completion candidates to a variable, for later recall"))
  (define-key map [menu-bar minibuf set-save]
    '(menu-item "Save Candidates" icicle-candidate-set-save
      :help "Save the set of current completion candidates, for later recall"))
  (define-key map [menu-bar minibuf separator-set2] '("--"))
  (define-key map [menu-bar minibuf word-complete]
    '(menu-item "Word-Complete" icicle-prefix-word-complete
      :help "Complete at most one word of prefix"))
  (define-key map [menu-bar minibuf prefix-complete]
    '(menu-item "Prefix-Complete" icicle-prefix-complete
      :help "Complete prefix as far as possible"))
  (define-key map [menu-bar minibuf apropos-complete]
    '(menu-item "Apropos-Complete" icicle-apropos-complete :keys "S-TAB"
      :help "Complete regular expression as far as possible and list completions"))

  ;; Remap some commands for completion.
  (icicle-remap 'self-insert-command           'icicle-self-insert map (current-global-map))
  (icicle-remap 'universal-argument            'icicle-universal-argument ; `C-u'
                map (current-global-map))
  (icicle-remap 'negative-argument             'icicle-negative-argument ; `M--'
                map (current-global-map))
  (icicle-remap 'digit-argument                'icicle-digit-argument ; `C-9'
                map (current-global-map))
  (icicle-remap 'backward-delete-char-untabify 'icicle-backward-delete-char-untabify ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-backward-char          'icicle-delete-backward-char ; `DEL'
                map (current-global-map))
  (icicle-remap 'delete-char                   'icicle-delete-char ; `C-d', `deletechar'
                map (current-global-map))
  (icicle-remap 'backward-kill-word            'icicle-backward-kill-word ; `M-DEL'
                map (current-global-map))
  (icicle-remap 'kill-word                     'icicle-kill-word ; `M-d'
                map (current-global-map))
  (icicle-remap 'backward-kill-sexp            'icicle-backward-kill-sexp ; `C-M-backspace'
                map (current-global-map))
  (icicle-remap 'kill-sexp                     'icicle-kill-sexp ; `C-M-k', `C-M-delete'
                map (current-global-map))
  (icicle-remap 'backward-kill-sentence        'icicle-backward-kill-sentence ; `C-x DEL'
                map (current-global-map))
  (icicle-remap 'backward-kill-paragraph       'icicle-backward-kill-paragraph ; `C-backspace'
                map (current-global-map))
  (icicle-remap 'kill-paragraph                'icicle-kill-paragraph ; `C-delete'
                map (current-global-map))
  (icicle-remap 'kill-line                     'icicle-kill-line ; `C-k', `deleteline'
                map (current-global-map))  
  (icicle-remap 'reposition-window             'icicle-kill-failed-input ; `C-M-l'
                map (current-global-map))  
  (icicle-remap 'transpose-chars               'icicle-transpose-chars ; `C-t'
                map (current-global-map))
  (icicle-remap 'transpose-words               'icicle-transpose-words ; `M-t'
                map (current-global-map))
  (icicle-remap 'transpose-sexps               'icicle-transpose-sexps ; `C-M-t'
                map (current-global-map))
  (icicle-remap 'yank-pop                      'icicle-yank-pop ; `M-y', `M-insert'
                map (current-global-map))

  ;; Rebind some global bindings for the completion maps.
  (unless icicle-cycling-respects-completion-mode-flag
    (icicle-rebind-global 'previous-line  'icicle-previous-prefix-candidate map) ; `down', `C-n'
    (icicle-rebind-global 'next-line      'icicle-next-prefix-candidate map)) ; `up', `C-p'
  (icicle-rebind-global 'scroll-down      'icicle-previous-apropos-candidate map) ; `prior', `M-v'
  (icicle-rebind-global 'scroll-up        'icicle-next-apropos-candidate map) ; `next', `C-v'
  (icicle-rebind-global 'backward-paragraph
                        'icicle-previous-prefix-candidate-action map) ; `C-up', `M-{'
  (icicle-rebind-global 'forward-paragraph
                        'icicle-next-prefix-candidate-action map) ; `C-down', `M-}'
  (icicle-rebind-global 'scroll-right
                        'icicle-previous-apropos-candidate-action map) ; `C-prior', `C-x >'
  (icicle-rebind-global 'scroll-left
                        'icicle-next-apropos-candidate-action map) ; `C-next', `C-x <'

  ;; Bind some additional keys.
  (define-key map icicle-word-completion-key 'icicle-prefix-word-complete)
  (define-key map [(control meta up)]        'icicle-help-on-previous-prefix-candidate) ; `C-M-up'
  (define-key map [(control meta down)]      'icicle-help-on-next-prefix-candidate) ; `C-M-down'
  (define-key map [(control meta prior)]  'icicle-help-on-previous-apropos-candidate) ; `C-M-prior'
  (define-key map [(control meta next)]      'icicle-help-on-next-apropos-candidate) ; `C-M-next'
  (define-key map [(control help)]           'icicle-help-on-candidate) ; `C-help'
  (define-key map [(control meta help)]      'icicle-help-on-candidate) ; `C-M-help'
  (define-key map [(control f1)]             'icicle-help-on-candidate) ; `C-f1'
  (define-key map [(control meta f1)]        'icicle-help-on-candidate) ; `C-M-f1'
  (define-key map [(control meta return)]    'icicle-help-on-candidate) ; `C-M-RET'
  (define-key map [(meta return)]            'icicle-candidate-read-fn-invoke) ; `M-RET'
  (define-key map [(control shift return)]   'icicle-candidate-alt-action) ; `C-S-RET'
  (define-key map [(control shift up)]     'icicle-previous-prefix-candidate-alt-action) ; `C-S-up'
  (define-key map [(control shift down)]     'icicle-next-prefix-candidate-alt-action) ; `C-S-down'
  (define-key map [(control shift prior)]'icicle-previous-apropos-candidate-alt-action);`C-S-prior'
  (define-key map [(control shift next)]    'icicle-next-apropos-candidate-alt-action) ; `C-S-next'
  (define-key map [delete]                   'icicle-remove-candidate) ; `delete'
  (define-key map [(shift delete)]           'icicle-delete-candidate-object) ; `S-delete'
  (define-key map [(control ?w)]             'icicle-kill-region)
  (define-key map [(control return)]         'icicle-candidate-action) ; `C-RET'
  (define-key map [(control ?o)]             'icicle-candidate-action) ; `C-o'
  (define-key map [(control ?!)]             'icicle-all-candidates-action) ; `C-!'
  (define-key map [(control shift insert)]   'icicle-all-candidates-alt-action) ; `C-S-insert'
  (define-key map [S-iso-lefttab]            'icicle-apropos-complete) ; `S-TAB'
  (define-key map [S-tab]                    'icicle-apropos-complete) ; `S-TAB'
  (define-key map [C-M-S-iso-lefttab]        'icicle-apropos-complete-no-display) ; `C-M-S-TAB'
  (define-key map [C-M-S-tab]                'icicle-apropos-complete-no-display) ; `C-M-S-TAB'
  (define-key map [(control ?i)]             'icicle-prefix-complete) ; `TAB'
  (define-key map [tab]                      'icicle-prefix-complete) ; `TAB'
  (define-key map [(control meta ?/)]        'icicle-prefix-complete) ; `C-M-/', for `dabbrev.el'.
  (define-key map [(control meta tab)]       'icicle-prefix-complete-no-display) ; `C-M-TAB'
  (define-key map [(meta ?h)]                'icicle-history) ; `M-h'
  (define-key map [(meta pause)]             'icicle-keep-only-past-inputs) ; `M-pause'
  (define-key map [(control pause)]     'icicle-toggle-highlight-historical-candidates) ; `C-pause'
  (define-key map [(control insert)]         'icicle-switch-to-Completions-buf) ; `C-insert'
  (define-key map [insert]                   'icicle-save-candidate) ; `insert'
  ;; `minibuffer-completion-help' got wiped out by remap for self-insert.
  (define-key map "?"                        'icicle-self-insert) ; `?'
  (define-key map " "                        'icicle-self-insert) ; " "
  (define-key map [M-S-backspace]            'icicle-erase-minibuffer) ; `M-S-backspace'
  (define-key map [M-S-delete]               'icicle-erase-minibuffer) ; `M-S-delete'
  (define-key map [(meta ?o)]                'icicle-insert-history-element) ; `M-o'
  ;; Replaces `kill-sentence':
  (define-key map [(meta ?k)]                'icicle-erase-minibuffer-or-history-element) ; `M-k'
  (define-key map [(meta ?q)]                'icicle-dispatch-M-q) ; `M-q'
  (define-key map [(control ?g)]             'icicle-abort-minibuffer-input) ; `C-g'
  (define-key map [(control ?l)]             'icicle-retrieve-previous-input) ; `C-l'
  (define-key map [(control shift ?l)]       'icicle-retrieve-next-input) ; `C-L' (`C-S-l')
  (define-key map [(meta ?$)]                'icicle-candidate-set-truncate) ; `M-$'
  (define-key map [(control ?~)]             'icicle-candidate-set-complement) ; `C-~'
  (define-key map [(control ?-)]             'icicle-candidate-set-difference) ; `C--'
  (define-key map [(control ?+)]             'icicle-candidate-set-union) ; `C-+'
  (define-key map [(control ?*)]             'icicle-candidate-set-intersection) ; `C-*'
  (define-key map [(control ?>)]             'icicle-candidate-set-save-more) ; `C->'
  (define-key map [(control meta ?>)]        'icicle-candidate-set-save) ; `C-M->'
  (define-key map [(control ?\))]            'icicle-candidate-set-save-more-selected) ; `C-)'
  (define-key map [(control meta ?\))]       'icicle-candidate-set-save-selected) ; `C-M-)'
  (define-key map [(control meta ?<)]        'icicle-candidate-set-retrieve) ; `C-M-<'
  (define-key map [(control meta ?})]        'icicle-candidate-set-save-to-variable) ; `C-M-}'
  (define-key map [(control meta ?{)]       'icicle-candidate-set-retrieve-from-variable) ; `C-M-{'
  (define-key map [(control ?})]             'icicle-candidate-set-save-to-cache-file) ; `C-}'
  (define-key map [(control ?{)]            'icicle-candidate-set-retrieve-from-cache-file) ; `C-{'
  (define-key map [(control ?%)]             'icicle-candidate-set-swap) ; `C-%'
  (define-key map [(control ?:)]             'icicle-candidate-set-define) ; `C-:'
  (define-key map [(control ?=)]             'icicle-insert-string-from-variable) ; `C-='
  (define-key map [(control ?,)]             'icicle-dispatch-C-comma) ; `C-,'
  (define-key map [(control ?`)]             'icicle-toggle-regexp-quote) ; `C-`'
  (define-key map [(control meta ?`)]        'icicle-toggle-literal-replacement) ; `C-M-`'
  (define-key map [(control ?<)]             'icicle-toggle-angle-brackets) ; `C-<'
  (define-key map [(control ?$)]             'icicle-toggle-transforming) ; `C-$'
  (define-key map [(control ??)]             'icicle-completion-help) ; `C-?'
  (define-key map [(control ?.)]             'icicle-dispatch-C-.) ; `C-.'
  (define-key map [(control ?#)]             'icicle-toggle-incremental-completion) ; `C-#'
  (define-key map [(control ?^)]             'icicle-dispatch-C-^) ; `C-^'
  (define-key map [(control shift ?a)]       'icicle-toggle-case-sensitivity) ; `C-S-a' (`C-A')
  (define-key map [(meta ?~)]                'icicle-toggle-~-for-home-dir) ; `M-~'
  (define-key map [(meta ?,)]                'icicle-dispatch-M-comma) ; `M-,'
  (define-key map [(control meta ?,)]        'icicle-toggle-alternative-sorting) ; `C-M-,'
  (define-key map [(meta ?.)]                'icicle-insert-string-at-point) ; `M-.'
  (define-key map [(meta ?*)]                'icicle-narrow-candidates) ; `M-*'
  (define-key map [(meta ?&)]                'icicle-narrow-candidates-with-predicate) ; `M-&'
  (define-key map [(control meta ?&)]        'icicle-save-predicate-to-variable) ; `C-M-&'
  (define-key map [(shift ?\ )]              'icicle-apropos-complete-and-narrow) ; `S-SPC'
  (define-key map [(meta ?:)]                'icicle-pp-eval-expression)) ; `M-:'

(defun icicle-restore-completion-keys (map)
  "Restore standard keys for minibuffer completion map MAP.
MAP is `minibuffer-local-completion-map',
`minibuffer-local-filename-completion-map', or
`minibuffer-local-must-match-map'."

  ;; Menu-bar Minibuf menu.
  ;; In Emacs 22+, local is parent of local-completion
  (unless (eq minibuffer-local-map (keymap-parent minibuffer-local-completion-map))   
    (define-key map [menu-bar minibuf quit]
      '(menu-item "Quit" keyboard-escape-quit :help "Abort input and exit minibuffer"))
    (define-key map [menu-bar minibuf return]
      '(menu-item "Enter" exit-minibuffer
        :help "Terminate input and exit minibuffer" :keys "RET")))
  (define-key map [menu-bar minibuf completion-help]                         nil)
  (define-key map [menu-bar minibuf separator-insert]                        nil)
  (define-key map [menu-bar minibuf icicle-retrieve-next-input]              nil)
  (define-key map [menu-bar minibuf icicle-retrieve-previous-input]          nil)
  (define-key map [menu-bar minibuf icicle-insert-history-element]           nil)
  (define-key map [menu-bar minibuf separator-last]                          nil)
  (define-key map [menu-bar minibuf action-all]                              nil)
  (define-key map [menu-bar minibuf separator-actions]                       nil)
  (define-key map [menu-bar minibuf set-define]                              nil)
  (define-key map [menu-bar minibuf icicle-keep-only-past-inputs]            nil)
  (define-key map [menu-bar minibuf set-union]                               nil)
  (define-key map [menu-bar minibuf set-difference]                          nil)
  (define-key map [menu-bar minibuf set-intersection]                        nil)
  (define-key map [menu-bar minibuf icicle-narrow-candidates-with-predicate] nil)
  (define-key map [menu-bar minibuf icicle-narrow-candidates]                nil)
  (define-key map [menu-bar minibuf set-complement]                          nil)
  (define-key map [menu-bar minibuf separator-set1]                          nil)
  (define-key map [menu-bar minibuf set-swap]                                nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more-selected] nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-selected]      nil)
  (define-key map [menu-bar minibuf icicle-candidate-set-save-more]          nil)
  (define-key map [menu-bar minibuf set-retrieve-from-cache-file]            nil)
  (define-key map [menu-bar minibuf set-retrieve-from-variable]              nil)
  (define-key map [menu-bar minibuf set-retrieve]                            nil)
  (define-key map [menu-bar minibuf set-save-to-cache-file]                  nil)
  (define-key map [menu-bar minibuf set-save-to-variable]                    nil)
  (define-key map [menu-bar minibuf set-save]                                nil)
  (define-key map [menu-bar minibuf separator-set2]                          nil)
  (define-key map [menu-bar minibuf word-complete]                           nil)
  (define-key map [menu-bar minibuf prefix-complete]                         nil)
  (define-key map [menu-bar minibuf apropos-complete]                        nil)
  (define-key map [menu-bar minibuf ?\?]
    '(menu-item "List Completions" minibuffer-completion-help
      :help "Display all possible completions"))
  (define-key map [menu-bar minibuf space]
    '(menu-item "Complete Word" minibuffer-complete-word :help "Complete at most one word"))
  (define-key map [menu-bar minibuf tab]
    '(menu-item "Complete" minibuffer-complete :help "Complete as far as possible"))

  ;; Unmap commands that were bound for completion.
  (icicle-unmap 'self-insert-command           map 'icicle-self-insert)
  (icicle-unmap 'universal-argument            map 'icicle-universal-argument)
  (icicle-unmap 'negative-argument             map 'icicle-negative-argument)
  (icicle-unmap 'digit-argument                map 'icicle-digit-argument)
  (icicle-unmap 'backward-delete-char-untabify map 'icicle-backward-delete-char-untabify)
  (icicle-unmap 'delete-backward-char          map 'icicle-delete-backward-char)
  (icicle-unmap 'delete-char                   map 'icicle-delete-char)
  (icicle-unmap 'backward-kill-word            map 'icicle-backward-kill-word)
  (icicle-unmap 'kill-word                     map 'icicle-kill-word)
  (icicle-unmap 'backward-kill-sexp            map 'icicle-backward-kill-sexp)
  (icicle-unmap 'kill-sexp                     map 'icicle-kill-sexp)
  (icicle-unmap 'backward-kill-sentence        map 'icicle-backward-kill-sentence)
  (icicle-unmap 'backward-kill-paragraph       map 'icicle-backward-kill-paragraph)
  (icicle-unmap 'kill-paragraph                map 'icicle-kill-paragraph)
  (icicle-unmap 'kill-line                     map 'icicle-kill-line)
  (icicle-unmap 'reposition-window             map 'icicle-kill-failed-input)
  (icicle-unmap 'transpose-chars               map 'icicle-transpose-chars)
  (icicle-unmap 'transpose-words               map 'icicle-transpose-words)
  (icicle-unmap 'transpose-sexps               map 'icicle-transpose-sexps)
  (icicle-unmap 'yank-pop                      map 'icicle-yank-pop)

  ;; Restore commands that were bound for completion.
  (substitute-key-definition 'icicle-previous-prefix-candidate         'previous-line      map)
  (substitute-key-definition 'icicle-next-prefix-candidate             'next-line          map)
  (substitute-key-definition 'icicle-previous-apropos-candidate        'scroll-down        map)
  (substitute-key-definition 'icicle-next-apropos-candidate            'scroll-up          map)
  (substitute-key-definition 'icicle-previous-prefix-candidate-action  'backward-paragraph map)
  (substitute-key-definition 'icicle-next-prefix-candidate-action      'forward-paragraph  map)
  (substitute-key-definition 'icicle-previous-apropos-candidate-action 'scroll-right       map)
  (substitute-key-definition 'icicle-next-apropos-candidate-action     'scroll-left        map)

  ;; Restore additional bindings.
  (define-key map icicle-word-completion-key nil) ; Do first, so can be rebound, as needed.
  (define-key map [(control meta up)]        nil)
  (define-key map [(control meta down)]      nil)
  (define-key map [(control meta prior)]     nil)
  (define-key map [(control meta next)]      nil)
  (define-key map [(control help)]           nil)
  (define-key map [(control meta help)]      nil)
  (define-key map [(control f1)]             nil)
  (define-key map [(control meta f1)]        nil)
  (define-key map [(control meta return)]    nil)
  (define-key map [(meta return)]            nil)
  (define-key map [(control shift return)]   nil)
  (define-key map [(control shift up)]       nil)
  (define-key map [(control shift down)]     nil)
  (define-key map [(control shift prior)]    nil)
  (define-key map [(control shift next)]     nil)
  (define-key map [delete]                   nil)
  (define-key map [(shift delete)]           nil)
  (define-key map [(control ?w)]             nil)
  (define-key map [(control return)]         nil)
  (define-key map [(control ?o)]             nil)
  (define-key map [(control ?!)]             nil)
  (define-key map [(control shift insert)]   nil)
  (define-key map [S-iso-lefttab]            nil)
  (define-key map [S-tab]                    nil)
  (define-key map [C-M-S-iso-lefttab]        nil)
  (define-key map [C-M-S-tab]                nil)
  (define-key map [(control ?i)]             'minibuffer-complete)
  (define-key map [tab]                      'minibuffer-complete)
  (define-key map [(control meta ?/)]        nil)
  (define-key map [(control meta tab)]       nil)
  (define-key map [(meta ?h)]                nil)
  (define-key map [(meta pause)]             nil)
  (define-key map [(control pause)]          nil)
  (define-key map [(control insert)]         nil)
  (define-key map [insert]                   nil)
  (define-key map "?"                        'minibuffer-completion-help)
  (define-key map " "                        'minibuffer-complete-word)
  (define-key map [M-S-backspace]            nil)
  (define-key map [M-S-delete]               nil)
  (define-key map [(meta ?o)]                nil)
  (define-key map [(meta ?k)]                nil)
  (define-key map [(meta ?q)]                nil)
  (define-key map [(control ?g)]             'abort-recursive-edit)
  (define-key map [(control ?l)]             nil)
  (define-key map [(control shift ?l)]       nil)
  (define-key map [(meta ?$)]                nil)
  (define-key map [(control ?~)]             nil)
  (define-key map [(control ?-)]             nil)
  (define-key map [(control ?+)]             nil)
  (define-key map [(control ?*)]             nil)
  (define-key map [(control ?>)]             nil)
  (define-key map [(control meta ?>)]        nil)
  (define-key map [(control ?\))]            nil)
  (define-key map [(control meta ?\))]       nil)
  (define-key map [(control meta ?<)]        nil)
  (define-key map [(control meta ?})]        nil)
  (define-key map [(control meta ?{)]        nil)
  (define-key map [(control ?})]             nil)
  (define-key map [(control ?{)]             nil)
  (define-key map [(control ?%)]             nil)
  (define-key map [(control ?:)]             nil)
  (define-key map [(control ?=)]             nil)
  (define-key map [(control ?,)]             nil)
  (define-key map [(control ?`)]             nil)
  (define-key map [(control meta ?`)]        nil)
  (define-key map [(control ?<)]             nil)
  (define-key map [(control ?$)]             nil)
  (define-key map [(control ??)]             nil)
  (define-key map [(control ?.)]             nil)
  (define-key map [(control ?#)]             nil)
  (define-key map [(control ?^)]             nil)
  (define-key map [(control shift ?a)]       nil)
  (define-key map [(meta ?~)]                nil)
  (define-key map [(meta ?,)]                nil)
  (define-key map [(control meta ?,)]        nil)
  (define-key map [(meta ?.)]                nil)
  (define-key map [(meta ?*)]                nil)
  (define-key map [(meta ?&)]                nil)
  (define-key map [(control meta ?&)]        nil)
  (define-key map [(shift ?\ )]              nil)
  (define-key map [(meta ?:)]                nil)
  (define-key map [(meta ?n)]                'next-history-element)
  (define-key map [down]                     'next-history-element)
  (define-key map [next]                     'next-history-element)
  (define-key map [(meta ?p)]                'previous-history-element)
  (define-key map [up]                       'previous-history-element)
  (define-key map [prior]                    'switch-to-completions)
  (define-key map [(meta ?v)]                'switch-to-completions))

(defun icicle-minibuffer-setup ()
  "Run in minibuffer on activation, to enable completion cycling.
Usually run by inclusion in `minibuffer-setup-hook'."
  (when (and icicle-mode (window-minibuffer-p (selected-window)) (not executing-kbd-macro))
    ;; The pre- and post-command hooks are local to the
    ;; minibuffer, so they are added here, not in `icicle-mode'.
    ;; They are removed in `icicle-mode' when mode is exited.
    (unless (fboundp 'define-minor-mode) (make-local-hook 'pre-command-hook))
    (add-hook 'pre-command-hook         'icicle-top-level-prep)
    (add-hook 'pre-command-hook         'icicle-run-icicle-pre-command-hook nil t)
    (unless (fboundp 'define-minor-mode) (make-local-hook 'post-command-hook))
    (add-hook 'post-command-hook        'icicle-run-icicle-post-command-hook nil t)
    ;; Change the region background here dynamically.  It would be better to
    ;; just use a buffer-local face, but those don't yet exist.
    (when (= 1 (recursion-depth))
      (setq icicle-saved-region-background (face-background 'region)))
    (when icicle-change-region-background-flag
      (set-face-background 'region icicle-region-background))
    ;; Reset prompt, because some commands (e.g. `find-file') don't use `read-file-name'
    ;; or `completing-read'.  Reset other stuff too.
    (setq icicle-candidate-nb                   nil
          icicle-completion-candidates          nil
          icicle-current-completion-mode        nil
          icicle-completing-p                   nil
          icicle-default-directory              default-directory
          icicle-incremental-completion-p       icicle-incremental-completion-flag
          icicle-initial-value                  nil
          icicle-last-completion-command        nil
          icicle-last-completion-candidate      nil
          icicle-last-input                     nil
          icicle-input-fail-pos                 nil
          icicle-pre-minibuffer-buffer          (cadr (buffer-list))
          icicle-prompt                         "")
    (while icicle-saved-candidate-overlays
      (delete-overlay (car icicle-saved-candidate-overlays))
      (setq icicle-saved-candidate-overlays (cdr icicle-saved-candidate-overlays)))
    (when icicle-cycling-respects-completion-mode-flag
      (dolist (map (if (boundp 'minibuffer-local-filename-completion-map)
                       (list minibuffer-local-completion-map
                             minibuffer-local-filename-completion-map
                             minibuffer-local-must-match-map)
                     (list minibuffer-local-completion-map minibuffer-local-must-match-map)))
        (define-key map icicle-modal-cycle-up-key   'icicle-previous-candidate-per-mode)
        (define-key map icicle-modal-cycle-down-key 'icicle-next-candidate-per-mode)))
    (icicle-update-ignored-extensions-regexp)
    (when (memq icicle-init-value-flag '(preselect-start preselect-end))
      (icicle-select-minibuffer-contents))
    (when (and icicle-show-Completions-initially-flag
               (icicle-completing-p)    ; Function initializes variable `icicle-completing-p'.
               (sit-for icicle-incremental-completion-delay)) ; Let user interrupt.
      ;; Apropos completion, by default.      
      (icicle-apropos-complete))        ; Defined in `icicles-cmd.el'.
    (run-hooks 'icicle-minibuffer-setup-hook)))

(defun icicle-select-minibuffer-contents ()
  "Select minibuffer contents and leave point at its beginning."
  (let ((min (icicle-minibuffer-prompt-end)))
    (set-mark (if (eq 'preselect-start icicle-init-value-flag) (point-max) min))
    (goto-char (if (eq 'preselect-start icicle-init-value-flag) min (point-max)))))

;; $$$ (defadvice next-history-element (after icicle-select-minibuffer-contents activate)
;;   "Select minibuffer contents and leave point at its beginning."
;;   (when (and icicle-mode (memq icicle-init-value-flag '(preselect-start preselect-end)))
;;     (icicle-select-minibuffer-contents)
;;     (setq deactivate-mark nil)))

(defun icicle-cancel-Help-redirection ()
  "Cancel redirection of focus from *Help* buffer to minibuffer.
Focus was redirected during `icicle-help-on-candidate'."
  (let* ((help-window (get-buffer-window "*Help*" 0))
         (help-frame (and help-window (window-frame help-window))))
    (when help-frame (redirect-frame-focus help-frame))))

(defun icicle-run-icicle-pre-command-hook ()
  "Run `icicle-pre-command-hook' functions.
Used in `pre-command-hook'."
  (run-hooks 'icicle-pre-command-hook))

(defun icicle-run-icicle-post-command-hook ()
  "Run `icicle-post-command-hook' functions.
Used in `post-command-hook'."
  (run-hooks 'icicle-post-command-hook))

(defun icicle-set-calling-cmd ()
  "Remember last command that called for completion.
Used in `completion-setup-hook'."
  (setq icicle-cmd-calling-for-completion this-command))

(defun icicle-update-ignored-extensions-regexp ()
  "Update ignored extensions if `completion-ignored-extensions' changed."
  (when (and (icicle-file-name-input-p) ; Defined in `icicles-fn.el'.
             (not (equal icicle-ignored-extensions completion-ignored-extensions)))
    (setq icicle-ignored-extensions-regexp ; Make regexp for ignored file extensions.
          (concat "\\(" (mapconcat #'regexp-quote completion-ignored-extensions "\\|") "\\)\\'"))
    ;; Flag to prevent updating `icicle-ignored-extensions-regexp' unless
    ;; `completion-ignored-extensions' changes.
    (setq icicle-ignored-extensions completion-ignored-extensions)))

;; We change the region background here dynamically.
;; It would be better to just use a buffer-local face, but those don't yet exist.
(defun icicle-restore-region-face ()
  "Restore region face.  It was changed during minibuffer activity
if `icicle-change-region-background-flag' is non-nil."
  (when icicle-change-region-background-flag
    (set-face-background 'region icicle-saved-region-background)))

(defun icicle-activate-mark ()
  "Prevent region from being deactivated.  Use in `icicle-post-command-hook'."
  (when (and (window-minibuffer-p (selected-window))
             icicle-completing-p
             (not executing-kbd-macro))
    (setq deactivate-mark nil)))

(defun icicle-redefine-standard-commands ()
  "Replace certain standard Emacs commands with Icicles versions."
  (when (and (fboundp 'icicle-completing-read) icicle-redefine-standard-commands-flag)
    (defalias 'dabbrev-completion           (symbol-function 'icicle-dabbrev-completion))
    (defalias 'lisp-complete-symbol         (symbol-function 'icicle-lisp-complete-symbol))
    (defalias 'repeat-complex-command       (symbol-function 'icicle-repeat-complex-command))
    (defalias 'customize-apropos            (symbol-function 'icicle-customize-apropos))
    (defalias 'customize-apropos-faces      (symbol-function 'icicle-customize-apropos-faces))
    (defalias 'customize-apropos-groups     (symbol-function 'icicle-customize-apropos-groups))
    (defalias 'customize-apropos-options    (symbol-function 'icicle-customize-apropos-options))
    (defalias 'customize-face               (symbol-function 'icicle-customize-face))
    (defalias 'customize-face-other-window  (symbol-function 'icicle-customize-face-other-window))
    (defalias 'read-from-minibuffer         (symbol-function 'icicle-read-from-minibuffer))
    (defalias 'read-string                  (symbol-function 'icicle-read-string))))

(defun icicle-restore-standard-commands ()
  "Restore standard Emacs commands replaced in Icicle mode."
  (when (and (fboundp 'old-completing-read) icicle-redefine-standard-commands-flag)
    (defalias 'dabbrev-completion           (symbol-function 'old-dabbrev-completion))
    (defalias 'lisp-complete-symbol         (symbol-function 'old-lisp-complete-symbol))
    (defalias 'repeat-complex-command       (symbol-function 'old-
1519
repeat-complex-command))
    (defalias 'customize-apropos            (symbol-function 'old-customize-apropos))
    (defalias 'customize-apropos-faces      (symbol-function 'old-customize-apropos-faces))
    (defalias 'customize-apropos-groups     (symbol-function 'old-customize-apropos-groups))
    (defalias 'customize-apropos-options    (symbol-function 'old-customize-apropos-options))
    (defalias 'customize-face               (symbol-function 'old-customize-face))
    (defalias 'customize-face-other-window  (symbol-function 'old-customize-face-other-window))
    (defalias 'read-from-minibuffer         (symbol-function 'old-read-from-minibuffer))
    (defalias 'read-string                  (symbol-function 'old-read-string))))

(defun icicle-redefine-std-completion-fns ()
  "Replace standard completion functions with versions for Icicle mode."
  (when (fboundp 'icicle-completing-read)
    (defalias 'next-history-element         (symbol-function 'icicle-next-history-element))
    (defalias 'exit-minibuffer              (symbol-function 'icicle-exit-minibuffer))
    (defalias 'minibuffer-complete-and-exit (symbol-function 'icicle-minibuffer-complete-and-exit))
    (defalias 'switch-to-completions        (symbol-function 'icicle-switch-to-completions))
    (defalias 'choose-completion-string     (symbol-function 'icicle-choose-completion-string))
    (defalias 'mouse-choose-completion      (symbol-function 'icicle-mouse-choose-completion))
    (defalias 'completion-setup-function    (symbol-function 'icicle-completion-setup-function))
    (defalias 'completing-read              (symbol-function 'icicle-completing-read))
    (defalias 'read-file-name               (symbol-function 'icicle-read-file-name))
    (defalias 'read-face-name               (symbol-function 'icicle-read-face-name))
    (defalias 'display-completion-list      (symbol-function 'icicle-display-completion-list))
    (when (fboundp 'face-valid-attribute-values)
      (defalias 'face-valid-attribute-values (symbol-function
                                              'icicle-face-valid-attribute-values)))))

(defun icicle-restore-std-completion-fns ()
  "Restore standard completion functions replaced in Icicle mode."
  (when (fboundp 'old-completing-read)
    (defalias 'next-history-element         (symbol-function 'old-next-history-element))
    (defalias 'exit-minibuffer              (symbol-function 'old-exit-minibuffer))
    (defalias 'minibuffer-complete-and-exit (symbol-function 'old-minibuffer-complete-and-exit))
    (defalias 'switch-to-completions        (symbol-function 'old-switch-to-completions))
    (defalias 'choose-completion-string     (symbol-function 'old-choose-completion-string))
    (defalias 'mouse-choose-completion      (symbol-function 'old-mouse-choose-completion))
    (defalias 'completion-setup-function    (symbol-function 'old-completion-setup-function))
    (defalias 'completing-read              (symbol-function 'old-completing-read))
    (defalias 'read-file-name               (symbol-function 'old-read-file-name))
    (defalias 'read-face-name               (symbol-function 'old-read-face-name))
    (defalias 'display-completion-list      (symbol-function 'old-display-completion-list))
    (when (fboundp 'face-valid-attribute-values)
      (defalias 'face-valid-attribute-values (symbol-function 'old-face-valid-attribute-values)))))

(defun icicle-redefine-standard-options ()
  "Replace certain standard Emacs options with Icicles versions."
  (when (boundp 'icicle-search-ring-max)
    (setq icicle-saved-search-ring-max        search-ring-max ; Save it.
          search-ring-max                     icicle-search-ring-max)
    (setq icicle-saved-regexp-search-ring-max regexp-search-ring-max ; Save it.
          regexp-search-ring-max              icicle-regexp-search-ring-max))
  (when (boundp 'icicle-kmacro-ring-max)
    (setq icicle-saved-kmacro-ring-max        kmacro-ring-max ; Save it.
          kmacro-ring-max                     icicle-kmacro-ring-max)))

(defun icicle-restore-standard-options ()
  "Restore standard Emacs options replaced in Icicle mode."
  (when (boundp 'icicle-saved-search-ring-max)
    (setq search-ring-max        icicle-saved-search-ring-max)
    (setq regexp-search-ring-max icicle-saved-regexp-search-ring-max)))

;; This is used only in Emacs 22+, but we define it always anyway.
(defun icicle-undo-std-completion-faces ()
  "Get rid of standard completion-root highlighting in *Completions*."
  ;; Do this because the standard Emacs 22 highlighting can interfere with
  ;; apropos-completion highlighting.
  (when (fboundp 'face-spec-reset-face)
    (when (facep 'completions-common-part)
      (face-spec-reset-face 'completions-common-part)
      (set-face-attribute 'completions-common-part nil :inherit nil))
    (when (facep 'completions-first-difference)
      (face-spec-reset-face 'completions-first-difference)
      (set-face-attribute 'completions-first-difference nil :inherit nil))))

(defun icicle-bind-isearch-keys ()
  "Bind `S-TAB' in Isearch maps.  Use in `isearch-mode-hook'."
  (define-key isearch-mode-map [S-tab] 'icicle-isearch-complete)
  (define-key minibuffer-local-isearch-map [S-tab] 'isearch-complete-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'icicles-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; icicles-mode.el ends here

0

