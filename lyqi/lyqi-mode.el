;;	$RCSfile: lyqi-mode.el,v $	
;;	$Revision: 1.9 $	
;;	$Date: 2004/03/14 15:14:55 $	
;;	$Author: nicolas $	
;;; 
;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2003 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/
;;; 

(defgroup lyqi nil
  "LilyPond quick insert mode."
  :prefix "lyqi-"
  :group 'applications)

(eval-when-compile (require 'cl))
(require 'lyqi-base)
(require 'lyqi-parser)
(require 'lyqi-editor)
(require 'lyqi-midi)
(require 'lyqi-rumor)

(defconst lyqi-version "0.2.5")

(defconst lyqi-languages 
  '(nederlands english deutsch norsk svenska italiano catalan espanol)
  "Possible languages for writing LilyPond note names.")

(defcustom lyqi-self-inserting-keys "()<>~{}|[] "
  "Self inserting keys in lyqi-mode-map."
  :group 'lyqi
  :type 'string)

(defcustom lyqi-self-inserting-+-char-keys "-_^\\"
  "Self inserting keys, after which the user is asked an extra char to insert."
  :group 'lyqi
  :type 'string)

(defcustom lyqi-self-inserting-+-string-keys
  '((?- "\C-c-") (?_ "\C-c_") (?^ "\C-c^") (?\\ "\C-c\\") (?# "#") (?\" "\"" "\""))
  "Self inserting keys, after which the user is asked an extra string to insert."
  :group 'lyqi)

(defcustom lyqi-force-duration t
  "Force duration to appear when inserting a note"
  :group 'lyqi
  :type  'boolean)

(defcustom lyqi-relative-octave-default nil
  "Relative or absolute octave in lilypond insert mode by default?"
  :group 'lyqi
  :type  'boolean)

(defcustom lyqi-default-language 'nederlands
  "The default language for writing LilyPond note names."
  :group 'lyqi
  :options lyqi-languages
  :type 'symbol)

(defvar lyqi-editing-state nil
  "The current editing state: language, octave mode, etc.")

(defvar lyqi-mudela-editor nil
  "A mudela editor.")

(defvar lyqi-mudela-parser nil
  "A rudimentary mudela parser")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lilypond-quick-insert-mode interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-GNUEmacs (&rest body)
  (if (string-match "GNU Emacs" (version)) `(progn ,@body)))

(defmacro with-XEmacs (&rest body)
  (if (string-match "XEmacs" (version)) `(progn ,@body)))

(with-XEmacs
 (defun my-get-key (fn)
   "Returns the key (a string) binded to `fn'"
   (let ((keys (where-is-internal fn nil t)))
     (and keys (my-join (mapcar (lambda (key) 
                                  (if (consp key)
                                      (format "%s-%s"
                                              (upcase (substring (symbol-name (car key)) 0 1))
                                              (symbol-name (cadr key)))
                                         (symbol-name key)))
                                (append keys nil)) " ")))))
(with-GNUEmacs
 (defun my-get-key (fn)
   "Returns the key (a string) binded to `fn'"
   (let ((keys (where-is-internal fn nil t)))
     (and keys (my-join (mapcar (lambda (key) (if (and (<= 1 key) (<= key 26)) 
                                                  (format "C-%c" (+ 96 key))
                                                  (char-to-string key)))
                                keys) " ")))))

(defun lyqi-display-state ()
  "Display current state (language used, octave mode) and help commands in the minibuffer."
  (message "lyqi-%s [%s,%s pitches,midi %s] Press %s to quit, %s for help."
           lyqi-version
           (slot-value lyqi-editing-state 'language)
           (if (slot-value lyqi-editing-state 'relative-octave) "relative" "absolute")
           (if lyqi-midi-on "on" "off")
           (my-get-key 'lyqi-quit)
           (my-get-key 'lyqi-help)))

;; (defmacro with-lyqi-interactive (&rest body)
;;   "Utility to make a lyqi interactive command, with message display at the end."
;;   `(progn
;;      (interactive)
;;      ,@body
;;      (lyqi-display-state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note, rest, skips insertion

(defun lyqi-insert-note (pitch)
  "Insert a new mudela note of pitch `pitch'."
  (let ((note (make-note lyqi-mudela-editor pitch)))
    (word-insert lyqi-mudela-editor note)
    (when lyqi-midi-on
      (play-note note t))
    (lyqi-display-state)))

(defun lyqi-insert-note-do ()
  "Insert a new do / c note at point."
  (interactive)
  (lyqi-insert-note 0))

(defun lyqi-insert-note-re ()
  "Insert a new re / d note at point."
  (interactive)
  (lyqi-insert-note 1))

(defun lyqi-insert-note-mi ()
  "Insert a new mi / e note at point."
  (interactive)
  (lyqi-insert-note 2))

(defun lyqi-insert-note-fa ()
  "Insert a new fa / f note at point."
  (interactive)
  (lyqi-insert-note 3))

(defun lyqi-insert-note-sol ()
  "Insert a new sol / g note at point."
  (interactive)
  (lyqi-insert-note 4))

(defun lyqi-insert-note-la ()
  "Insert a new la / a note at point."
  (interactive)
  (lyqi-insert-note 5))

(defun lyqi-insert-note-si ()
  "Insert a new si / b note at point."
  (interactive)
  (lyqi-insert-note 6))

(defun lyqi-insert-rest ()
  "Insert a rest at point."
  (interactive)
  (word-insert lyqi-mudela-editor (make-rest lyqi-mudela-editor))
  (lyqi-display-state))

(defun lyqi-insert-skip ()
  "Insert a skip at point."
  (interactive)
  (word-insert lyqi-mudela-editor (make-skip lyqi-mudela-editor))
  (lyqi-display-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; note, rest, skips update

;; (defun lyqi-change-duration (duration)
;;   "Change the last word duration, if any, and if possible."
;;   (with-word-update lyqi-mudela-editor the-word
;;    (set-duration the-word duration)))
(defun lyqi-change-duration (duration)
  "Change the last word duration, if any, and if possible."
  (with-word-update lyqi-mudela-editor the-word
    (set-duration the-word duration)))

(defun lyqi-change-duration-1 ()
  "Change the previous word duration to 1."
  (interactive)
  (lyqi-change-duration 1)
  (lyqi-display-state))

(defun lyqi-change-duration-2 ()
  "Change the previous word duration to 2."
  (interactive)
  (lyqi-change-duration 2)
  (lyqi-display-state))

(defun lyqi-change-duration-4 ()
  "Change the previous word duration to 4."
  (interactive)
  (lyqi-change-duration 3)
  (lyqi-display-state))

(defun lyqi-change-duration-8 ()
  "Change the previous word duration to 8."
  (interactive)
  (lyqi-change-duration 4)
  (lyqi-display-state))

(defun lyqi-change-duration-16 ()
  "Change the previous word duration to 16."
  (interactive)
  (lyqi-change-duration 5)
  (lyqi-display-state))

(defun lyqi-change-duration-32 ()
  "Change the previous word duration to 32."
  (interactive)
  (lyqi-change-duration 6)
  (lyqi-display-state))

(defun lyqi-change-duration-64 ()
  "Change the previous word duration to 64."
  (interactive)
  (lyqi-change-duration 7)
  (lyqi-display-state))

(defun lyqi-change-duration-128 ()
  "Change the previous word duration to 128."
  (interactive)
  (lyqi-change-duration 8)
  (lyqi-display-state))

(defun lyqi-change-dots ()
  "Increase modulo 5 the previous word dot number."
  (interactive)
  (with-word-update lyqi-mudela-editor the-word
   (set-dots the-word))
  (lyqi-display-state))

(defun lyqi-change-alteration-up ()
  "Increase, if possible, the last note alteration."
  (interactive)
  (with-word-update lyqi-mudela-editor the-note
   (set-alteration-up the-note)
   (when lyqi-midi-on
     (play-note the-note t)))
  (lyqi-display-state))

(defun lyqi-change-alteration-down ()
  "Decrease, if possible, the last note alteration."
  (interactive)
  (with-word-update lyqi-mudela-editor the-note
   (set-alteration-down the-note)
   (when lyqi-midi-on
     (play-note the-note t)))
  (lyqi-display-state))

(defun lyqi-change-alteration-natural ()
  "Set, if possible, the last note alteration to natural."
  (interactive)
  (with-word-update lyqi-mudela-editor the-note
   (set-alteration-natural the-note)
   (when lyqi-midi-on
     (play-note the-note t)))
  (lyqi-display-state))

(defun lyqi-change-octave-up ()
  "Increase the last note octave."
  (interactive)
  (with-word-update lyqi-mudela-editor the-note
   (set-octave-up the-note)
   (when lyqi-midi-on
     (play-note the-note t)))
  (lyqi-display-state))

(defun lyqi-change-octave-down ()
  "Decrease the last note octave."
  (interactive)
  (with-word-update lyqi-mudela-editor the-note
   (set-octave-down the-note)
   (when lyqi-midi-on
     (play-note the-note t)))
  (lyqi-display-state))

(defun lyqi-change-octave-zero ()
  "Set the last note octave to zero."
  (interactive)
  (with-word-update lyqi-mudela-editor the-note
   (set-octave-zero the-note)
   (when lyqi-midi-on
     (play-note the-note t)))
  (lyqi-display-state))

(defun lyqi-change-reminder-alt ()
  "Change the last note's reminder alteration state."
  (interactive)
  (with-word-update lyqi-mudela-editor the-note
   (set-reminder-alt the-note))
  (lyqi-display-state))  
  
(defun lyqi-change-cautionary-alt ()
  "Change the last note's cautionary alteration state."
  (interactive)
  (with-word-update lyqi-mudela-editor the-note
   (set-cautionary-alt the-note))
  (lyqi-display-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(with-XEmacs
 (defun my-read-char-exclusive (prompt)
   (let ((event (next-event nil prompt)))
     (while (not (key-press-event-p event))
       (next-event event prompt))
     (event-to-character event))))
(with-GNUEmacs
 (defun my-read-char-exclusive (prompt)
   (read-char-exclusive prompt)))

(defun lyqi-insert-tuplet ()
  "Interactively inserts a \\times x/y {"
  (interactive)
  (let ((x ""))
    (lyqi-just-one-space)
    (insert "\\times ")
    (while (not (and (string< x "9") (string< "0" x)))
      (setq x (char-to-string (my-read-char-exclusive 
                               "Insert a number for the numerator (\"x/\")"))))
    (insert (format "%s/" x)) (setq x "/")
    (while (not (and (string< x "9") (string< "0" x)))
      (setq x (char-to-string (my-read-char-exclusive 
                               "Insert a number for the denominator (\"/y\")"))))
    (insert (format "%s { " x)))
  (lyqi-display-state))

(defun lyqi-word-forward ()
  "Move to the following mudela word end, if any, otherwise to the end of the 
following text word."
  (interactive)
  (unless (re-search-forward (slot-value lyqi-mudela-parser 'regexp) nil t)
    (forward-word 1))
  (lyqi-display-state))

(defun lyqi-word-backward ()
  "Move to the previous mudela word beginning, if any, otherwise to the beginning of the 
previous text word."
  (interactive)
  (unless (re-search-backward (slot-value lyqi-mudela-parser 'regexp) nil t)
    (backward-word 1))
  (lyqi-display-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 

(defun my-next (elt seq)
  "Returns the element following `elt' in `seq'. 
If it is the last, returns the first element of the sequence."
  (let ((nexts (cdr (member elt seq))))
    (if (consp nexts)
        (car nexts)
        (car seq))))

(defun lyqi-switch-language ()
  "Select the next mudela language in `lyqi-languages'."
  (interactive)
  (setf lyqi-default-language 
        (my-next (slot-value lyqi-editing-state 'language) lyqi-languages))
  (set-language lyqi-editing-state lyqi-default-language)
  (update-regexp lyqi-mudela-parser)
  (lyqi-display-state))

(defun lyqi-switch-octave-mode ()
  "Switch between relative and absolute octave modes."
  (interactive)
  (setf (slot-value lyqi-editing-state 'relative-octave)
        (not (slot-value lyqi-editing-state 'relative-octave)))
  (lyqi-display-state))

(defun my-center-string (str len)
  "Makes a centered string from `str', of length `len'"
  (let* ((inner-str (if (< (length str) len)
                        str
                        (substring str 0 len)))
         (right-space (/ (- len (length inner-str)) 2))
         (left-space (- len right-space (length inner-str))))
    (format "%s%s%s" 
            (make-string left-space (string-to-char " "))
            inner-str
            (make-string right-space (string-to-char " ")))))

(defun lyqi-help ()
  "Display a help message in a dedicated buffer."
  (interactive)
  (describe-mode)
;;   (with-output-to-temp-buffer "*Help*"
;;     (princ "LilyPond quick insert mode
;; Note entry:
;;               |  | | | |  |  | | | | | |  |
;;               |  | | | |  |  | | | | | |  |
;;               |  |_| |_|  |  |_| |_| |_|  |
;;               |   |   |   |   |   |   |   |
;;               |___|___|___|___|___|___|___|\n")

;;     (princ (format "pitch keys:    %s \n\n"
;;                    (my-join (mapcar (lambda (fn)
;;                                       (my-center-string (format "`%s'" (my-get-key fn)) 3))
;;                                     '(lyqi-insert-note-do
;;                                       lyqi-insert-note-re
;;                                       lyqi-insert-note-mi
;;                                       lyqi-insert-note-fa
;;                                       lyqi-insert-note-sol
;;                                       lyqi-insert-note-la
;;                                       lyqi-insert-note-si)) " ")))
;;     (princ (format "duration keys: %s\n"
;;                    (my-join (mapcar (lambda (fn)
;;                                       (my-center-string (format "`%s'" (my-get-key fn)) 3))
;;                                     '(lyqi-change-duration-1
;;                                       lyqi-change-duration-2
;;                                       lyqi-change-duration-4
;;                                       lyqi-change-duration-8
;;                                       lyqi-change-duration-16
;;                                       lyqi-change-duration-32
;;                                       lyqi-change-duration-64
;;                                       lyqi-change-duration-128)) " ")))
;;     (princ (format "               %s\n"
;;                    (my-join (mapcar (lambda (num)
;;                                       (my-center-string 
;;                                        (int-to-string (expt 2 (- num 1))) 3))
;;                                     '(1 2 3 4 5 6 7 8)) "|")))
;;     (princ (format "alteration:    `%s' flat 
;;                `%s' sharp\n
;;                `%s' force reminder alteration\n
;;                `%s' force cautionary alteration\n"
;;                    (my-get-key 'lyqi-change-alteration-down)
;;                    (my-get-key 'lyqi-change-alteration-up)
;;                    (my-get-key 'lyqi-change-reminder-alt)
;;                    (my-get-key 'lyqi-change-cautionary-alt)))
;;     (princ (format "dot key:       `%s'\n" (my-get-key 'lyqi-change-dots)))
;;     (princ (format "octave keys:   `%s' down
;;                `%s' up\n"
;;                    (my-get-key 'lyqi-change-octave-down)
;;                    (my-get-key 'lyqi-change-octave-up)))
;;     (princ (format "rests:         `%s'\n" (my-get-key 'lyqi-insert-rest)))
;;     (princ (format "skips:         `%s'\n" (my-get-key 'lyqi-insert-skip)))
;;     (princ (format "tuplets:       `%s'\n" (my-get-key 'lyqi-insert-tuplet)))
;;     (princ (format "self inserting keys: `%s'\n" (my-join (split-string lyqi-self-inserting-keys "") "' `")))
;;     (princ "\nOther bindings:\n")
;;     (princ (format "absolute/relative octave switch: `%s'\n" 
;;                    (my-get-key 'lyqi-switch-octave-mode)))
;;     (princ (format "language switch: `%s'\n" (my-get-key 'lyqi-switch-language)))
;;     (princ (format "help:            `%s'\n" (my-get-key 'lyqi-help)))
;;     (princ (format "Midi note playing start/stop: `%s'\n" 
;;                    (my-get-key 'lyqi-midi-start-stop)))
;;     (princ (format "back to LilyPond-mode: `%s'\n" (my-get-key 'lyqi-quit)))
;;     (princ (format "Transpose region: `%s'\n" (my-get-key 'lyqi-transpose-region)))
    (lyqi-display-state))

(defun lyqi-quit ()
  "Quit lilypond-quick-insert-mode, back to LilyPond-mode"
  (interactive)
  (LilyPond-mode))

(defun lyqi-relative-to-absolute-region ()
  "Rewrite region with absolute octave mode instead of relative octave mode.
An octave transposition may be required afterward."
  (interactive)
  (when (not (slot-value lyqi-editing-state 'relative-octave))
    (lyqi-switch-octave-mode))
  (change-octave-mode-region lyqi-mudela-editor (region-beginning) (region-end))
  (lyqi-display-state))

(defun lyqi-absolute-to-relative-region ()
  "Rewrite region with relative octave mode instead of absolute octave mode."
  (interactive)
  (when (slot-value lyqi-editing-state 'relative-octave)
    (lyqi-switch-octave-mode))
  (change-octave-mode-region lyqi-mudela-editor (region-beginning) (region-end))
  (lyqi-display-state))  

(defun lyqi-transpose-region-aux (from-note to-note)
  "Transpose the current region, the interval being defined by `from-note'
and `to-note', two mudela-notes."
  (when to-note
    (let ((note-diff (+ (- (slot-value to-note 'pitch)
                           (slot-value from-note 'pitch))
                        (* 7 (- (slot-value to-note 'octave)
                                (slot-value from-note 'octave)))))
          (exact-pitch-diff (- (midi-pitch to-note) (midi-pitch from-note))))
      (transpose-region lyqi-mudela-editor note-diff exact-pitch-diff (region-beginning) (region-end)))))

(defun lyqi-transpose-region (to-note-str)
  "Interactively transpose the current region. The user is asked the transposition interval, 
starting from c/do."
  (interactive "sTranspose to: ")
  (let ((from-note (make-instance 'mudela-note :pitch 0 :octave 0))
        (to-note (parse-string lyqi-mudela-parser to-note-str (make-instance 'mudela-note-state :octave 0))))
    (lyqi-transpose-region-aux from-note to-note)))

;;; by Reuben Thomas <rrt@sc3d.org>
(defun lyqi-transpose-interval-region (trans)
  "Interactively transpose the current region. The user is asked the transposition interval in tones."
  (interactive "sTranspose by interval (tones[+]|[-]) : ")
  (let* ((interval (string-to-int trans))
         (adj (substring trans -1))
         (alt (cond ((equal adj "+") 3) 
                    ((equal adj "-") 1) 
                    (t               2)))
         (oct (/ interval 7))
         (tone (% interval 7)))
    (when (< tone 0)
      (setq tone (+ tone 7))
      (setq oct (- oct 1)))
    (let ((from-note (make-instance 'mudela-note :pitch 0 :octave 0))
          (to-note (make-instance 'mudela-note :pitch tone :octave oct :alteration alt)))
      (lyqi-transpose-region-aux from-note to-note)))
  (lyqi-display-state))

(defun lyqi-play-back-region ()
  "If midi is on, play back notes in region."
  (interactive)
  (when (process-runningp lyqi-midi-keyboard)
    (mapcar 'play-note (parse-region lyqi-mudela-parser (region-beginning) (region-end)))))

(defun lyqi-change-language-region ()
  "Change note language in region. The user is asked for source and destination languages."
  (interactive)
  (let* ((current-lang (slot-value lyqi-editing-state 'language))
         (next-lang (my-next current-lang lyqi-languages))
         (lang-collection (mapcar (lambda (lang) (list (symbol-name lang))) lyqi-languages))
         (from-lang (intern (completing-read (format "Change from language [%s]: " current-lang)
                                             lang-collection nil t nil nil (symbol-name current-lang))))
         (to-lang (intern (completing-read (format "Change from language %s to [%s]: " from-lang next-lang)
                                           lang-collection nil t nil nil (symbol-name next-lang)))))
    (change-language-region lyqi-mudela-editor from-lang to-lang (region-beginning) (region-end)))
  (lyqi-display-state))

;; (defun lyqi-self-insert-plus-char (char))
;; (defun lyqi-self-insert-plus-string (char) &optional ending)

;;; Rumor

(defun lyqi-rumor-session-stop ()
  "Stop a running rumor session."
  (interactive)
  (process-stop lyqi-rumor-process)
  (define-key lyqi-mode-map " " 'self-insert-command))

(defun lyqi-rumor-session-start ()
  "Start a rumor session. Press SPC to stop the session"
  (interactive)
  (define-key lyqi-mode-map " " 'lyqi-rumor-session-stop)
  (process-start lyqi-rumor-process))

(defun lyqi-rumor-set-legato ()
  "Change rumor's legato parameter."
  (interactive)
  (let ((legato (with-slots (legato) lyqi-rumor-process
                            (setf (slot-value lyqi-rumor-process 'legato)
                                  (not legato)))))
    (message "rumor: legato mode set %s for next session." (if legato "on" "off"))
    legato))

(defun lyqi-rumor-set-no-dots ()
  "Change rumor's no-dots parameter."
  (interactive)
  (let ((no-dots (with-slots (no-dots) lyqi-rumor-process
                   (setf (slot-value lyqi-rumor-process 'no-dots)
                         (not no-dots)))))
    (message "rumor: dots %sshown in next session." (if no-dots "not " ""))
    no-dots))

(defun lyqi-rumor-set-flat ()
  "Change rumor's flat parameter."
  (interactive)
  (let ((flat (with-slots (flat) lyqi-rumor-process
                (setf (slot-value lyqi-rumor-process 'flat)
                      (not flat)))))
    (message "rumor: flat mode set %s for next session." (if flat "on" "off"))
    flat))

(defun lyqi-rumor-set-grain (grain-str)
  "Set rumor's grain."
  (interactive "sRumor's new grain: ")
  (let ((grain (setf (slot-value lyqi-rumor-process 'grain)
                     (string-to-number grain-str))))
    (message "rumor: grain set to %d for next session" grain)
    grain))

(defun lyqi-rumor-set-tempo (tempo-str)
  "Set rumor's tempo."
  (interactive "sRumor's new tempo: ")
  (let ((tempo (setf (slot-value lyqi-rumor-process 'tempo)
                     (string-to-number tempo-str))))
    (message "rumor: tempo set to %d for next session" tempo)
    tempo))

(defun lyqi-rumor-set-alsa-port (alsa-port-str)
  "Set rumor's alsa-port."
  (interactive "sRumor's new alsa port: ")
  (let ((port (setf (slot-value lyqi-rumor-process 'port)
                    (string-to-number alsa-port-str))))
    (message "rumor: alsa port set to %d for next session" port)
    port))

(defun lyqi-rumor-set-meter (meter)
  "Set rumor's meter."
  (interactive "sRumor's new meter: ")
  (setf (slot-value lyqi-rumor-process 'meter)
        meter)
  (message "rumor: meter set to %s for next session" meter)
  meter)

(defun lyqi-rumor-set-key (key)
  "Set rumor's key."
  (interactive "sRumor's new key (in dutsch): ")
  (setf (slot-value lyqi-rumor-process 'key)
        key)
  (message "rumor: key set to %s for next session" key)
  key)

;;; Midi play back

(defun lyqi-midi-start-stop ()
  "Start or stop midi playing."
  (interactive)
  (if lyqi-midi-on
      (lyqi-midi-stop)
      (lyqi-midi-start))
  (setq lyqi-midi-manually-off (not lyqi-midi-on))
  (lyqi-display-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lyqi-mode definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro lyqi-make-self-insert-+-char (key)
  "Define a self-insert-<char>-+-char function, and "
  (let ((fn-symb (intern (format "lyqi-%s-self-insert-+-char" key))))
    `(progn
       (defun ,fn-symb ()
         ,(format "Insert the character %s and ask the user an extra character to insert." key)
         (interactive)
         (insert ,key)
         (insert (char-to-string (my-read-char-exclusive "Following character: "))))
       (define-key lyqi-mode-map ,key ',fn-symb))))

(defmacro lyqi-make-self-insert-+-string (char key &optional end-string)
  "Define a self-insert-<char>-+-char function, and "
  (let ((fn-symb (intern (format "lyqi-%s-self-insert-+-string" (char-to-string char)))))
    `(progn
     (defun ,fn-symb ()
       ,(format "Insert the character %c and ask the user an extra character to insert." char)
       (interactive)
       (insert ,(char-to-string char))
       (insert (read-string "Following string: "))
       ,(if end-string `(insert ,end-string)))
     (define-key lyqi-mode-map ,key ',fn-symb))))

(define-derived-mode lyqi-mode
  LilyPond-mode "lyqi"
  "Major mode for LilyPond quick note insert.
\\{lyqi-mode-map}"
  (make-local-variable 'lyqi-editing-state)
  (make-local-variable 'lyqi-mudela-parser)
  (make-local-variable 'lyqi-mudela-editor)

  (setq lyqi-editing-state (make-instance 'mudela-editing-state 
                                          :translation-table mudela-translation-table
                                          :relative-octave lyqi-relative-octave-default
                                          :force-duration lyqi-force-duration))
  (set-language lyqi-editing-state lyqi-default-language)
  (setq lyqi-mudela-parser (make-mudela-parser lyqi-editing-state))
  (setq lyqi-mudela-editor (make-instance 'mudela-editor 
                                          :editing-state lyqi-editing-state
                                          :parser lyqi-mudela-parser
                                          :note-state (make-instance 'mudela-note-state)))
  ;; I don't know how to directly set a class slot !
  ;; Is that a feature or a bug ? when I set a class allocated slot
  ;; of a mother class instance, the slot is not also updated in child
  ;; class instances...
  (let ((c (make-instance 'mudela-note))
        (r (make-instance 'mudela-rest))
        (s (make-instance 'mudela-skip)))
    (setf (slot-value c 'editing-state) lyqi-editing-state)
    (setf (slot-value r 'editing-state) lyqi-editing-state)
    (setf (slot-value s 'editing-state) lyqi-editing-state))

  ;; midi start
  (unless lyqi-midi-timidity
    (cond (lyqi-midi-use-external-timidity-server
           (setq lyqi-midi-timidity (make-instance 'external-timidity-server))
           (setf (slot-value lyqi-midi-timidity 'seqport) lyqi-midi-external-timidity-port))
          (t
           (setq lyqi-midi-timidity (make-instance 'timidity-server
                                                   :command lyqi-midi-demon-command
                                                   :name "timidity")))))
  (unless lyqi-midi-keyboard
    (setq lyqi-midi-keyboard (make-instance 'mymidikbd
                                            :command lyqi-midi-keyboard-command
                                            :name "mymidikbd")))
  (unless lyqi-rumor-process
    (setq lyqi-rumor-process (make-instance 'rumor
                                            :command lyqi-rumor-command
                                            :name "rumor"
                                            :grain     lyqi-rumor-default-grain
                                            :tempo     lyqi-rumor-default-tempo
                                            :legato    lyqi-rumor-default-legato
                                            :no-dots   lyqi-rumor-default-no-dots
                                            :flat      lyqi-rumor-default-flat
                                            :strip     lyqi-rumor-default-strip
                                            :meter     lyqi-rumor-default-meter
                                            :key       lyqi-rumor-default-key
                                            :alsa-port lyqi-rumor-default-alsa-port)))
  (when (and (not lyqi-midi-manually-off)
             lyqi-midi-enabled-default)
    (lyqi-midi-start)))

;; makes all the printing characters undefined.
(suppress-keymap lyqi-mode-map t)
;; rests and skips
(define-key lyqi-mode-map "r" 'lyqi-insert-rest)
(define-key lyqi-mode-map "s" 'lyqi-insert-skip)
;; pitches : do re mi fa sol la si
(define-key lyqi-mode-map "d" 'lyqi-insert-note-do)
(define-key lyqi-mode-map "f" 'lyqi-insert-note-re)
(define-key lyqi-mode-map "g" 'lyqi-insert-note-mi)
(define-key lyqi-mode-map "h" 'lyqi-insert-note-fa)
(define-key lyqi-mode-map "j" 'lyqi-insert-note-sol)
(define-key lyqi-mode-map "k" 'lyqi-insert-note-la)
(define-key lyqi-mode-map "l" 'lyqi-insert-note-si)
;; alterations
(define-key lyqi-mode-map "i" 'lyqi-change-alteration-up)
(define-key lyqi-mode-map "e" 'lyqi-change-alteration-down)
(define-key lyqi-mode-map "n" 'lyqi-change-alteration-natural)
(define-key lyqi-mode-map "!" 'lyqi-change-reminder-alt)
(define-key lyqi-mode-map "?" 'lyqi-change-cautionary-alt)
;; octave
(define-key lyqi-mode-map "'" 'lyqi-change-octave-up)
(define-key lyqi-mode-map "," 'lyqi-change-octave-down)
(define-key lyqi-mode-map "=" 'lyqi-change-octave-zero)
;; durations: 1 2 4 8 16 32 64 128
(define-key lyqi-mode-map "1" 'lyqi-change-duration-1)
(define-key lyqi-mode-map "2" 'lyqi-change-duration-2)
(define-key lyqi-mode-map "4" 'lyqi-change-duration-4)
(define-key lyqi-mode-map "8" 'lyqi-change-duration-8)
(define-key lyqi-mode-map "7" 'lyqi-change-duration-16)
(define-key lyqi-mode-map "5" 'lyqi-change-duration-32)
(define-key lyqi-mode-map "0" 'lyqi-change-duration-64)
(define-key lyqi-mode-map "9" 'lyqi-change-duration-128)
;; dots
(define-key lyqi-mode-map "." 'lyqi-change-dots)
;; tuplets
(define-key lyqi-mode-map "\C-ct" 'lyqi-insert-tuplet)
;; other bindings
(define-key lyqi-mode-map "\C-co" 'lyqi-switch-octave-mode)
(define-key lyqi-mode-map "\C-c\C-l" 'lyqi-switch-language)
(define-key lyqi-mode-map "\C-cq" 'lyqi-quit) ; back to LilyPond-mode
(define-key lyqi-mode-map "\C-ch" 'lyqi-help)
(define-key lyqi-mode-map "\M-b" 'lyqi-word-backward)
(define-key lyqi-mode-map "\M-f" 'lyqi-word-forward)
(define-key lyqi-mode-map "\C-c\C-t" 'lyqi-transpose-region)
(define-key lyqi-mode-map "\C-cm" 'lyqi-midi-start-stop)
(define-key lyqi-mode-map "\C-cp" 'lyqi-play-back-region)
;; prefix key for rumor commands
(define-prefix-command 'ctl-c-p)
(define-key lyqi-mode-map "\C-cr" ctl-c-p)
(define-key lyqi-mode-map "\C-crs" 'lyqi-rumor-session-start)
(define-key lyqi-mode-map "\C-crg" 'lyqi-rumor-set-grain)
(define-key lyqi-mode-map "\C-crt" 'lyqi-rumor-set-tempo)
(define-key lyqi-mode-map "\C-crl" 'lyqi-rumor-set-legato)
(define-key lyqi-mode-map "\C-crd" 'lyqi-rumor-set-no-dots)
(define-key lyqi-mode-map "\C-crf" 'lyqi-rumor-set-flat)
(define-key lyqi-mode-map "\C-crm" 'lyqi-rumor-set-meter)
(define-key lyqi-mode-map "\C-crk" 'lyqi-rumor-set-key)
(define-key lyqi-mode-map "\C-crp" 'lyqi-rumor-set-alsa-port)
;; self inserting keys
(dolist (key (split-string lyqi-self-inserting-keys ""))
  (define-key lyqi-mode-map key 'self-insert-command))
(dolist (key (split-string lyqi-self-inserting-+-char-keys ""))
  (eval `(lyqi-make-self-insert-+-char ,key)))
(dolist (key-descr lyqi-self-inserting-+-string-keys)
  (eval `(lyqi-make-self-insert-+-string ,@key-descr)))
