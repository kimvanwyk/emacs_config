;;	$RCSfile: lyqi-base.el,v $	
;;	$Revision: 1.5 $	
;;	$Date: 2003/09/27 16:59:27 $	
;;	$Author: nicolas $	
;;; 
;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2003 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/
;;;

(eval-when-compile (require 'cl))
(require 'eieio)

;;;;;;;;;;;;;;;;

(defconst mudela-translation-table
  '((pitch . ((nederlands . ["c" "d" "e" "f" "g" "a" "b"])
              (english    . ["c" "d" "e" "f" "g" "a" "b"])
              (deutsch    . ["c" "d" "e" "f" "g" "a" "h"])
              (norsk      . ["c" "d" "e" "f" "g" "a" "h"])
              (svenska    . ["c" "d" "e" "f" "g" "a" "h"])
              (italiano   . ["do" "re" "mi" "fa" "sol" "la" "si"])
              (catalan    . ["do" "re" "mi" "fa" "sol" "la" "si"])
              (espanol    . ["do" "re" "mi" "fa" "sol" "la" "si"])))
    (accidental . ((nederlands . ["eses" "es" "" "is" "isis"])
                   (english    . ["ff" "f" "" "s" "ss"])
                   (deutsch    . ["eses" "es" "" "is" "isis"])
                   (norsk      . ["essess" "ess" "" "iss" "ississ"])
                   (svenska    . ["essess" "ess" "" "iss" "ississ"])
                   (italiano   . ["bb" "b" "" "d" "dd"])
                   (catalan    . ["bb" "b" "" "d" "dd"])
                   (espanol    . ["bb" "b" "" "s" "ss"])))
    (replacements . ((nederlands . (("eeses" "eses") ("ees" "es")
                                    ("aeses" "ases") ("aes" "as")))
                     (deutsch    . (("hes" "b") ;("heses" "bes")
                                    ("eeses"  "eses") ("ees" "es")
                                    ("aeses" "ases") ("aes" "as")))
                     (norsk      . (("eessess" "essess") ("eess" "ess")
                                    ("hess" "b") ("hessess" "bess")))
                     (svenska    . (("eessess" "essess") ("eess" "ess")
                                    ("aessess" "assess") ("aess" "ass")
                                    ("hess" "b") ("hessess" "bess")))))
    (rest . "r")
    (skip . "s")
    (octave-up . ?')
    (octave-down . ?,)
    (dot . ?.)
    (reminder-accidental . ?!)
    (cautionary-accidental . ??)))

(defclass mudela-editing-state ()
  ((translation-table :initarg :translation-table
		      :initform nil
		      :documentation "A mudela string or character <---> internal value translation table.")
   (language :initarg :language
	     :initform nederlands
	     :documentation "Current output language")
   (relative-octave :initarg :relative-octave
		    :initform nil
		    :documentation "Current octave mode. relative if non-nil, absolute otherwise.")
   (force-duration :initarg :force-duration
		   :initform t
		   :documentation "Current duration output mode. always present if non-nil, ellipsed otherwise")
   (pitch-dict :initarg :pitch-dict
               :initform nil
               :documentation "A mudela<->internal values dictionnnary for pitches and alterations")
   (alterations :initarg :alterations
		:initform [2 2 2 2 2 2 2]
		:documenation "Last alteration for each note."))
  "The current editing state : octave mode, language, etc.")

;; (defmethod init-state ((editing-state mudela-editing-state) &optional lang)
;; 	   "Initialize `editing-state' pitch-dict due to `lang', 
;; and `translation-table' to `mudela-translation-table'."
;;   (setf (slot-value editing-state 'translation-table) mudela-translation-table)
;;   (with-slots (language) editing-state
;;     (set-language editing-state (or lang language)))
;;   editing-state)

(defmethod get-translation ((editing-state mudela-editing-state) key)
  "Return the value corresponding to `key' in `translation-table'"
  (with-slots (translation-table) editing-state
    (cdr (assoc key translation-table))))

(defmethod get-pitch ((editing-state mudela-editing-state) pitch-string)
  "Return a (pitch . alteration) pair corresponding to `pitch-string' in `pitch-dict' (if any)"
  (with-slots (pitch-dict) editing-state
    (car (rassoc pitch-string pitch-dict))))

(defmethod get-pitch-string ((editing-state mudela-editing-state) pitch alteration)
  "Return the mudela pitch string corresponding to the (`pitch' . `alteration') pair
in `pitch-dict' (if any)"
  (with-slots (pitch-dict) editing-state
    (cdr (assoc (cons pitch alteration) pitch-dict))))

(defmethod set-language ((editing-state mudela-editing-state) lang)
  "Set the editing-state object's language and update its pitch-dict accordingly."
  (labels ((get-translation2 (key)
			     (cdr (assoc lang (get-translation editing-state key)))))
    (setf (slot-value editing-state 'language) (if (stringp lang) (intern lang) lang))
    (setf (slot-value editing-state 'pitch-dict)
	  (let (dict)
	    (dotimes (pitch 7 (nreverse dict))
	      (dotimes (alter 5 dict)
		(setq dict 
		      (cons (cons (cons pitch alter) 
				  (let ((note (concat (aref (get-translation2 'pitch) pitch)
						      (aref (get-translation2 'accidental) alter))))
				    (or (cadr (assoc note (get-translation2 'replacements)))
					note)))
			    dict))))))
    lang))

;;;;;;;;;;;;;;;

(defclass mudela-note-state ()
  ((pitch :initarg :pitch
	  :initform 0
	  :documentation "Previous note pitch")
   (octave :initarg :octave
	   :initform 1
	   :documentation "Previous note octave")
   (duration :initarg :duration
	     :initform 3
	     :documentation "Previous music token duration")
   (dots :initarg :dots
	 :initform 0
	 :documentation "Previous music token dot number"))
  "Describe the current note state : current octave, duration, etc.")

;;;;;;;;;;;;;;;;

(defclass mudela-word ()
  ((editing-state :allocation :class
		  :documentation "The current editing state (language, relative/absolute octaves, etc),
used to generate mudela strings"))
  "Base class for mudela words: notes, rests, skips, etc.")

(defmethod mudela-string ((word mudela-word) &optional note-state)
  "(return an empty string. should be reimplemented by derived classes)"
  "")

(defmethod set-alteration-up ((word mudela-word))
  "Increase, if possible, the word alteration."
  nil)

(defmethod set-alteration-down ((word mudela-word))
  "Decrease, if possible, the word alteration."
  nil)

(defmethod set-alteration-natural ((word mudela-word))
  "Set, if possible, the word alteration to natural."
  nil)

(defmethod set-octave-up ((word mudela-word))
  "Increase the word's octave."
  nil)

(defmethod set-octave-down ((word mudela-word))
  "Decrease the word's octave."
  nil)

(defmethod set-octave-zero ((word mudela-word))
  "Set the note octave to zero."
  nil)

(defmethod set-duration ((word mudela-word) duration)
  "Set the word's duration."
  duration)

(defmethod set-dots ((word mudela-word))
  "Increase, modulo 5, the word's dot number."
  nil)

(defmethod set-reminder-alt ((word mudela-word))
  "Change reminder alteration state for word."
  nil)

(defmethod set-cautionary-alt ((word mudela-word))
  "Change cautionary alteration state for word."
  nil)

(defmethod transpose ((word mudela-word) note-diff exact-pitch-diff &optional note-state)
  "Transpose `word'."
  word)

(defclass mudela-word-duration (mudela-word)
  ((duration :initarg :duration
	     :initform 3               ; 2^(3 - 1) = 4 ==> quater note
	     :documentation "duration, from 1 to 8. real-duration = 2^(duration - 1)")
   (dots :initarg :dots
	 :initform 0                    ; no dot
	 :documentation "dots, from 0 (no dot) to N>0 (N dots)"))
  "A mudela word that have a duration information.")

(defmethod set-duration ((word mudela-word-duration) duration)
  "Set the word's duration."
  (setf (slot-value word 'dots) 0)
  (setf (slot-value word 'duration) duration))

(defmethod set-dots ((word mudela-word-duration))
  "Increase, modulo 5, the word's dot number."
  (setf (slot-value word 'dots)
	(mod (1+ (slot-value word 'dots)) 5)))

(defmethod update-note-state ((word mudela-word-duration) note-state)
  "Update the current `note-state' thanks to the given music `word': duration and dots."
  (with-slots (duration dots) word
    (setf (slot-value note-state 'duration) duration)
    (setf (slot-value note-state 'dots) dots))
  note-state)

(defmethod mudela-duration ((word mudela-word-duration) &optional note-state)
  "Return the mudela duration string for `word'. If `editing-state' 
indicates that duration is facultative, and `note-state' duration
and dots are the same that `word' duration and dots, the string is empty."
  (with-slots (duration dots editing-state) word
    (if (and (not (slot-value editing-state 'force-duration))
	     note-state
	     (= duration (slot-value note-state 'duration))
	     (= dots     (slot-value note-state 'dots)))
	;; same duration and dots, and user permit duration ellipse
	""
	(format "%d%s"
		(expt 2 (1- duration))
		(make-string dots (get-translation editing-state 'dot))))))

(defclass mudela-note (mudela-word-duration)
  ((pitch :initarg :pitch
	  :initform 0                   ; do / c
	  :documentation "note pitch, from 0 (do / c) to 6 (si / b)")
   (alteration :initarg :alteration
	       :initform 2              ; becarre / natural
	       :documentation "note alteration, from 0 (bb) to 4 (##)")
   (octave :initarg :octave
	   :initform 1
	   :documentation "note octave, 0 being the octave starting with the do / c 
which is in the 2nd interline in bass clef (4th line F clef)")
   (reminder-accidental :initarg :reminder-accidental
			:initform nil
			:documentation "if non-nil, force a reminder accidental")
   (cautionary-accidental :initarg :cautionary-accidental
			  :initform nil
			  :documentation "if non-nil and reminder-accidental is nil, 
indicate a cautionary accidental"))
  "Note : duration and pitch")
  
(defmethod set-alteration-natural ((note mudela-note))
  "Set notes's alteration to natural"
  ;; we update the alterations table in the current editing state
  (aset (slot-value (slot-value note 'editing-state) 'alterations)
        (slot-value note 'pitch)
        2)
  ;; reset reminder and cautionary slots
  (setf (slot-value note 'cautionary-accidental) nil)
  (setf (slot-value note 'reminder-accidental) nil)
  (setf (slot-value note 'alteration) 2))  

(defmethod set-alteration-up ((note mudela-note))
  "Increase, if possible, the note alteration."
  (with-slots (alteration) note
    (when (< alteration 4)
      ;; we update the alterations table in the current editing state
      (aset (slot-value (slot-value note 'editing-state) 'alterations)
	    (slot-value note 'pitch)
	    (1+ alteration))
      ;; reset reminder and cautionary slots
      (setf (slot-value note 'cautionary-accidental) nil)
      (setf (slot-value note 'reminder-accidental) nil)
      (setf (slot-value note 'alteration) (1+ alteration)))))

(defmethod set-alteration-down ((note mudela-note))
  "Decrease, if possible, the note alteration."
  (with-slots (alteration) note
    (when (> alteration 0)
      ;; we update the alterations table in the current editing state
      (aset (slot-value (slot-value note 'editing-state) 'alterations)
	    (slot-value note 'pitch)
	    (1- alteration))
      ;; reset reminder and cautionary slots
      (setf (slot-value note 'cautionary-accidental) nil)
      (setf (slot-value note 'reminder-accidental) nil)
      (setf (slot-value note 'alteration) (1- alteration)))))

(defmethod set-octave-up ((note mudela-note))
  "Increase the note's octave."
  (with-slots (octave) note
    (when (< octave 4)
      (setf (slot-value note 'octave) (1+ octave)))))

(defmethod set-octave-down ((note mudela-note))
  "Decrease the note's octave."
  (with-slots (octave) note
    (when (> octave -3)
      (setf (slot-value note 'octave) (1- octave)))))

(defmethod set-octave-zero ((note mudela-note))
  "Set the note octave to zero."
  (setf (slot-value note 'octave) 0))

(defmethod set-reminder-alt ((note mudela-note))
  "Change reminder alteration state for note."
  (with-slots (reminder-accidental) note
    (setf (slot-value note 'reminder-accidental) (not reminder-accidental))))

(defmethod set-cautionary-alt ((note mudela-note))
  "Change cautionary alteration state for note."
  (with-slots (cautionary-accidental) note
    (unless cautionary-accidental
      (setf (slot-value note 'reminder-accidental) nil))
    (setf (slot-value note 'cautionary-accidental) (not cautionary-accidental))))

(defmethod update-note-state ((note mudela-note) note-state)
  "Update the current `note-state' thanks to the given `note': duration, dots, pitch and octave."
  (call-next-method)
  (with-slots (pitch octave) note
    (setf (slot-value note-state 'pitch) pitch)
    (setf (slot-value note-state 'octave) octave))
  note-state)

(defmethod mudela-string ((note mudela-note) &optional note-state)
  "Return the mudela string for `note', depending on the context 
given by `editing-state' and `note-state': pitch, accidental, octave, 
duration (with dots), reminder or cautionary accidental."
  (format "%s%s%s%s"
	  (mudela-pitch note)
	  (mudela-octave note note-state)
	  (mudela-chromatic note)
	  (mudela-duration note note-state)))

(defmethod mudela-pitch ((note mudela-note))
  "Return the mudela pitch (with alteration) string for `note'"
  (with-slots (pitch alteration editing-state) note
    (get-pitch-string editing-state pitch alteration)))

(defmethod mudela-octave ((note mudela-note) &optional note-state)
  "Return the mudela octave string for `note'. In case of relative octave mode, 
`note-state' is mandatory."
  (with-slots (pitch octave editing-state) note
    (if (slot-value editing-state 'relative-octave)
	;; relative octave
	(when note-state
	  (let ((abspitch1 (+ (* 7 (slot-value note-state 'octave))
			      (slot-value note-state 'pitch)))
		(abspitch2 (+ (* 7 octave) pitch)))
	    (if (< (abs (- abspitch1 abspitch2)) 4)
		""                      ; same relative octave
		(if (> abspitch1 abspitch2)
		    (make-string (+ (/ (- abspitch1 abspitch2 4) 7) 1)
				 (get-translation editing-state 'octave-down))
		    (make-string (+ (/ (- abspitch2 abspitch1 4) 7) 1)
				 (get-translation editing-state 'octave-up))))))
	;; absolute octave
	(if (> octave 0)
	    (make-string octave (get-translation editing-state 'octave-up))
	    (make-string (* -1 octave) (get-translation editing-state 'octave-down))))))

(defmethod mudela-chromatic ((note mudela-note))
  "Return the mudela chromatic information string for `note'."
  (with-slots (reminder-accidental cautionary-accidental editing-state) note
    (cond (reminder-accidental 
	   (char-to-string (get-translation editing-state 'reminder-accidental)))
	  (cautionary-accidental
	   (char-to-string (get-translation editing-state 'cautionary-accidental)))
	  (t ""))))

(defmethod midi-pitch ((note mudela-note))
  "Return `note''s midi pitch, from 0 to 127."
  (with-slots (pitch alteration octave) note
    (+ (aref [0 2 4 5 7 9 11] pitch)
       (- alteration 2)
       (* octave 12)
       48)))

(defmethod transpose ((note mudela-note) note-diff exact-pitch-diff)
  "Transpose `note'. Ex: (transpose [do] -5 -9) -> [mib,]"
  (with-slots (pitch octave alteration) note
    (let ((newnote (copy-sequence note)))
      ;; pitch
      (setf (slot-value newnote 'pitch) (mod (+ pitch note-diff) 7))
      ;; octave
      (cond ((< (+ pitch note-diff) 0)
	     (setf (slot-value newnote 'octave) 
		   (+ octave (/ (+ pitch note-diff -6) 7))))
	    ((> (+ pitch note-diff) 6)
	     (setf (slot-value newnote 'octave) 
		   (+ octave (/ (+ pitch note-diff) 7)))))
      ;; alteration
      (setf (slot-value newnote 'alteration) 
	    (min (max (+ (- exact-pitch-diff (- (midi-pitch newnote) (midi-pitch note)))
			 alteration)
		      0)
		 4))
      newnote)))
    

(defclass mudela-rest (mudela-word-duration)
  nil
  "Rest.")

(defmethod mudela-string ((rest mudela-rest) &optional note-state)
  "Return the mudela string for `rest'."
  (with-slots (editing-state) rest
    (format "%s%s"
	    (get-translation editing-state 'rest)
	    (mudela-duration rest note-state))))

(defclass mudela-skip (mudela-word-duration)
  nil
  "Skip.")

(defmethod mudela-string ((skip mudela-skip) &optional note-state)
  "Return the mudela string for `skip'."
  (with-slots (editing-state) skip
    (format "%s%s"
	    (get-translation editing-state 'skip)
	    (mudela-duration skip note-state))))

(defclass mudela-verbatim (mudela-word)
  ((text :initarg :text
	 :initform ""
	 :documentation "Verbatim mudela text, storing not recognized mudela words"))
  "Not recognized text")

(defmethod update-note-state ((verbatim mudela-verbatim) note-state)
  "(do nothing)"
  note-state)

(defmethod mudela-string ((verbatim mudela-verbatim) &optional note-state)
  "Return the verbatim mudela string contained in this object"
  (with-slots (text) verbatim
    text))

(provide 'lyqi-base)
