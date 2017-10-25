;;	$RCSfile: lyqi-editor.el,v $	
;;	$Revision: 1.6 $	
;;	$Date: 2004/03/14 15:15:54 $	
;;	$Author: nicolas $	
;;; 
;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) 2003 copyright Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/
;;;	

(eval-when-compile (require 'cl))
(require 'eieio)
(require 'lyqi-base)
(require 'lyqi-parser)

(unless (fboundp 'object-of-class-p)
  (defun object-of-class-p (obj class)
    (obj-of-class-p obj class)))

(defun lyqi-just-one-space ()
  "Invoke `just-one-space', unless point is at the beginning of a line."
  (unless (bolp)
    (just-one-space)))

(defclass mudela-editor ()
  ((editing-state :initarg :editing-state
		  :documentation "The current editing state (language, relative/absolute octaves, etc),
used to generate mudela regexps")
   (parser :initarg :parser
	   :documentation "A mudela-parser instance used to read buffer.")
   (note-state :initarg :note-state
	       :documentation "Current note state."))
  "A mudela parser: basic mudela edition, such as insert/deleting/updating notes.")

(defmethod point-note-state ((editor mudela-editor))
  "Return a note state deduced thanks to words preceding point."
  (with-slots (parser note-state) editor
    (let* ((fake-note-state (make-instance 'mudela-note-state :duration nil :dots 0))
	   (prev-duration-word ;; the previous word with explicit duration
	    (save-excursion
	      (do ((word-descr (get-word parser fake-note-state t)
			       (get-word parser fake-note-state t)))
		  ((or (null (car word-descr))
		       (<= (point) (point-min))))
		(when (and (object-of-class-p (car word-descr) 'mudela-word-duration)
			   (slot-value (car word-descr) 'duration))
		  (return (car word-descr)))
		(goto-char (cadr word-descr)))))
	   (duration ;; duration of that duration word
	    (if prev-duration-word
		(slot-value prev-duration-word 'duration)
		(slot-value note-state 'duration)))
	   (dots ;; number of dots in that duration word
	    (if prev-duration-word
		(slot-value prev-duration-word 'dots)
		(slot-value note-state 'dots)))
	   (prev-note ;; the previous note
	    (save-excursion
	      (do ((word-descr (get-word parser note-state t)
			       (get-word parser note-state t)))
		  ((or (null (car word-descr))
		       (<= (point) (point-min))))
		(when (mudela-note-p (car word-descr))
		  (return (car word-descr)))
		(goto-char (cadr word-descr)))))
	   (pitch ;; the pitch of that note
	    (if prev-note
		(slot-value prev-note 'pitch)
		(slot-value note-state 'pitch)))
	   (octave ;; the octave of that note
	    (if (and prev-note (not (slot-value (slot-value parser 'editing-state) 
						'relative-octave)))
		(slot-value prev-note 'octave)
		(slot-value note-state 'octave))))
      (make-instance 'mudela-note-state
		     :pitch pitch
		     :octave octave
		     :duration duration
		     :dots dots))))

(defmethod delete-word ((editor mudela-editor) &optional (backward t))
  "Delete the first recognized word after (if `backward' is nil)
or before (otherwise) point (if any)."
  (with-slots (parser note-state) editor
    (destructuring-bind (word beginning end) (get-word parser note-state t)
      (when word
	(delete-region beginning end)
	(goto-char beginning)
	(lyqi-just-one-space)
	(backward-char)))))

(defmethod make-note ((editor mudela-editor) pitch)
  "Make a new note, of pitch `pitch', which octave and duration are taken
from `editor''s note-state slot, and alteration taken from editing-state alterations 
slot."
  (setf (slot-value editor 'note-state) (point-note-state editor))
  (with-slots (note-state editing-state) editor
    (with-slots ((pitch0 pitch) (octave0 octave) (duration0 duration) (dots0 dots)) note-state
      (make-instance 'mudela-note :pitch pitch
		     :alteration  (aref (slot-value editing-state 'alterations) pitch)
		     :duration duration0 :dots dots0
		     :octave (cond ((> (- pitch pitch0) 3) (1- octave0))
				   ((> (- pitch0 pitch) 3) (1+ octave0))
				   (t octave0))))))

(defmethod make-rest ((editor mudela-editor))
  "Make a new rest, which duration is taken from `editor''s note-state slot."
  (setf (slot-value editor 'note-state) (point-note-state editor))
  (with-slots (note-state) editor
    (with-slots ((duration0 duration) (dots0 dots)) note-state
      (make-instance 'mudela-rest :duration duration0 :dots dots0))))

(defmethod make-skip ((editor mudela-editor))
  "Make a new rest, which duration is taken from `editor''s note-state slot."
  (setf (slot-value editor 'note-state) (point-note-state editor))
  (with-slots (note-state) editor
    (with-slots ((duration0 duration) (dots0 dots)) note-state
      (make-instance 'mudela-skip :duration duration0 :dots dots0))))

(defmethod word-insert ((editor mudela-editor) word)
  "Insert the word's mudela string at current point, and updates
editor's note-state."
  (setf (slot-value editor 'note-state) (point-note-state editor))
  (with-slots (note-state) editor
    (lyqi-just-one-space)
    (insert (mudela-string word note-state))
    (lyqi-just-one-space)
    (indent-for-tab-command) ;; TODO: be softer.
    word))

(defmethod search-word ((editor mudela-editor) &optional backward)
  "Return a (word note-state beginning end) list, `word' being a 
mudela-word representation of the first word after (if `backward is nil) 
or before (otherwise) point, `note-state' the note-state deduced before that
word, `beginning' and `end' being the beginning and end position of the word."
  (with-slots (parser note-state) editor
    (destructuring-bind (tmpword beginning end) (get-word parser note-state backward)
      (cond (tmpword
	     (goto-char beginning)
	     (let ((new-note-state (point-note-state editor)))
	       (list (parse-string parser
				   (buffer-substring-no-properties beginning end)
				   new-note-state)
		     new-note-state
		     beginning
		     end)))
	    (t (list nil nil 0 0))))))

(defmacro with-word-update (editor word-symbol &rest body)
  "Read last word / update / delete / re-write word facility.
The word preceding point will be read by `editor', and will
be modified in `body', by refereing it as `word-symbol' (a
non quoted symbol). Then, word will be deleted in the buffer, 
and a new string, reflecting changes appareing in `body', 
will be inserted."
  (let ((beginning (gensym))
	(end (gensym))
	(note-state-at-point (gensym)))
    `(destructuring-bind (,word-symbol ,note-state-at-point ,beginning ,end) (search-word ,editor t)
       (when ,word-symbol
	 ,@body
	 (goto-char ,beginning)
	 (delete-region ,beginning ,end)
	 (word-insert ,editor ,word-symbol)
	 ,word-symbol))))

(defmethod transpose-region ((editor mudela-editor) note-diff exact-pitch-diff beginning end)
  "Transpose notes in current region."
  (with-slots (parser note-state) editor
    (goto-char beginning)
    (setf (slot-value editor 'note-state) (point-note-state editor))
    (let ((word-list (parse-region parser beginning end)))
      (delete-region beginning end)
      (dolist (word word-list)
	(let ((transp-word (transpose word note-diff exact-pitch-diff)))
	  (word-insert editor transp-word))))))

(defmethod change-octave-mode-region ((editor mudela-editor) beginning end)
  "Switch octave mode for notes included between `beginning' and `end'"
  (with-slots (parser note-state editing-state) editor
    (goto-char beginning)
    (setf (slot-value editor 'note-state) (point-note-state editor))
    (let ((word-list (parse-region parser beginning end)))
      (delete-region beginning end)
      ;; we switch editing-state's octave mode
      (setf (slot-value editing-state 'relative-octave)
	    (not (slot-value editing-state 'relative-octave)))
      (dolist (word word-list)
	(word-insert editor word)))))

(defmethod change-language-region ((editor mudela-editor) from-lang to-lang beginning end)
  "Change language for notes included between `beginning' and `end',
from `from-lang' to `to-lang' (two symbols)."
  (with-slots (parser note-state editing-state) editor
    (goto-char beginning)
    (setf (slot-value editor 'note-state) (point-note-state editor))
    ;; first, read the region with from-lang language
    (when (not (equal (slot-value editing-state 'language) from-lang))
      (set-language editing-state from-lang)
      (update-regexp parser))
    (let ((word-list (parse-region parser beginning end)))
      (delete-region beginning end)
      ;; then, we change language from writing
      (set-language editing-state to-lang)
      (update-regexp parser)
      (dolist (word word-list)
	(word-insert editor word)))))

(provide 'lyqi-editor)
