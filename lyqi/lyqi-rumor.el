;;	$RCSfile: lyqi-rumor.el,v $	
;;	$Revision: 1.2 $	
;;	$Date: 2004/03/14 15:15:24 $	
;;	$Author: nicolas $	
;;; 
;;; Part of lyqi, a major emacs mode derived from LilyPond-Mode,
;;; for quick note insertion while editing GNU LilyPond music scores.
;;; 
;;; (c) copyright 2003 Nicolas Sceaux <nicolas.sceaux@free.fr>
;;; See http://nicolas.sceaux.free.fr/lilypond/
;;;	

(require 'eieio)
(require 'lyqi-base)
(require 'lyqi-midi)

(defcustom lyqi-rumor-command "rumor"
  "Command used to start the rumor process."
  :group 'lyqi
  :type  'string)

;;; rumor options
(defcustom lyqi-rumor-default-grain 16
  "Set minimum time resolution to the NUMth note. Only powers of two are valid, from 1 up to 128."
  :group 'lyqi
  :type  'number)

(defcustom lyqi-rumor-default-tempo 80
  "Metronome speed in beats per minute."
  :group 'lyqi
  :type  'number)

(defcustom lyqi-rumor-default-legato nil
  "If true, ignore any rests between notes."
  :group 'lyqi
  :type  'boolean)

(defcustom lyqi-rumor-default-no-dots nil
  "If true, do not use dotted notes."
  :group 'lyqi
  :type  'boolean)

(defcustom lyqi-rumor-default-flat nil
  "If true, output only pitches as you play, no lengths."
  :group 'lyqi
  :type  'boolean)

(defcustom lyqi-rumor-default-strip t
  "If true, strip leading and trailing rests from output."
  :group 'lyqi
  :type  'boolean)

;; the two following are obiously score-dependant, not really
;; custom vars. However, with defcustom, they can be set thanks 
;; to set-variable.
(defcustom lyqi-rumor-default-meter "4/4"
  "P/Q. Set time signature. Bar will have P beats of duration of the Qth note. Q must be a power of two."
  :group 'lyqi
  :type  'string)

(defcustom lyqi-rumor-default-key "c"
  "Set base note of current scale.
Valid values for KEY are ces, c, cis, des, .... Double sharps/flats are not allowed.
Note that KEY has to be given using note language `nederlands'."
  :group 'lyqi
  :type  'string)

(defcustom lyqi-rumor-default-alsa-port 64
  "rumor ALSA port"
  :group 'lyqi
  :type  'number)

(defvar lyqi-rumor-process nil
  "The rumor process.")

(defclass rumor (midi-process)
  ((grain   :initarg :grain)
   (tempo   :initarg :tempo)
   (legato  :initarg :legato)
   (no-dots :initarg :no-dots)
   (flat    :initarg :flat)
   (strip   :initarg :strip)
   (meter   :initarg :meter)
   (key     :initarg :key)
   (port    :initarg :alsa-port)))

(defmethod process-start :BEFORE ((rumor rumor))
  "Start a rumor recording session"
  (with-slots (grain tempo legato no-dots flat strip meter key port) rumor
    (setf (slot-value rumor 'args)
          (list "2>/dev/null"
                (format "--meter=%s" meter)
                (format "--tempo=%d" tempo)
                (format "--grain=%d" grain)
                (format "--key=%s"   key)
                (format "--alsa=%d:0,%d:0" port port)))
    (when legato
      (push "--legato" (slot-value rumor 'args)))
    (when no-dots
      (push "--no-dots" (slot-value rumor 'args)))
    (when flat
      (push "--flat" (slot-value rumor 'args)))
    (when strip
      (push "--strip" (slot-value rumor 'args)))
    (push (format "--lang=%s" (case (slot-value lyqi-editing-state 'language)
                                (nederlands "ne")
                                (english "en-short")
                                (deutsch "de")
                                (norsk "no")
                                (svenska "sv")
                                (italiano "it")
                                (catalan "ca")
                                (espanol"es")))
          (slot-value rumor 'args))
    (when (slot-value lyqi-editing-state 'force-duration)
      (push "--explicit-duration" (slot-value rumor 'args)))
    (unless (slot-value lyqi-editing-state 'relative-octave)
      (push "--absolute-pitches" (slot-value rumor 'args)))
    (push "--no-chords" (slot-value rumor 'args))
    (setf (slot-value rumor 'args) (nreverse (slot-value rumor 'args)))))

(defmethod process-start :AFTER ((rumor rumor))
  (set-process-filter (slot-value rumor 'process) 'rumor-filter))

(defun rumor-filter (process output)
  "Process Filter Function for rumor.
 Just insert rumor output in current buffer."
  (insert output)
  (accept-process-output process 0.3 0))

(provide 'lyqi-rumor)

