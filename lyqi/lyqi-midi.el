;;	$RCSfile: lyqi-midi.el,v $	
;;	$Revision: 1.7 $	
;;	$Date: 2004/03/14 15:16:05 $	
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

(defcustom lyqi-midi-demon-command "timidity -iA -B2,8 -Os -EFreverb=0"
  "Command used to start the midi demon."
  :group 'lyqi
  :type  'string)

(defcustom lyqi-midi-keyboard-command "mymidikbd"
  "Command used to start the midi keyboard process."
  :group 'lyqi
  :type  'string)

(defcustom lyqi-midi-enabled-default nil
  "Automatically launch midi?"
  :group 'lyqi
  :type  'boolean)

(defcustom lyqi-midi-external-timidity-port 128
  "ALSA port of external timidity server."
  :group 'lyqi
  :type 'integer)

(defcustom lyqi-midi-use-external-timidity-server nil
  "If true, don't start a new timidity server, but use an existing
one, which port is `lyqi-midi-external-timidity-port'."
  :group 'lyqi
  :type  'boolean)

(defvar lyqi-midi-tempo 80
  "Tempo used for play back (quaters per minute).")

(defvar lyqi-midi-manually-off nil
  "Tells if user has previously switched off midi")

(defvar lyqi-midi-timidity nil
  "The timidity demon process.")

(defvar lyqi-midi-keyboard nil
  "The midi keyboard process.")

(defvar lyqi-midi-on nil
  "Say if midi processes are running")

(defun lyqi-midi-set-timidity-alsa-port (port)
  "When using an external timidity demon, set its ALSA sequencer port."
  (interactive "nTimidity ALSA port: ")
  (setf lyqi-midi-external-timidity-port port)
  (when lyqi-midi-timidity
    (setf (slot-value lyqi-midi-timidity 'seqport) port)))

(defun lyqi-midi-timidity-start ()
  (process-start lyqi-midi-timidity)
  (mapcar (lambda (client)
            (setf (slot-value client 'server-port)
                  (slot-value lyqi-midi-timidity 'seqport)))
          (list lyqi-midi-keyboard)))
          ;;(list lyqi-midi-keyboard lyqi-midi-rumor)))

(defun lyqi-midi-start ()
  "Starts the timidity process with ALSA interface
and the keyboard process."
  (unless lyqi-midi-on
    (lyqi-midi-timidity-start)
    (process-start lyqi-midi-keyboard))
  (setq lyqi-midi-on (and (process-runningp lyqi-midi-timidity)
                          (process-runningp lyqi-midi-keyboard))))

(defun lyqi-midi-stop ()
  "Stops timidity and keyboard processes."
  (process-stop lyqi-midi-timidity)
  (process-stop lyqi-midi-keyboard)
  (setq lyqi-midi-on nil))

(defmethod play-note ((note mudela-note) &optional short)
  "Play the given note, by sending its pitch and length (in sec) 
to the midi keyboard process."
  (when (process-runningp lyqi-midi-keyboard)
    (with-slots (duration dots) note
      (process-send-string
       (process-name (slot-value lyqi-midi-keyboard 'process))
       (format "%d %f\n" 
               (midi-pitch note)
               (if short
                   -1.0
                   (* (expt 2.0 (- 3 duration))
                      (do ((i 0 (1+ i))
                           (sum 0.0 (+ sum (expt 2.0 (- i)))))
                          ((> i dots) sum))
                      (/ 60.0 (* 1.0 lyqi-midi-tempo)))))))))

(defmethod play-note ((word mudela-word-duration) &optional short)
  "Play the given note, by sending its pitch and length (in sec)
to the midi keyboard process."
  (when (process-runningp lyqi-midi-keyboard)
    (with-slots (duration dots) word
      (process-send-string (process-name (slot-value lyqi-midi-keyboard 'process))
                           (format "%d %f\n" 
                                   -1
                                   (* (expt 2.0 (- 3 duration))
                                      (do ((i 0 (1+ i))
                                           (sum 0.0 (+ sum (expt 2.0 (- i)))))
                                          ((> i dots) sum))
                                      (/ 60.0 (* 1.0 lyqi-midi-tempo))))))))

(defmethod play-note ((word mudela-word) &optional short)
	   "Play the given note, by sending its pitch and length (in sec)
to the midi keyboard process."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass midi-process ()
  ((command :initarg :command
            :initform nil
            :documentation "process start command")
   (args    :initarg :args
            :initform nil
            :documentation "process command arguments")
   (name    :initarg :name
            :initform nil
            :documentation "process name")
   (process :initform nil
            :documentation "process object")))
(defmethod process-runningp ((process midi-process))
  "Return t if the process is running, nil otherwise."
  (with-slots ((proc process)) process
      (and proc (eq (process-status proc) 'run))))
(defmethod process-start ((process midi-process))
  "Start the process"
  (unless (process-runningp process)
    (with-slots (name command args) process
        (setf (slot-value process 'process)
              (apply 'start-process name name (append (split-string command " ")
                                                      args))))))
(defmethod process-stop ((process midi-process))
  "Stop the process"
  (when (process-runningp process)
    (with-slots ((proc process)) process
        (delete-process (process-name proc)))))

(defclass timidity-server (midi-process)
  ((seqport :initform nil
            :documentation "Timidity ALSA sequencer port")))
(defmethod process-start :AFTER ((timidity timidity-server))
  "Grep the sequencer port."
  ;; we have to wait a bit before reading timidity's output
  (sleep-for 1)
  (let ((port
         (with-current-buffer (buffer-name (process-buffer (slot-value timidity 'process)))
           (goto-char (point-max))
           (if (re-search-backward "Opening sequencer port: \\([0-9]+\\):" nil t)
               (buffer-substring (match-beginning 1) 
                                 (match-end 1))))))
    (when port
      (setf (slot-value timidity 'seqport) (string-to-int port)))))

(defclass external-timidity-server (midi-process)
  ((seqport ;:initform lyqi-midi-external-timidity-port
            :documentation "External timidity ALSA sequencer port")))
(defmethod process-runningp ((timidity external-timidity-server))
  t)
(defmethod process-start ((timidity external-timidity-server))
  t)
(defmethod process-stop ((timidity external-timidity-server))
  t)


(defclass timidity-client (midi-process)
  ((server-port :initform nil
                :documentation "The timidity server ALSA port")))

(defclass mymidikbd (timidity-client) nil)
(defmethod process-start :BEFORE ((kbd mymidikbd))
  "Update command argument list before execution."
  (setf (slot-value kbd 'args)
        (list (number-to-string (slot-value kbd 'server-port)))))

(provide 'lyqi-midi)

