;;	$RCSfile: lyqi-parser.el,v $	
;;	$Revision: 1.5 $	
;;	$Date: 2003/09/27 16:33:49 $	
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
(require 'lyqi-base)

;;;;;; few string utilities ;;;;;;;;;;

;; for XEmacs21 compatibility
(if (not (fboundp 'match-string-no-properties))
    (defalias 'match-string-no-properties 'match-string))

(defun my-join (string-list joiner)
  "Returns a concatenation of all string-list elements, with joiner between elements"
  (apply 'concat 
	 (car string-list) 
	 (mapcar (lambda (str) (concat joiner str))
		 (cdr string-list))))

(defun my-sort-string-by-length (string-list)
  "Sort the given string list by decreasing string length."
  (nreverse 
   (sort string-list
	 (lambda (str1 str2)
	   (or (< (length str1) (length str2))
	       (and (= (length str1) (length str2))
		    (string< str1 str2)))))))

(defun my-char-to-restring (char)
  (concat (if (member char '(?. ?* ?+ ??)) "\\" "")
	  (char-to-string char)))

;;;;;; parser classes ;;;;;;;;;

(defclass base-parser ()
  ((regexp :initarg :regexp
	   :documentation "A regular expression associated with the parser"))
  "base class for parsers")

(defmethod update-regexp ((parser base-parser))
	   "(do nothing) Return the parser's regexp"
  (slot-value parser 'regexp))

(defclass syllab-parser (base-parser)
  nil
  "A syllab parser, ie a specialized word component parser.")

(defclass word-parser (base-parser)
  ((regexp-not-before :initarg :not-before
		      :initform nil
		      :documentation "A regexp describing what should not be found after the words
that the parser recognize.")
   (regexp-not-after  :initarg :not-after
		      :initform nil
		      :documentation "A regexp describing what should not be found before the words
that the parser recognize.")
   (syllab-parsers :initarg :syllab-parsers
		   :initform nil
		   :documentation "A list of (syllab-parser . facultative) pair, where `syllab-parser'
is a syllab-parser object, and facultative says if the syylab if facultative in the word.")
   (word-class :initarg :word-class
	       :type symbol
	       :documentation "The class name of the recognized words."))
  "A word parser, which aims at building a given type of objects when parsing a recognized word.
A word is composed of syllabs, and thus a word-parser is composed of syllab-parsers.")

(defmethod update-regexp ((parser word-parser))
	   "Update the parser's regexp thanks to its syllabe-parsers regexps.
Return the new regexp."
  (with-slots (syllab-parsers) parser
    (setf (slot-value parser 'regexp) 
	  (apply 'concat "\\b" (mapcar (lambda (sylparser)
                                         (format (if (cdr sylparser) "\\(%s\\)?" "%s") 
                                                 (update-regexp (car sylparser))))
                                       syllab-parsers)))))

(defclass text-parser (base-parser)
  ((word-parsers :initarg :word-parsers
		 :initform nil
		 :documentation "List of word-parser objects used to parse a text. Order matters.")
   (class-unknown :initarg :class-unknown
		  :type symbol
		  :documentation "Class name used to store not recognized text.")
   (slot-unknown :initarg :slot-unknown
		 :type symbol
		 :documentation "Slot name of class-unknown where the not recognized text will be stored"))
  "A text parser, which aims at building an object list, each object being built when a word is recognized.
A text is composed of words, and thus a text-parser is composed of word-parsers. When a piece of text is not
recognized by word-parsers, it is stored in the slot `slot-unknown' of an object of class `class-unknown'.")

(defmethod update-regexp ((parser text-parser))
	   "Update the text-parser's regexp thanks to its word-parsers regexps.
Return the new regexp."
  (with-slots (word-parsers) parser
    (setf (slot-value parser 'regexp)
	  (format "\\(%s\\)" (my-join (mapcar (lambda (word-parser)
						(format "\\(%s\\)" (update-regexp word-parser)))
					      word-parsers) 
				      "\\|")))))

;;;;;;;; mudela parser ;;;;;;;;;;;;;
;; no multiple inheritance with eieio... can not define a base class that has an editing-state slot

(defclass mudela-syllab-parser (syllab-parser)
  ((editing-state :initarg :editing-state
		  :documentation "The current editing state (language, relative/absolute octaves, etc),
used to generate mudela regexps"))
  "A syllab parser specialized for mudela.")

(defmethod parse-string ((parser mudela-syllab-parser) mudela-str note-state)
	   "do nothing"
  nil)

(defclass duration-parser (mudela-syllab-parser)
  nil
  "A duration and dots parser.")

(defmethod init-parser ((parser duration-parser) editing-state)
	   "Init the parser regexp"
  (setf (slot-value parser 'editing-state) editing-state)
  (setf (slot-value parser 'regexp)
	(format "\\(%s\\)\\(%s*\\)"
		(my-join (my-sort-string-by-length 
			  (mapcar 'int-to-string
				  (mapcar (lambda (n) (expt 2 (- n 1)))
					  '(1 2 3 4 5 6 7 8))))
			 "\\|")
		(my-char-to-restring (get-translation editing-state 'dot))))
  parser)

(defmethod parse-string ((parser duration-parser) mudela-str note-state)
	   "If `parser' regexp matches `mudela-str', return slot initialization description,
ie (:duration N :dots P), with N and P read from `mudela-str'. Otherwise, a default definition
is generated thanks to `note-state'."
  (with-slots (regexp) parser
    (if (string-match regexp mudela-str)
      (list :duration (round (1+ (log (string-to-number (match-string-no-properties 1 mudela-str)) 2)))
	    :dots (length (match-string-no-properties 2 mudela-str)))
      (with-slots (duration dots) note-state
	(list :duration duration :dots dots)))))

(defclass pitch-parser (mudela-syllab-parser)
  nil
  "A pitch, alteration and octave parser.")

(defmethod update-regexp ((parser pitch-parser))
	   "Update the parser's regexp and return it"
  (with-slots (editing-state) parser
    (setf (slot-value parser 'regexp)
	  (format "\\(%s\\)\\(%s+\\|%s+\\)?" 
		  (my-join (my-sort-string-by-length (mapcar 'cdr (slot-value editing-state 'pitch-dict)))
			   "\\|")
		  (my-char-to-restring (get-translation editing-state 'octave-down))
		  (my-char-to-restring (get-translation editing-state 'octave-up))))))

(defmethod init-parser ((parser pitch-parser) editing-state)
	   "Init the parser regexp"
  (setf (slot-value parser 'editing-state) editing-state)
  (update-regexp parser)
  parser)

(defmethod parse-string ((parser pitch-parser) mudela-str note-state)
	   "If `parser' regexp matches `mudela-str', return slot initialization description,
ie (:pitch N :alteration P :octave Q), with N, P and Q read from `mudela-str'."
  (with-slots (regexp editing-state) parser
    (when (string-match regexp mudela-str)
      (let* ((pitch-alter (get-pitch editing-state (match-string-no-properties 1 mudela-str)))
	     (pitch (car pitch-alter))
	     (alter (cdr pitch-alter)))
	(list :pitch pitch
	      :alteration alter
	      :octave (+ 0 
			 (if (slot-value editing-state 'relative-octave)
			     (+ (slot-value note-state 'octave)
				(cond ((> (- pitch (slot-value note-state 'pitch)) 3) -1)
				      ((> (- (slot-value note-state 'pitch) pitch) 3) 1)
				      (t 0)))
			     0)
			 (if (match-string-no-properties 2 mudela-str)
			     (* (if (string= (char-to-string (get-translation editing-state 'octave-down))
					     (substring (match-string-no-properties 2 mudela-str) 0 1))
				    -1 
				    1)
				(length (match-string-no-properties 2 mudela-str)))
			     0)))))))

(defclass chromatic-parser (mudela-syllab-parser)
  nil
  "A chromatic information parser.")

(defmethod init-parser ((parser chromatic-parser) editing-state)
	   "Init the parser regexp"
  (setf (slot-value parser 'editing-state) editing-state)
  (setf (slot-value parser 'regexp)
	(format "\\(%s\\|%s\\)"
		(my-char-to-restring (get-translation editing-state 'reminder-accidental))
		(my-char-to-restring (get-translation editing-state 'cautionary-accidental))))
  parser)

(defmethod parse-string ((parser chromatic-parser) mudela-str note-state)
	   "If `parser' regexp matches `mudela-str', return slot initialization description,
ie (:reminder-accidental N :cautionary-accidental P, with N and P read from `mudela-str'."
  (with-slots (regexp editing-state) parser
    (if (string-match regexp mudela-str)
	(list :reminder-accidental (string= (char-to-string (get-translation editing-state 'reminder-accidental))
					    (substring (match-string-no-properties 0 mudela-str) 0 1))
	      :cautionary-accidental (string= (char-to-string (get-translation editing-state 'cautionary-accidental))
					      (substring (match-string-no-properties 0 mudela-str) 0 1)))
	(list :reminder-accidental nil :cautionary-accidental nil))))

(defclass r-parser (mudela-syllab-parser)
  nil
  "A r (rest) parser.")

(defmethod init-parser ((parser r-parser) editing-state)
	   "Init the parser regexp"
  (setf (slot-value parser 'editing-state) editing-state)
  (setf (slot-value parser 'regexp) 
	(format "\\(%s\\|%s\\)"
		(get-translation editing-state 'rest)
		(upcase (get-translation editing-state 'rest))))
  parser)

(defclass s-parser (mudela-syllab-parser)
  ((editing-state :initarg :editing-state
		  :documentation "The current editing state (language, relative/absolute octaves, etc),
used to generate mudela regexps"))
  "A s (skip) parser.")

(defmethod init-parser ((parser s-parser) editing-state)
	   "Init the parser regexp"
  (setf (slot-value parser 'editing-state) editing-state)
  (setf (slot-value parser 'regexp) (get-translation editing-state 'skip))
  parser)

(defclass mudela-word-parser (word-parser)
  ((editing-state :initarg :editing-state
		  :documentation "The current editing state (language, relative/absolute octaves, etc),
used to generate mudela regexps"))
  "A specialized mudella word parser.")

(defmethod init-parser ((parser mudela-word-parser) editing-state)
	   "Init the parser regexp"
  (setf (slot-value parser 'editing-state) editing-state)
  (with-slots (syllab-parsers) parser
    (dolist (sylparser syllab-parsers)
      (init-parser (car sylparser) editing-state)))
  (update-regexp parser)
  parser)

(defmethod parse-string ((parser mudela-word-parser) mudela-str note-state &optional before after)
	   "If the parser regexp matches `mudela-str' exactly and if `regexp-not-after' and
`regexp-not-before' do not match the strings `before' or `after', return an instance
of `word-class' by parsing `mudela-str'."
  (with-slots (word-class syllab-parsers regexp regexp-not-after regexp-not-before) parser
    (when (and (string-match regexp mudela-str)
	       (string= (match-string-no-properties 0 mudela-str) mudela-str)
	       (not (and before (string-match regexp-not-after before)))
	       (not (and after (string-match regexp-not-before after))))
      (apply 'make-instance word-class (apply 'append 
					      (remove-if 'null (mapcar (lambda (syl-parser)
									 (parse-string (car syl-parser) mudela-str note-state))
								       syllab-parsers)))))))

(defclass mudela-parser (text-parser)
  ((editing-state :initarg :editing-state
		  :documentation "The current editing state (language, relative/absolute octaves, etc),
used to generate mudela regexps"))
  "A simple mudela parser, that can read notes, rests and skips")

(defmethod init-parser ((parser mudela-parser) editing-state)
	   "Initialize `parser' : editing-state, syllab and word parsers, etc."
  (setf (slot-value parser 'editing-state) editing-state)
  (with-slots (word-parsers) parser
    (dolist (word-pars word-parsers)
      (init-parser word-pars editing-state)))
  (update-regexp parser)
  parser)

(defmethod parse-string ((parser mudela-parser) mudela-str note-state &optional before after)
	   "If `mudela-str' is exactly recognized as a known word, return an object 
corresponding to that word. Otherwise, return nil."
  (with-slots (word-parsers) parser
    (do* ((wparsers word-parsers (cdr wparsers))
	  (wparser (car wparsers) (car wparsers))
	  obj)
	((or obj (not wparser)) obj)
      (setq obj (parse-string wparser mudela-str note-state before after)))))

(defmethod get-word ((parser mudela-parser) note-state &optional backward limit)
	   "Return a (word beginning end) list, `word' being the first mudela-word
found after (if `backward' is nil) or before (otherwise) point, `beginning' and
`end' being its beginning and end points. If no such word is found, 
 (nil beginning end) is returned, `beginning' and `end' being the parsed region.
The position is preserved."
  (save-excursion
    (with-slots (regexp) parser
      (let (word
	    (beginning (and (not backward) (point)))
	    (end (and backward (point))))
	(while (and (not word)
		    (if backward
			(re-search-backward regexp limit t)
			(re-search-forward regexp limit t)))
	  (let* ((b (match-beginning 0))
		 (e (match-end 0))
		 (point-before (and (> b (point-min)) (1- b)))
		 (point-after  (and (< e (point-max)) e))
		 (str-before   (and point-before (buffer-substring-no-properties point-before (1+ point-before))))
		 (str-after    (and point-after (buffer-substring-no-properties point-after (1+ point-after))))
		 (token (parse-string parser (match-string-no-properties 0) note-state str-before str-after)))
	    (if token
		(setq word token
		      beginning b
		      end e)
		(goto-char (if backward (1- e) (1+ b))))))
	(list word 
	      (if backward (or beginning limit) beginning)
	      (if backward end (or end limit)))))))

(defmethod parse-region ((parser mudela-parser) beginning end)
	   "Return an object list describing what as been read by `parser' in the 
region delimited by `beginning' and `end'."
  (with-slots (class-unknown slot-unknown) parser
    (labels ((make-verbatim (text)
			    (let ((obj (make-instance class-unknown)))
			      (setf (slot-value obj slot-unknown) text)
			      obj)))
      (save-excursion 
	(let ((start-verb beginning)
	      tokens
	      (note-state (make-instance 'mudela-note-state)))
	  (goto-char beginning)
	  (do ((word-descr (get-word parser note-state nil end)
			   (get-word parser note-state nil end)))
	      ((or (null (car word-descr)) (>= (point) end)))
	    ;; first, we push verbatim text in tokens
	    (push (make-verbatim (buffer-substring-no-properties start-verb (cadr word-descr))) tokens)
	    ;; then, the recognized word
	    (push (car word-descr) tokens)
	    ;; finally, update position
	    (goto-char (caddr word-descr))
	    (setq start-verb (point))
	    (setf note-state (update-note-state (car word-descr) note-state)))
	  ;; remaining verbatim text
	  (when (< start-verb end)
	    (push (make-verbatim (buffer-substring-no-properties start-verb end)) tokens))
	  (nreverse tokens))))))


(defun make-mudela-parser (editing-state)
  "Build and initialize a simple mudela parser."
  (let* (;; syllab-parsers
	 (duration-pars (make-instance 'duration-parser))
	 (pitch-pars (make-instance 'pitch-parser))
	 (chromatic-pars (make-instance 'chromatic-parser))
	 (r-pars (make-instance 'r-parser))
	 (s-pars (make-instance 's-parser))
	 ;; word-parsers
	 (note-pars (make-instance 'mudela-word-parser
				   :not-before "[a-zA-Z]"
				   :not-after "[a-zA-Z\\\\]"
				   :word-class 'mudela-note
				   :syllab-parsers (list (cons pitch-pars nil)
							 (cons chromatic-pars t)
							 (cons duration-pars t))))
	 (rest-pars (make-instance 'mudela-word-parser
				   :not-before "[a-zA-Z]"
				   :not-after "[a-zA-Z\\\\]"
				   :word-class 'mudela-rest
				   :syllab-parsers (list (cons r-pars nil)
							 (cons duration-pars t))))
	 (skip-pars (make-instance 'mudela-word-parser
				   :not-before "[a-zA-Z]"
				   :not-after "[a-zA-Z\\\\]"
				   :word-class 'mudela-skip
				   :syllab-parsers (list (cons s-pars nil)
							 (cons duration-pars t))))
	 ;; text-parser
	 (mudela-pars (make-instance 'mudela-parser
				    :class-unknown 'mudela-verbatim
				    :slot-unknown 'text
				    :word-parsers (list note-pars
							rest-pars
							skip-pars))))
    (init-parser mudela-pars editing-state)
    mudela-pars))

(provide 'lyqi-parser)
