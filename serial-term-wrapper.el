(defun serial-start-term (port speed)
(interactive (list (read-string "port: " "COM6")
      (read-string "speed: " "115200")))
(make-serial-process :port port :speed (string-to-int speed))
)

(defun serial-send-string (str)
(interactive (list (read-string "string to send: ")))
(process-send-string "COM6" str))

(defun serial-send-PTC-wrapped-string-4660 (str)
(interactive (list (read-string "string to wrap and send: ")))
(serial-send-string (concat "" str "")))
