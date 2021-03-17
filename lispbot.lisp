;;;; lispbot.lisp

(in-package #:lispbot)

(defvar *path-to-conf* "~/.config/lispbot/init.conf")

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *client* (wsd:make-client (second (get-file *path-to-conf*))))
(defvar *oauth* (first (get-file *path-to-conf*)))

(defparameter *target-channel* "#dario_satu")

(wsd:start-connection *client*)

(wsd:on :message *client*
	(lambda (msg)
	  (pong (string-trim '(#\Space #\Tab #\Newline ) (format nil "~A" msg)))
	  (let ((message (dispatch-msg (extract-msg msg))))
	    (dispatch-msg message))))

(wsd:on :close *client*
    (lambda (&key code reason)
      (format t "Closed because '~A' (Code=~A)~%" reason code)
      (close-con)))

(defun run-command (cmd)
  (let ((s (make-string-output-stream)))
    (isolated:read-eval-print (format nil "~A" cmd) s)
    (coerce (string-trim '(#\ #\newline) (get-output-stream-string s)) 'string)))

(defun dispatch-msg (msg)
  (ppcre:register-groups-bind (cmd p1 p2)
      ("(?:\!(\\w+))+(?:\\s(\\w+))?(?:\\s(.*))?" msg) ;magic regex: Thanks Oetzi <3
    (alexandria:switch (cmd :test #'string-equal)
      ("ayaya" (send :privmsg "AYAYA my dear friends!" *target-channel*))
      ("eval"  (let ((s (read-from-string p2)))
		 (send :privmsg (format nil "~A ~A" s (run-command s)) *target-channel*))))))

(defun extract-msg (msg)
  (pong msg)
  (multiple-value-bind (* message)
      (ppcre:scan-to-strings "PRIVMSG\\s\#\\w+\\s\:(.*)" msg) ;magic regex: Thanks Oetzi
    (when (> (length message) 0)
      (elt message 0)))) 

(defun send (type msg chan)
  (cond ((equal type :pong) (wsd:send *client* "PONG :tmi.twitch.tv"))
	((equal type :privmsg) (wsd:send *client* (format nil "PRIVMSG ~a :~A" chan msg)))
	(t :ok)))

(defun pong (msg)
  (alexandria:switch (msg :test #'string-equal)
    ("PING :tmi.twitch.tv" (progn
			       (format t "PONG")
			       (wsd:send *client* "PONG :tmi.twitch.tv")))))

(defun close-con ()
  (wsd:close-connection *client*))

(defun factorial (n)
  (alexandria:factorial n))

(defun connect-join-channel (nick pass chan)
  (wsd:send *client* (format nil "PASS ~a" pass))
  (wsd:send *client* (format nil "NICK ~a" nick))
  (wsd:send *client* (format nil "JOIN ~a" chan))
  (wsd:send *client* (format nil "PRIVMSG ~a :~A" chan (third (get-file *path-to-conf*)))))

(connect-join-channel "lispiboi" *oauth* *target-channel*)
