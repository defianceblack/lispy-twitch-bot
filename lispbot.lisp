;;;; lispbot.lisp

(in-package #:lispbot)

(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defparameter *path-to-conf* "~/.config/lispbot/init.conf")
(defparameter *docker* nil)
(defparameter *target-channel* nil)
(defparameter *client* nil)
(defparameter *bot-name* nil)
(defvar *oauth* (first (get-file *path-to-conf*)))

(defun start-connection ()
  "Starts the connection and assigns hooks to it."
  (wsd:start-connection *client*)
  
  (wsd:on :message *client*
	  (lambda (msg)
	    (when (not (find #\; msg :test #'equal))
	      (pong (string-trim '(#\Space #\Tab #\Newline ) (format nil "~A" msg)))
	      (let ((message (dispatch-msg (extract-msg msg))))
		(dispatch-msg message)))))

  (wsd:on :close *client*
	  (lambda (&key code reason)
	    (format t "Closed because '~A' (Code=~A)~%" reason code)
	    (close-connection))))

(defun close-connection ()
  "Closes the connection"
  (wsd:close-connection *client*))

(defun run-command (cmd)
  "Executes a given command via a 'hopefully' secure subset of common lisp."
  (let ((s (make-string-output-stream)))
    (isolated:read-eval-print (format nil "~A" cmd) s)
    (coerce (string-trim '(#\ #\newline) (get-output-stream-string s)) 'string)))

(defun run-docker (cmd)
  "Executes a given command via docker encapsulation."
  (string-trim '(#\newline #\space) (uiop:run-program
				     (list "docker"
					   "run"
					   *docker*
					   "sbcl"
					   "--noinform"
					   "--noprint"
					   "--eval" (format nil "(print ~A)" cmd))
				     :force-shell nil
				     :output :string)))

(defun dispatch-msg (msg)
  "Checks for !<command> if you want to add commands, you just have to add them here. cmd is the command, p1 is the first parameter and p2 are all the other parameters."
  (ppcre:register-groups-bind (cmd p1 p2)
      ("(?:\!(\\w+))+(?:\\s(\\w+))?(?:\\s(.*))?" msg) ;magic regex: Thanks Oetzi <3
    (alexandria:switch (cmd :test #'string-equal)
      ("ayaya" (send :privmsg "AYAYA my dear friends!" *target-channel*))
      ("eval"  (let ((s (read-from-string p2)))
		 (send :privmsg (format nil "~A => ~A" s (run-docker s)) *target-channel*))))))

(defun extract-msg (msg)
  "Checks if twitch requires a PONG, then extracts the message via regex."
  (pong msg)
  (multiple-value-bind (* message)
      (ppcre:scan-to-strings "PRIVMSG\\s\#\\w+\\s\:(.*)" msg) ;magic regex: Thanks Oetzi
    (when (> (length message) 0)
      (elt message 0)))) 

(defun send (type msg chan)
  "Send a message of the given type to the connection. Either :pong or :privmsg"
  (cond ((equal type :pong) (wsd:send *client* "PONG :tmi.twitch.tv"))
	((equal type :privmsg) (wsd:send *client* (format nil "PRIVMSG ~a :~A" chan msg)))
	(t :ok)))

(defun pong (msg)
  "Reacts to twitchs PING messages with a PONG."
  (alexandria:switch (msg :test #'string-equal)
    ("PING :tmi.twitch.tv" (progn
			       (format t "PONG")
			       (wsd:send *client* "PONG :tmi.twitch.tv")))))

(defun connect-join-channel (nick pass chan)
  "Starts the connection and initializes the login for the twitch IRC."
  (wsd:send *client* (format nil "PASS ~a" pass))
  (wsd:send *client* (format nil "NICK ~a" nick))
  (wsd:send *client* (format nil "JOIN ~a" chan))
  (wsd:send *client* (format nil "PRIVMSG ~a :~A" chan (third (get-file *path-to-conf*)))))

(defun run-from-repl (docker target-channel bot-name)
  "Start the bot in a REPL."
  (setf *docker* docker)
  (setf *target-channel* target-channel)
  (setf *bot-name* bot-name)
  (setf *client* (wsd:make-client (second (get-file *path-to-conf*))))
  (start-connection)
  (connect-join-channel *bot-name* *oauth* *target-channel*)) 

(defun run ()
  "Entry point, starts the bot."
  (print (uiop:command-line-arguments))
  (let ((cmds (uiop:command-line-arguments)))
    (setf *docker* (first cmds))
    (setf *target-channel* (second cmds))
    (setf *bot-name* (third cmds))
    (setf *client* (wsd:make-client (second (get-file *path-to-conf*)))))
  (start-connection)
  (connect-join-channel *bot-name* *oauth* *target-channel*))

