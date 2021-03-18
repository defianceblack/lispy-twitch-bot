;;;; lispbot.asd

(asdf:defsystem #:lispbot
  :description "Lispbot is a IRC bot for Twitch.tv that transforms your chat into a REPL."
  :author "Timo 'eXodiquas' Netzer <exodiquas@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on ("isolated" "websocket-driver-client" "cl-ppcre")
  :components ((:file "package")
               (:file "lispbot"))
  :build-operation "asdf:program-op"
  :build-pathname "lispbot"
  :entry-point "lispbot:run")
