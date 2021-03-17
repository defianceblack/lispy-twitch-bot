;;;; lispbot.asd

(asdf:defsystem #:lispbot
  :description "Describe lispbot here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("isolated" "websocket-driver-client" "cl-ppcre")
  :components ((:file "package")
               (:file "lispbot"))
  :build-operation "asdf:program-op"
  :build-pathname "lispbot"
  :entry-point "lispbot")
