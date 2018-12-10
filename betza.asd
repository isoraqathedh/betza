(in-package #:cl-user)

(asdf:defsystem #:betza
  :name "Betza → Movement Diagram"
  ;:version "B.4.0.00.1"
  :licence "MIT"
  :description "Simple converter of Betza Funny notation into movement diagrams (plus a few more things in passing)"
  :serial T
  :components ((:file "parser")
               ;;(:file "destination")
               (:file "move-diagram"))
  :depends-on (:plump :iterate))
