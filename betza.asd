(defpackage #:info.isoraqathedh.betza-asdf
  (:use #:cl #:asdf))
(in-package #:info.isoraqathedh.betza-asdf)

(defsystem betza
  :name "Betza → Movement Diagram"
  ;:version "B.4.0.00.1"
  :licence "MIT"
  :description "Simple converter of Betza Funny notation into movement diagrams (plus a few more things in passing)"
  :serial T
  :components ((:file "parser")
               (:file "board"))
  :depends-on (:plump))
