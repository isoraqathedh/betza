;;; package.lisp

;;; also handles readtable definition
(defpackage #:info.isoraqathedh.betza
  (:use #:cl)
  (:export #:betza-readtable))

(in-package #:info.isoraqathedh.betza)
(named-readtables:defreadtable betza-readtable
  (:merge nil))

(defpackage #:info.isoraqathedh.betza-lexer
  (:use #:cl #:plump #:named-readtables)
  (:nicknames :betza-lexer)
  (:export
   #:power
   #:landmark
   #:modifiers
   #:limit
   #:piece
   #:powers
   #:parse-piece
   #:string->power))

(defpackage #:info.isoraqathedh.betza.generators
  (:use :cl)
  (:nicknames :betza-generators)
  (:export
   #:with-parsed-square
   #:return-once
   #:line
   #:interleave
   #:interleave*
   #:collect-repeatedly))

(defpackage #:info.isoraqathedh.betza.board
  (:nicknames #:betza-board)
  (:use :cl))
