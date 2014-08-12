(defpackage :info.isoraqathedh.betza.board
  (:use #:cl #:betza-lexer))

(in-package :info.isoraqathedh.betza.board)

;;; Globals

(defvar *location-descriptors*
  (loop with hash-table = (make-hash-table)
        for (coords . names)
          in '(((0 . 0) :zero)
               ((1 . 0) :wazir :w)
               ((1 . 1) :ferz :advisor :f)
               ((2 . 0) :dabbabah :war-machine :d)
               ((2 . 1) :knight :horse :n)
               ((2 . 2) :alfil :elephant :a)
               ((3 . 0) :threeleaper :g)
               ((3 . 1) :camel :j)
               ((3 . 2) :zebra :l)
               ((3 . 3) :tripper :h)
               ((4 . 0) :fourleaper)
               ((4 . 1) :giraffe)
               ((4 . 2) :double-knight)
               ((4 . 3) :antelope))
        do (loop for i in names do (setf (gethash i hash-table) coords))
        finally (return hash-table))
  "The exact location where some names belong to (a displacement of (2, 1) is a knight-distance away.")

;;; Classes:
;;; destination class
;;; See class documentation for usage.

(defclass destination ()
  ((x :accessor destination-x
      :initarg :x
      :initform 0
      :documentation "x-coordinate (left-right) of the destination. Right is positive.")
   (y :accessor destination-y
      :initarg :y
      :initform 0
      :documentation "y-coordinate (up-down) of the destination. Up is positive.")
   (signature :accessor signature
              :initarg :signature
              :initform nil
              :documentation "list of objects that represents what the piece can do at that square."))
  (:documentation "A representation of what a piece can do at a square."))

(defun riderp (power)
  "Detects if a move has a range modifier. Returns T/NIL as well as length"
  (when (or (find (landmark power) '("R" "B" "Q") :test #'string=)
            (= (length (landmark power)) 2))
    (if (limit power) (parse-integer (limit power)) :infinite)))

(defun capturingp (modifiers)
  "Detects if a piece can capture or not"
  (or (find #\c modifiers)
      (not (or (find #\m modifiers)
               (find #\c modifiers)))))

(defun movingp (modifiers)
  "Detects if a piece can move or not"
  (or (find #\m modifiers)
      (not (or find #\m modifiers)
           (or find #\c modifiers))))
