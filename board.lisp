(defpackage :info.isoraqathedh.betza.board
  (:use #:cl #:betza-lexer))

(in-package :info.isoraqathedh.betza.board)

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

(defmethod print-object ((object destination) stream)
  (print-unreadable-object (object stream :type t)
    (with-accessors ((x destination-x) (y destination-y)) object
      (format stream "(~a, ~a)" x y))))

(defparameter *primitives*
  (loop with ht = (make-hash-table)
        for (landmark . coords) in '((#\W 1 0) (#\F 1 1)
                                     (#\D 2 0) (#\N 2 1) (#\A 2 2)
                                     (#\H 3 0) (#\L 3 1) (#\J 3 2) (#\G 3 3))
        do (setf (gethash landmark ht) coords)
        finally (return ht))
  "Where each of the primitives point to, for instance \"W\" points to (1, 0).")

(defun riderp (power)
  "Detects if a move has a range modifier. Returns T/NIL as well as length"
  (when (or (limit power)
            (find (landmark power) '("R" "B" "Q") :test #'string=)
            (= (length (landmark power)) 2))
    (if (limit power) (parse-integer (limit power)) :infinite)))

(defun make-destination-object (x y signature)
  "Helper function: makes a DESTINATION"
  (make-instance 'destination :x x :y y :signature signature))

(defun make-destinations (betza)
  "Turns a funny notation item into a list of destinations."
  ;; Consider also turning things into target-squares here.
  (loop for power in (powers betza)
        for riderp = (riderp power)
        for landmark = (aref (landmark power) 0)
        append (cond ((find landmark "QK")
                      (list (make-destination-object 1 0 (if riderp (list :range riderp)))
                            (make-destination-object 1 1 (if riderp (list :range riderp)))))
                     ((eql landmark #\R) (list (make-destination-object 1 0 (list :range riderp))))
                     ((eql landmark #\B) (list (make-destination-object 1 1 (list :range riderp))))
                     (t (list (make-destination-object (first (gethash landmark *primitives*))
                                                       (second (gethash landmark *primitives*))
                                                       (if riderp (list :range riderp))))))))

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
