(defpackage #:info.isoraqathedh.betza-lexer
  (:use #:cl #:plump-parser)
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

(in-package #:betza-lexer)
(define-matcher uppercase (in #\A #\Z))
(define-matcher number (in #\0 #\9))

(defclass power ()
  ((landmark :initarg :landmark :initform (error "LANDMARK required") :accessor landmark)
   (modifiers :initarg :modifiers :initform () :accessor modifiers)
   (limit :initarg :limit :initform NIL :accessor limit)))

(defmethod print-object ((power power) stream)
  (format stream "~a~:[~a~;[~{~a~}]~]~@[~d~]"
          (modifiers power) (listp (landmark power)) (landmark power) (limit power)))

(defclass piece ()
  ((powers :initarg :powers :initform () :accessor powers)))

(defmethod print-object ((piece piece) stream)
  (format stream "#?\"~{~a~}\"" (powers piece)))

(defun read-bracketed ()
  (loop for next = (peek)
        while (and next (char/= next #\]))
        collect (read-power)
        finally (consume)))

(defun read-number ()
  (when (funcall (make-matcher :number))
    (consume-until (make-matcher (not :number)))))

(defun read-power ()
  (let ((modifiers (consume-until (make-matcher (or :uppercase (any #\[ #\])))))
        (landmark (consume)))
    (unless landmark (error "Landmark expected."))
    (case landmark
      (#\[ (setf landmark (read-bracketed)))
      (#\])
      (T (if (and (peek) (char= landmark (peek)))
             (setf landmark (format NIL "~a~a" landmark (consume)))
             (setf landmark (string landmark)))))
    (make-instance
     'power
     :landmark landmark
     :modifiers modifiers
     :limit (read-number))))

(defun string->power (string)
  (with-lexer-environment (string)
    (read-power)))

(defun parse-piece (string)
  (with-lexer-environment (string)
    (loop while (peek)
          collect (read-power) into powers
          finally (return (make-instance 'piece :powers powers)))))

(defun %format (s a c p)
  (declare (ignore s c p))
  (print-as-lists a))

(defgeneric print-as-lists (obj)
  (:method ((piece piece))
    (format T "(:piece ~{~/betza-lexer::%format/~^ ~})" (powers piece)))
  (:method ((power power))
    (format T "(:power ")
    (print-as-lists (landmark power))
    (format T " ~s~@[ ~d~])" (modifiers power) (limit power)))
  (:method ((list list))
    (format T "(~{~/betza-lexer::%format/~^ ~})" list))
  (:method (object)
    (format T "~s" object)))

(set-dispatch-macro-character #\# #\? #'(lambda (s c a)
                                          (declare (ignore c a))
                                          (let ((string (read s)))
                                            (etypecase string
                                              (string `(parse-piece ,string))))))
